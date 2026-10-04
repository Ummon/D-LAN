/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <Common/StringUtils.h>
using namespace Common;

#include <cctype>
#include <utility>

#include <QRegularExpression>
#include <QTextBoundaryFinder>
#include <QVarLengthArray>

namespace
{
   const char16_t DEVANAGARI_CANDRABINDU = 0x0901;
   const char16_t DEVANAGARI_ANUSVARA = 0x0902;
   const char16_t DEVANAGARI_NUKTA = 0x093C;
   const char16_t DEVANAGARI_VIRAMA = 0x094D;
   const char16_t ZERO_WIDTH_NON_JOINER = 0x200C;
   const char16_t ZERO_WIDTH_JOINER = 0x200D;

   /**
     * Whether the combining marks following a base character of the given script are optional for searching,
     * like Latin accents or Hebrew and Arabic vowel points. Other scripts keep their marks because they
     * distinguish words, e.g. the Devanagari vowel signs of काम and कम, or the kana dakuten.
     * 'Common' covers marks without a base letter, e.g. at the beginning of the text.
     */
   bool hasOptionalMarks(QChar::Script script)
   {
      switch (script)
      {
      case QChar::Script_Common:
      case QChar::Script_Inherited:
      case QChar::Script_Latin:
      case QChar::Script_Greek:
      case QChar::Script_Cyrillic:
      case QChar::Script_Armenian:
      case QChar::Script_Hebrew:
      case QChar::Script_Arabic:
      case QChar::Script_Hangul: // Only the archaic tone marks.
         return true;
      default:
         return false;
      }
   }

   bool isVariationSelector(char32_t c)
   {
      return (c >= 0xFE00 && c <= 0xFE0F) || (c >= 0xE0100 && c <= 0xE01EF);
   }

   /**
     * Whether 'nasal' is the nasal consonant of the class of 'consonant', e.g. न for द.
     * The five classes (velar, palatal, retroflex, dental and labial) have four stops followed by their nasal.
     */
   bool isHomorganicNasal(QChar nasal, QChar consonant)
   {
      for (const char16_t classStart : { u'क', u'च', u'ट', u'त', u'प' })
         if (nasal.unicode() == classStart + 4)
            return consonant.unicode() >= classStart && consonant.unicode() < classStart + 4;
      return false;
   }

   /**
     * Replace each nasal consonant followed by a virama and a consonant of its class by an anusvara,
     * both spellings are standard: e.g. हिन्दी => हिंदी.
     * If given, 'positions' must have one entry per character of 'str' and is updated accordingly.
     */
   void foldDevanagariNasals(QString& str, QList<int>* positions = nullptr)
   {
      if (!str.contains(QChar(DEVANAGARI_VIRAMA)))
         return;

      qsizetype j = 0;
      for (qsizetype i = 0; i < str.size(); ++i, ++j)
      {
         if (positions)
            (*positions)[j] = (*positions)[i];

         if (i + 2 < str.size() && str.at(i + 1) == QChar(DEVANAGARI_VIRAMA) && isHomorganicNasal(str.at(i), str.at(i + 2)))
         {
            str[j] = QChar(DEVANAGARI_ANUSVARA);
            ++i; // Skip the virama.
         }
         else
            str[j] = str.at(i);
      }
      str.truncate(j);
      if (positions)
         positions->resize(j);
   }
}

/**
  * Fold a text for searching:
  *  - Lower case and compatibility decomposition (e.g. fullwidth 'Ａ' => 'a').
  *  - Remove the optional marks, see 'hasOptionalMarks(..)', and the variation selectors.
  *  - Remove the zero-width joiners, they only change the rendering of a word.
  *  - Replace the non-ASCII decimal digits by their ASCII equivalent, e.g. '२०२४' => '2024'.
  *  - Devanagari: remove the nukta, replace the candrabindu and the nasal conjuncts by an anusvara.
  */
QString StringUtils::toLowerAndRemoveAccents(const QString& str)
{
   const QString decomposed = str.toLower().normalized(QString::NormalizationForm_KD);
   QString result;
   result.reserve(decomposed.size());
   QChar::Script baseScript = QChar::Script_Common; // Script of the last non-mark character.

   // Decode supplementary characters in place, without allocating a UTF-32 copy.
   for (qsizetype i = 0; i < decomposed.size(); ++i)
   {
      char32_t c = decomposed.at(i).unicode();
      if (decomposed.at(i).isHighSurrogate() && i + 1 < decomposed.size() && decomposed.at(i + 1).isLowSurrogate())
      {
         c = QChar::surrogateToUcs4(decomposed.at(i), decomposed.at(i + 1));
         ++i;
      }

      const QChar::Category category = QChar::category(c);
      if (category == QChar::Mark_NonSpacing || category == QChar::Mark_SpacingCombining)
      {
         if (c == DEVANAGARI_NUKTA || isVariationSelector(c) || hasOptionalMarks(baseScript))
            continue;
         if (c == DEVANAGARI_CANDRABINDU)
            c = DEVANAGARI_ANUSVARA;
      }
      else if (c == ZERO_WIDTH_NON_JOINER || c == ZERO_WIDTH_JOINER)
         continue;
      else
      {
         baseScript = QChar::script(c);
         if (category == QChar::Number_DecimalDigit)
            c = u'0' + QChar::digitValue(c);
      }
      result.append(QChar::fromUcs4(c));
   }

   // Restore Hangul syllables and composed kana after compatibility decomposition.
   QString composed = result.normalized(QString::NormalizationForm_C);
   foldDevanagariNasals(composed);
   return composed;
}

/**
  * Fold text and map each resulting UTF-16 unit to its source grapheme's start.
  * Append the source length as a sentinel so match ends can also be mapped.
  */
QString StringUtils::toLowerAndRemoveAccents(const QString& str, QList<int>& positions)
{
   QString folded;
   folded.reserve(str.size());
   positions.clear();
   positions.reserve(str.size() + 1);

   QTextBoundaryFinder boundaries(QTextBoundaryFinder::Grapheme, str);
   for (int start = 0, end; (end = boundaries.toNextBoundary()) != -1; start = end)
   {
      if (end == start + 1 && str.at(start).unicode() < 0x80)
      {
         folded += str.at(start).toLower();
         positions << start;
         continue;
      }
      const QString part = StringUtils::toLowerAndRemoveAccents(str.mid(start, end - start));
      folded += part;
      for (int i = 0; i < part.size(); ++i)
         positions << start;
   }

   // Compatibility decomposition may join formerly separate graphemes: e.g. ㄱ + ㅏ
   // becomes conjoining Jamo, which must compose to 가 just as in whole-string folding.
   QString composed = folded.normalized(QString::NormalizationForm_C);
   if (composed != folded)
   {
      QList<int> composedPositions;
      composedPositions.reserve(composed.size() + 1);
      QTextBoundaryFinder foldedBoundaries(QTextBoundaryFinder::Grapheme, folded);
      for (int start = 0, end; (end = foldedBoundaries.toNextBoundary()) != -1; start = end)
      {
         const QString part = folded.mid(start, end - start);
         const QString composedPart = part.normalized(QString::NormalizationForm_C);
         for (int i = 0; i < composedPart.size(); ++i)
            composedPositions << positions[part == composedPart ? start + i : start];
      }
      positions = std::move(composedPositions);
   }

   // A nasal conjunct may also span two graphemes, e.g. न् + द without Indic conjunct grapheme clusters.
   foldDevanagariNasals(composed, &positions);

   positions << str.size();
   return composed;
}

/**
  * Take raw terms in a string and split, trim and filter to
  * return a list of lower case keywords without accents.
  * Some character or word can be removed.
  * The preserved combining marks, like the Devanagari vowel signs, are part of the words.
  * @example " The little  DUCK " => ["the", "little", "duck"].
  */
QStringList StringUtils::splitInWords(const QString& words)
{
   static const QRegularExpression regExp("[^\\p{L}\\p{Mn}\\p{Mc}\\p{N}]+");
   return StringUtils::toLowerAndRemoveAccents(words).split(regExp, Qt::SkipEmptyParts);
}

/**
  * Like 'splitInWords(..)' plus the sub-words of each word, see 'subWordBoundaries(..)'.
  * The words are kept, their sub-words are only another way to find them.
  * It's used to index a name: the searched terms aren't split in sub-words.
  * @example "superGirl.avi" => ["supergirl", "avi", "super", "girl"].
  */
QStringList StringUtils::splitInWordsAndSubWords(const QString& words)
{
   QStringList result = StringUtils::splitInWords(words);

   const QList<int> boundaries = StringUtils::subWordBoundaries(words);
   if (boundaries.isEmpty())
      return result;

   // A boundary always precedes a letter, never a mark: separating the sub-words doesn't change the way they are folded.
   QString separated;
   separated.reserve(words.size() + boundaries.size());
   int previous = 0;
   for (const int boundary : boundaries)
   {
      separated.append(QStringView(words).sliced(previous, boundary - previous)).append(u' ');
      previous = boundary;
   }
   separated.append(QStringView(words).sliced(previous));

   for (const QString& subWord : StringUtils::splitInWords(separated))
      if (!result.contains(subWord))
         result << subWord;

   return result;
}

/**
  * Return the positions in 'str', in ascending order, where a sub-word begins inside a word:
  *  - Change of case, for the scripts having one (Latin, Cyrillic, Greek, ...): "superGirl" => "super|Girl".
  *    The last letter of an upper case run begins a sub-word if it's followed by at least two lower case letters:
  *    "HTTPServer" => "HTTP|Server", a plural like "PDFs" isn't split.
  *  - Change of script: "Naruto第3話" => "Naruto|第3話", "東京タワー" => "東京|タワー".
  *    The digits and the letters shared by several scripts, like 'ー', belong to the current sub-word.
  */
QList<int> StringUtils::subWordBoundaries(const QString& str)
{
   enum class Kind { SEPARATOR, OTHER, LOWER, UPPER };
   struct Character
   {
      int position;
      Kind kind;
      QChar::Script script; // 'Script_Unknown' if the character doesn't identify a script.
   };

   // The marks and the joiners belong to the previous character.
   QVarLengthArray<Character, 64> characters;
   for (qsizetype i = 0; i < str.size(); ++i)
   {
      Character character { static_cast<int>(i), Kind::SEPARATOR, QChar::Script_Unknown };

      char32_t c = str.at(i).unicode();
      if (str.at(i).isHighSurrogate() && i + 1 < str.size() && str.at(i + 1).isLowSurrogate())
      {
         c = QChar::surrogateToUcs4(str.at(i), str.at(i + 1));
         ++i;
      }

      if (QChar::isMark(c) || c == ZERO_WIDTH_NON_JOINER || c == ZERO_WIDTH_JOINER)
         continue;

      if (QChar::isLetter(c))
      {
         character.kind = QChar::isLower(c) ? Kind::LOWER : QChar::isUpper(c) || QChar::isTitleCase(c) ? Kind::UPPER : Kind::OTHER;
         const QChar::Script script = QChar::script(c);
         if (script != QChar::Script_Common && script != QChar::Script_Inherited)
            character.script = script;
      }
      else if (QChar::isNumber(c))
         character.kind = Kind::OTHER;

      characters.append(character);
   }

   QList<int> boundaries;
   QChar::Script script = QChar::Script_Unknown; // The script of the current sub-word.
   for (qsizetype i = 0; i < characters.size(); ++i)
   {
      const Character& current = characters[i];
      if (current.kind == Kind::SEPARATOR)
      {
         script = QChar::Script_Unknown;
         continue;
      }

      bool boundary = false;
      if (current.script != QChar::Script_Unknown)
      {
         boundary = script != QChar::Script_Unknown && current.script != script;
         script = current.script;
      }

      if (!boundary && current.kind == Kind::UPPER && i > 0)
      {
         const Kind previous = characters[i - 1].kind;
         boundary = previous == Kind::LOWER ||
            (previous == Kind::UPPER && i + 2 < characters.size() && characters[i + 1].kind == Kind::LOWER && characters[i + 2].kind == Kind::LOWER);
      }

      if (boundary)
         boundaries << current.position;
   }

   return boundaries;
}

/**
 * Take a string (like a command line) and split it in trimmed arguments.
 * Arguments can be quoted, for instance :
 *    abc "def ghi" => ["abc", "def ghi"]
 */
QStringList StringUtils::splitArguments(const QString& str)
{
   QStringList args;
   QString currentArg;
   bool inQuotes = false;
   bool quoted = false; // 'true' if the current argument has a quoted part, it's kept even if empty.

   for (int i = 0; i < str.length(); i++)
   {
      const QChar c = str[i];

      if (c == '"')
      {
         inQuotes = !inQuotes;
         quoted = true;
      }
      else if (c.isSpace() && !inQuotes)
      {
         if (quoted || !currentArg.isEmpty())
            args << currentArg;
         currentArg.clear();
         quoted = false;
      }
      else
      {
         currentArg.append(c);
      }
   }

   if (quoted || !currentArg.isEmpty())
      args << currentArg;

   return args;
}

/**
  * Return whether the string contains at least one character in the Hangul script.
  */
bool StringUtils::isKorean(const QString& str)
{
   for (const QChar c : str)
      if (c.script() == QChar::Script_Hangul)
         return true;
   return false;
}

/**
  * Return whether the string contains Hiragana or Katakana. Han characters alone
  * do not identify Japanese because they are shared with other languages.
  */
bool StringUtils::isJapanese(const QString& str)
{
   // Decode supplementary kana in place, without allocating a UTF-32 copy.
   for (qsizetype i = 0; i < str.size(); ++i)
   {
      const QChar first = str.at(i);
      char32_t c = first.unicode();
      if (first.isHighSurrogate() && i + 1 < str.size() && str.at(i + 1).isLowSurrogate())
      {
         c = QChar::surrogateToUcs4(first, str.at(i + 1));
         ++i;
      }
      else if (first.isSurrogate())
         continue; // Unpaired surrogates cannot identify Japanese.

      const QChar::Script script = QChar::script(c);
      if (script == QChar::Script_Hiragana || script == QChar::Script_Katakana)
         return true;
   }
   return false;
}

/**
  * Return whether the string contains at least one character in the Devanagari script (Hindi, Marathi, Nepali, ...).
  */
bool StringUtils::isDevanagari(const QString& str)
{
   for (const QChar c : str)
      if (c.script() == QChar::Script_Devanagari)
         return true;
   return false;
}

/**
  * Compare two std::string without case sensitive.
  * @return 0 if equal, 1 if s1 > s2, -1 if s1 < s2.
  */
int StringUtils::strcmpi(const std::string& s1, const std::string& s2)
{
   for (unsigned int i = 0; i < s1.length() && i < s2.length(); i++)
   {
      // Cast to 'unsigned char': giving a negative value to 'tolower' is undefined behaviour.
      const int c1 = std::tolower(static_cast<unsigned char>(s1[i]));
      const int c2 = std::tolower(static_cast<unsigned char>(s2[i]));
      if (c1 > c2) return 1;
      else if (c1 < c2) return -1;
   }
   if (s1.length() > s2.length())
      return 1;
   else if (s1.length() < s2.length())
      return -1;
   return 0;
}

/**
  * If more speedup is needed, it may be replaced by the FNV hash function: http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
  */
quint32 StringUtils::hashStringToInt(const QString& str)
{
   const QByteArray data = str.toUtf8();
   const QByteArrayView view = QByteArrayView(data);
   if (data.size() <= 1)
      return qChecksum(view);

   auto s = data.length();

   const quint32 part1 = qChecksum(view.sliced(0, s / 2));
   const quint32 part2 = qChecksum(view.sliced(s / 2, s / 2 + (s % 2 == 0 ? 0 : 1)));
   return part1 | part2 << 16;
}

#ifdef Q_OS_WIN32
QList<wchar_t> StringUtils::towcharList(const QString& str)
{
   QList<wchar_t> str_wchar(str.size() + 1);
   str.toWCharArray(str_wchar.data());
   str_wchar[str_wchar.size() - 1] = 0;
   return str_wchar;
}
#endif
