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

/**
  * @class EmoticonTextDocument
  * This class mainly exist because the QTextDocument::setMarkup clean all resources, I think this is a bug.
  */

#include <Chat/EmoticonTextDocument.h>
#include <QRegularExpression>
#include <QTextBlock>
#include <QTextCursor>
#include <QSet>
using namespace GUI;

namespace
{
   QString copyHtmlBreaksAsNewlines(const QString& markdown)
   {
      static const QRegularExpression breakExpression("<br\\s*/?>", QRegularExpression::CaseInsensitiveOption);
      QString prefix = "DLANCOPYBREAK";
      while (markdown.contains(prefix))
         prefix += 'X';
      QList<QPair<QRegularExpressionMatch, QString>> breaks;
      auto matches = breakExpression.globalMatch(markdown);
      while (matches.hasNext())
         breaks.append({ matches.next(), prefix + QString::number(breaks.size()) + "END" });
      if (breaks.isEmpty())
         return markdown;

      QString marked = markdown;
      for (auto i = breaks.crbegin(); i != breaks.crend(); ++i)
         // Qt's Markdown importer only reliably handles this spelling of the tag.
         marked.replace(i->first.capturedStart(), i->first.capturedLength(), "<br/>" + i->second);
      QTextDocument document;
      document.setMarkdown(marked);
      QString result = markdown;
      for (auto i = breaks.crbegin(); i != breaks.crend(); ++i)
      {
         // A rendered <br> leaves a line separator before its marker. In code or
         // escaped text, it leaves a literal '>' instead; retain that source text.
         const auto marker = document.find(i->second, 0, QTextDocument::FindCaseSensitively);
         if (!marker.isNull() && marker.selectionStart() > 0 &&
             document.characterAt(marker.selectionStart() - 1) == QChar::LineSeparator)
            result.replace(i->first.capturedStart(), i->first.capturedLength(), "\n");
      }
      return result;
   }

   QString unwrapSoftBreaks(const QString& markdown)
   {
      if (!markdown.contains('\n'))
         return markdown;
      // Do not repeatedly parse every line of a code block, list or table while
      // proving that its line breaks are structural. Keep those source lines intact.
      QSet<qsizetype> structuralBreaks;
      static const QRegularExpression fenceExpression(R"(^ {0,3}(`{3,}|~{3,})(.*)$)");
      static const QRegularExpression structuralLine(R"(^(?: {4}|\t| {0,3}(?:>|\||#{1,6}(?:\s|$)|(?:[-+*]|\d+[.)])\s)))");
      QString fence;
      qsizetype lineStart = 0;
      for (const auto line : QStringView(markdown).split('\n'))
      {
         const auto match = fenceExpression.matchView(line);
         if (!fence.isEmpty() || match.hasMatch() || structuralLine.matchView(line).hasMatch())
         {
            structuralBreaks.insert(lineStart - 1);
            structuralBreaks.insert(lineStart + line.size());
         }
         if (match.hasMatch())
         {
            const QString delimiter = match.captured(1);
            if (fence.isEmpty())
               fence = delimiter;
            else if (delimiter.front() == fence.front() && delimiter.size() >= fence.size() && match.captured(2).trimmed().isEmpty())
               fence.clear();
         }
         lineStart += line.size() + 1;
      }
      struct Wrap { qsizetype position; qsizetype length; bool removed = false; };
      QList<Wrap> wraps;
      static const QRegularExpression newlineExpression("[ \\t]*\\r?\\n[ \\t]*");
      auto matches = newlineExpression.globalMatch(markdown);
      while (matches.hasNext())
      {
         const auto match = matches.next();
         // Paragraph boundaries and explicit Markdown hard breaks stay in the source.
         if (structuralBreaks.contains(match.capturedStart() + match.captured().indexOf('\n')) ||
             match.capturedStart() == 0 || match.capturedEnd() == markdown.size() ||
             markdown.at(match.capturedStart() - 1) == '\n' || markdown.at(match.capturedEnd()) == '\n' ||
             markdown.at(match.capturedStart() - 1) == '\\' || match.captured().startsWith("  "))
            continue;
         wraps.append({ match.capturedStart(), match.capturedLength() });
      }
      if (wraps.isEmpty())
         return markdown;

      QTextDocument original;
      original.setMarkdown(markdown);
      const QString rendered = original.toHtml();
      QString result = markdown;
      // Usually all candidates are wrapping within paragraphs, requiring one parse.
      // If joining changes the rendering, subdivide to retain structural newlines
      // (headings, lists, code, tables, HTML) while still unwrapping ordinary text.
      const auto unwrap = [&](auto&& self, qsizetype begin, qsizetype end) -> void {
         QString candidate;
         candidate.reserve(markdown.size());
         qsizetype position = 0;
         for (qsizetype i = 0; i < wraps.size(); ++i)
            if (wraps[i].removed || (i >= begin && i < end))
            {
               candidate += QStringView(markdown).mid(position, wraps[i].position - position);
               candidate += ' ';
               position = wraps[i].position + wraps[i].length;
            }
         candidate += QStringView(markdown).mid(position);
         QTextDocument parsed;
         parsed.setMarkdown(candidate);
         if (parsed.toHtml() == rendered)
         {
            for (qsizetype i = begin; i < end; ++i)
               wraps[i].removed = true;
            result = candidate;
         }
         else if (end - begin > 1)
         {
            const qsizetype middle = begin + (end - begin) / 2;
            self(self, begin, middle);
            self(self, middle, end);
         }
      };
      unwrap(unwrap, 0, wraps.size());
      return result;
   }

   QString copyInlineFormattingAsMarkdown(const QString& markdown)
   {
      // These wrappers are emitted by the sender to work around Qt's emphasis writer.
      // Keep whitespace outside emphasis delimiters when making them readable again.
      // Markdown has no underline delimiter, so omit that style from clipboard text.
      static const QRegularExpression spanExpression(
         R"(<(?<wrapper>span|u) style="white-space: pre-wrap">(?<strike><s>)?(?<bold><b>)?(?<italic><i>)?(?<body>(?:[^<]|<br/?>)*)(?(italic)</i>)(?(bold)</b>)(?(strike)</s>)</\k<wrapper>>)");
      QString prefix = "DLANCOPYFORMAT";
      while (markdown.contains(prefix))
         prefix += 'X';
      struct Replacement
      {
         qsizetype position;
         qsizetype length;
         QString marker;
         QString maskedSpan;
         QString text;
         QString emphasis;
      };
      QList<Replacement> replacements;
      auto matches = spanExpression.globalMatch(markdown);
      while (matches.hasNext())
      {
         const auto match = matches.next();
         const QString body = match.captured("body");
         qsizetype begin = 0;
         qsizetype end = body.size();
         while (begin < end && body.at(begin).isSpace())
            ++begin;
         while (end > begin && body.at(end - 1).isSpace())
            --end;
         QString text = body.mid(begin, end - begin);
         const QString emphasis = QString(match.hasCaptured("bold") ? "**" : "") + (match.hasCaptured("italic") ? "*" : "");
         if (!text.isEmpty())
         {
            text = emphasis + text + emphasis;
            if (match.hasCaptured("strike"))
               text = "~~" + text + "~~";
         }
         text = body.left(begin) + text + body.mid(end);
         const QString marker = prefix + QString::number(replacements.size()) + "END";
         QString maskedSpan = match.captured();
         maskedSpan.replace(match.capturedStart("body") - match.capturedStart(), body.size(), marker);
         replacements.append({ match.capturedStart(), match.capturedLength(), marker, maskedSpan, text, emphasis });
      }
      if (replacements.isEmpty())
         return markdown;

      QString masked = markdown;
      for (auto i = replacements.crbegin(); i != replacements.crend(); ++i)
         masked.replace(i->position, i->length, i->maskedSpan);
      QTextDocument document;
      document.setMarkdown(masked);
      const QString rendered = document.toPlainText();
      QString result = markdown;
      for (auto i = replacements.crbegin(); i != replacements.crend(); ++i)
         // Code spans/blocks display the tags literally and must keep their source.
         if (rendered.contains(i->marker) && !rendered.contains(i->maskedSpan))
         {
            qsizetype begin = i->position;
            qsizetype end = begin + i->length;
            QString text = i->text;
            // Removing an underline can join two bold/italic runs. Merge touching
            // delimiters instead of producing literal stars ("**one****two**").
            const QString& emphasis = i->emphasis;
            if (!emphasis.isEmpty())
            {
               const auto marker = document.find(i->marker, 0, QTextDocument::FindCaseSensitively);
               const auto sameEmphasis = [&](int position) {
                  if (position < 0 || position >= document.characterCount() - 1 || document.characterAt(position).isSpace())
                     return false;
                  QTextCursor cursor(&document);
                  cursor.setPosition(position);
                  cursor.movePosition(QTextCursor::NextCharacter, QTextCursor::KeepAnchor);
                  const auto format = cursor.charFormat();
                  return (format.fontWeight() >= QFont::Bold) == (emphasis.size() >= 2) &&
                     format.fontItalic() == (emphasis.size() % 2 != 0) && !format.fontStrikeOut();
               };
               const qsizetype size = emphasis.size();
               if (text.startsWith(emphasis) && begin >= size && result.mid(begin - size, size) == emphasis &&
                   (begin == size || result.at(begin - size - 1) != '*') && sameEmphasis(marker.selectionStart() - 1))
               {
                  begin -= size;
                  text.remove(0, size);
               }
               if (text.endsWith(emphasis) && result.mid(end, size) == emphasis &&
                   (end + size == result.size() || result.at(end + size) != '*') && sameEmphasis(marker.selectionEnd()))
               {
                  end += size;
                  text.chop(size);
               }
            }
            result.replace(begin, end - begin, text);
         }
      return result;
   }

   struct EmoticonLink
   {
      qsizetype position;
      qsizetype length;
      QString marker;
      QString altMarkdown;
      QString symbol;
      QUrl url;
   };

   QList<EmoticonLink> protectEmoticonAltText(QString& markdown)
   {
      // Qt's importer creates a separate image for each text segment in an alt
      // description (including backslash escapes). Give it one plain segment.
      QString prefix = "DLANEMOTICON";
      while (markdown.contains(prefix))
         prefix += 'X';
      static const QRegularExpression imageExpression(
         R"((?<!\\)!\[((?:\\.|[^\]\\])*)\]\((emoticons://(?:\\.|[^\s()\\])*)\))");
      QList<EmoticonLink> links;
      auto matches = imageExpression.globalMatch(markdown);
      while (matches.hasNext())
      {
         const auto match = matches.next();
         QTextDocument alt;
         alt.setMarkdown(match.captured(1));
         links.append({ match.capturedStart(), match.capturedLength(), prefix + QString::number(links.size()),
            match.captured(1), alt.toPlainText(), QUrl(match.captured(2)) });
      }
      for (auto i = links.crbegin(); i != links.crend(); ++i)
         markdown.replace(i->position + 2, i->altMarkdown.size(), i->marker);
      return links;
   }
}

EmoticonTextDocument::EmoticonTextDocument(const Emoticons& emoticons, QObject* parent) :
   QTextDocument(parent),
   emoticons(emoticons)
{
}

void EmoticonTextDocument::setMarkdown(const QString& markdown)
{
   QString prepared = markdown;
   const auto links = protectEmoticonAltText(prepared);
   QTextDocument::setMarkdown(prepared);
   if (links.isEmpty())
      return;
   QHash<QString, QString> symbols;
   for (const auto& link : links)
      symbols.insert(link.marker, link.symbol);
   QList<QTextFragment> images;
   for (auto block = this->begin(); block.isValid(); block = block.next())
      for (auto it = block.begin(); !it.atEnd(); ++it)
         if (it.fragment().charFormat().isImageFormat())
            images.append(it.fragment());
   for (const auto& image : images)
   {
      const auto symbol = symbols.constFind(image.charFormat().stringProperty(QTextFormat::ImageAltText));
      if (symbol == symbols.constEnd())
         continue;
      QTextCursor cursor(this);
      cursor.setPosition(image.position());
      cursor.setPosition(image.position() + image.length(), QTextCursor::KeepAnchor);
      QTextCharFormat format;
      format.setProperty(QTextFormat::ImageAltText, *symbol);
      cursor.mergeCharFormat(format);
   }
   // Links inside code or escaped Markdown are literal text, not images.
   for (auto i = links.crbegin(); i != links.crend(); ++i)
   {
      auto cursor = this->find(i->marker, 0, QTextDocument::FindCaseSensitively);
      if (!cursor.isNull())
         cursor.insertText(i->altMarkdown);
   }
}

QString EmoticonTextDocument::toClipboardMarkdown(const QString& markdown, const Emoticons& emoticons)
{
   const QString unwrapped = unwrapSoftBreaks(markdown);
   QString prepared = unwrapped;
   const auto links = protectEmoticonAltText(prepared);
   if (links.isEmpty())
      return copyHtmlBreaksAsNewlines(copyInlineFormattingAsMarkdown(unwrapped));
   QTextDocument document;
   document.setMarkdown(prepared);
   QSet<QString> imageMarkers;
   for (auto block = document.begin(); block.isValid(); block = block.next())
      for (auto it = block.begin(); !it.atEnd(); ++it)
         if (it.fragment().charFormat().isImageFormat())
            imageMarkers.insert(it.fragment().charFormat().stringProperty(QTextFormat::ImageAltText));

   QString result = unwrapped;
   for (auto i = links.crbegin(); i != links.crend(); ++i)
   {
      if (!imageMarkers.contains(i->marker))
         continue;
      QString symbol = i->symbol;
      // Old messages lack the original alias; use the theme's first symbol.
      if (symbol.isEmpty() || symbol == "image")
         symbol = emoticons.getSmileSymbols(i->url.host(), i->url.path().mid(1)).value(0);
      if (!symbol.isEmpty())
         result.replace(i->position, i->length, symbol);
   }
   return copyHtmlBreaksAsNewlines(copyInlineFormattingAsMarkdown(result));
}

QVariant EmoticonTextDocument::loadResource(int type, const QUrl& name)
{
   if (type == QTextDocument::ImageResource && name.scheme() == "emoticons")
      return this->emoticons.getSmileImage(name.host(), name.path().mid(1)); // Skip the '/' at the beginning.
   return QTextDocument::loadResource(type, name);
}
