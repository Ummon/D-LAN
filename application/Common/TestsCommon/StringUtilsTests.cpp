#include <QTest>

#include <Common/StringUtils.h>

using Common::StringUtils;

class StringUtilsTests : public QObject
{
   Q_OBJECT

private slots:
   void isJapanese_data()
   {
      QTest::addColumn<QString>("text");
      QTest::addColumn<bool>("expected");

      const QString high(QChar(0xd82c));
      const QString low(QChar(0xdc01));
      const QString kana = QStringLiteral("\U0001B001");
      QTest::newRow("null") << QString() << false;
      QTest::newRow("ASCII") << "abc" << false;
      QTest::newRow("hiragana") << QStringLiteral("\u3042") << true;
      QTest::newRow("katakana") << QStringLiteral("\u30a2") << true;
      QTest::newRow("halfwidth") << QStringLiteral("\uff71") << true;
      QTest::newRow("han") << QStringLiteral("\u65e5\u672c") << false;
      QTest::newRow("shared mark") << QStringLiteral("\u30fc") << false;
      QTest::newRow("supplementary katakana") << QStringLiteral("\U0001B000") << true;
      QTest::newRow("supplementary hiragana") << kana << true;
      QTest::newRow("supplementary at start") << QString(kana + "abc") << true;
      QTest::newRow("supplementary in middle") << QString("abc" + kana + "def") << true;
      QTest::newRow("supplementary at end") << QString("abc" + kana) << true;
      QTest::newRow("emoji") << QStringLiteral("\U0001F600") << false;
      QTest::newRow("emoji then kana") << QString(QStringLiteral("\U0001F600") + kana) << true;
      QTest::newRow("unpaired high") << high << false;
      QTest::newRow("unpaired low") << low << false;
      QTest::newRow("trailing high") << QString("abc" + high) << false;
      QTest::newRow("reversed pair") << QString(low + high) << false;
      QTest::newRow("separated pair") << QString(high + "x" + low) << false;
      QTest::newRow("high before BMP kana") << QString(high + QChar(0x3042)) << true;
      QTest::newRow("high before supplementary kana") << QString(high + kana) << true;
      QTest::newRow("low before supplementary kana") << QString(low + kana) << true;
      QTest::newRow("embedded null") << QString(QString("abc") + QChar(0) + kana) << true;
      QTest::newRow("null splits pair") << QString(high + QChar(0) + low) << false;
      const QString longText(100000, QChar('a'));
      QTest::newRow("long non-Japanese") << longText << false;
      QTest::newRow("long early match") << QString(kana + longText) << true;
      QTest::newRow("long late match") << QString(longText + kana) << true;
   }

   void isJapanese()
   {
      QFETCH(QString, text);
      QFETCH(bool, expected);
      QCOMPARE(StringUtils::isJapanese(text), expected);
   }

   void allCodePoints()
   {
      // Cover every BMP and supplementary boundary against Qt's script data,
      // including unassigned characters and UTF-16 surrogate values.
      for (char32_t c = 0; c <= 0x10ffff; ++c)
      {
         const auto script = QChar::script(c);
         const bool expected = script == QChar::Script_Hiragana || script == QChar::Script_Katakana;
         const QString text = c <= 0xffff ? QString(QChar(static_cast<ushort>(c))) : QString::fromUcs4(&c, 1);
         QCOMPARE(StringUtils::isJapanese(text), expected);
      }
   }
};

QTEST_GUILESS_MAIN(StringUtilsTests)
#include "StringUtilsTests.moc"
