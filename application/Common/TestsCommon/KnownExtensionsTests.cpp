#include <QTest>

#include <Common/KnownExtensions.h>

using Common::KnownExtensions;
using Common::ExtensionCategory;

class KnownExtensionsTests : public QObject
{
   Q_OBJECT

private slots:
   void filenames_data()
   {
      QTest::addColumn<QString>("filename");
      QTest::addColumn<int>("beginning");
      QTest::addColumn<QString>("extension");
      QTest::addColumn<QString>("stem");
      QTest::newRow("empty") << QString() << -1 << QString() << QString();
      QTest::newRow("no dot") << "mp3" << -1 << "" << "mp3";
      QTest::newRow("unknown") << "file.unknown" << -1 << "" << "file.unknown";
      QTest::newRow("simple") << "file.mp3" << 5 << "mp3" << "file";
      QTest::newRow("mixed case") << "file.Mp3" << 5 << "Mp3" << "file";
      QTest::newRow("compound") << "archive.TaR.Gz" << 8 << "TaR.Gz" << "archive";
      QTest::newRow("longest compound") << "a.tar.gz2" << 2 << "tar.gz2" << "a";
      QTest::newRow("multiple extensions") << "a.mp3.zip" << 6 << "zip" << "a.mp3";
      QTest::newRow("compound after dots") << "a.b.c.tar.gz" << 6 << "tar.gz" << "a.b.c";
      QTest::newRow("hidden") << ".mp3" << -1 << "" << ".mp3";
      QTest::newRow("hidden compound") << ".tar.gz" << -1 << "" << ".tar.gz";
      QTest::newRow("hidden with extension") << ".file.mp3" << 6 << "mp3" << ".file";
      QTest::newRow("consecutive dots") << "..MP3" << 2 << "MP3" << ".";
      QTest::newRow("trailing dot") << "file.mp3." << -1 << "" << "file.mp3.";
      QTest::newRow("only dots") << "....." << -1 << "" << ".....";
      QTest::newRow("compound without basename") << "tar.gz" << -1 << "" << "tar.gz";
      QTest::newRow("no partial match") << "file.notmp3" << -1 << "" << "file.notmp3";
      const QString expandingStem = QString("x.") + QChar(0x0130);
      QTest::newRow("expanding lowercase") << QString(expandingStem + ".MP3") << 4 << "MP3" << expandingStem;
      const QString kelvinExtension = QString("M") + QChar(0x212a) + "V";
      QTest::newRow("Unicode lowercase") << QString("x." + kelvinExtension) << 2 << kelvinExtension << "x";
      const QString nullStem = QString("x") + QChar(0) + "y";
      QTest::newRow("embedded null") << QString(nullStem + ".JPG") << 4 << "JPG" << nullStem;
      const QString longStem = QString("part.").repeated(20000) + "file";
      QTest::newRow("long dotted name") << QString(longStem + ".TAR.GZ2") << int(longStem.size() + 1) << "TAR.GZ2" << longStem;
      QTest::newRow("long dotted unknown") << QString(longStem + ".unknown") << -1 << "" << QString(longStem + ".unknown");
   }

   void filenames()
   {
      QFETCH(QString, filename);
      QFETCH(int, beginning);
      QFETCH(QString, extension);
      QFETCH(QString, stem);
      QCOMPARE(KnownExtensions::getBeginningExtension(filename), beginning);
      QCOMPARE(KnownExtensions::getExtension(filename), extension);
      QCOMPARE(KnownExtensions::removeExtension(filename), stem);
   }

   void allRegisteredExtensions()
   {
      for (int category = 1; category < KnownExtensions::nbCategory(); ++category)
      {
         for (const auto& extension : KnownExtensions::getExtensions(static_cast<ExtensionCategory>(category)))
         {
            for (const QString& spelling : {extension, extension.toUpper()})
            {
               QVERIFY(KnownExtensions::exists(spelling));
               QCOMPARE(KnownExtensions::getCategoryFrom(spelling), KnownExtensions::getCategoryFrom(extension));
               for (const QString& prefix : {QString("a."), QString(".hidden.name."), QString("many.").repeated(100)})
               {
                  const QString filename = prefix + spelling;
                  QCOMPARE(KnownExtensions::getBeginningExtension(filename), int(prefix.size()));
                  QCOMPARE(KnownExtensions::getExtension(filename), spelling);
                  QCOMPARE(KnownExtensions::removeExtension(filename), prefix.chopped(1));
               }
            }
         }
      }
   }
};

QTEST_GUILESS_MAIN(KnownExtensionsTests)
#include "KnownExtensionsTests.moc"
