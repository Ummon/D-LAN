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

#include <QtTest>
#include <QApplication>
#include <QDir>
#include <QFile>
#include <QImage>
#include <QTemporaryDir>

#include <IconProvider.h>

class TestsIconProvider : public QObject
{
   Q_OBJECT

private:
   QTemporaryDir themeDirectory;

   static QIcon fileIcon(const QString& filename, bool warning = false)
   {
      Protos::Common::Entry entry;
      entry.set_type(Protos::Common::Entry::FILE);
      entry.set_name(filename.toStdString());
      return GUI::IconProvider::getIcon(entry, warning);
   }

private slots:
   void initTestCase()
   {
      QVERIFY(themeDirectory.isValid());
      const QString themePath = themeDirectory.filePath("dlan-test");
      QVERIFY(QDir().mkpath(themePath + "/16x16/mimetypes"));
      QFile index(themePath + "/index.theme");
      QVERIFY(index.open(QIODevice::WriteOnly));
      QVERIFY(index.write("[Icon Theme]\nName=D-LAN test\nDirectories=16x16/mimetypes\n"
                          "[16x16/mimetypes]\nSize=16\nContext=MimeTypes\nType=Fixed\n") > 0);
      index.close();
      const QList<QPair<QString, QColor>> icons{
         {"application-pdf", Qt::red}, {"text-plain", Qt::green},
         {"application-x-compressed-tar", Qt::blue}, {"application-gzip", Qt::magenta},
         {"text-x-makefile", Qt::cyan}, {"image-x-generic", Qt::yellow}
      };
      for (const auto& icon : icons)
      {
         QImage pixels(16, 16, QImage::Format_ARGB32);
         pixels.fill(icon.second);
         QVERIFY(pixels.save(themePath + "/16x16/mimetypes/" + icon.first + ".png"));
      }
      QIcon::setThemeSearchPaths({themeDirectory.path()});
      QIcon::setFallbackSearchPaths({});
      QIcon::setFallbackThemeName(QString());
      QIcon::setThemeName("dlan-test");
   }

   void fileTypes_data()
   {
      QTest::addColumn<QString>("filename");
      QTest::addColumn<QColor>("expected");
      QTest::newRow("pdf") << QString("report.pdf") << QColor(Qt::red);
      QTest::newRow("uppercase") << QString("REPORT.PDF") << QColor(Qt::red);
      QTest::newRow("text") << QString("notes.txt") << QColor(Qt::green);
      QTest::newRow("compound-extension") << QString("backup.tar.gz") << QColor(Qt::blue);
      QTest::newRow("single-extension") << QString("backup.gz") << QColor(Qt::magenta);
      QTest::newRow("extensionless-name") << QString("Makefile") << QColor(Qt::cyan);
      QTest::newRow("generic-image-fallback") << QString("photo.jpg") << QColor(Qt::yellow);
   }

   void fileTypes()
   {
      QFETCH(QString, filename);
      QFETCH(QColor, expected);
      // These files need not exist locally: this is also how remote entries work.
      const QIcon icon = fileIcon(filename);
      QVERIFY(!icon.isNull());
      QCOMPARE(icon.pixmap(16, 16).toImage().pixelColor(2, 2), expected);
      QCOMPARE(GUI::IconProvider::getIcon(Common::Path(filename)).pixmap(16, 16).toImage().pixelColor(2, 2), expected);
   }

   void reusesMimeTypeCache()
   {
      QCOMPARE(fileIcon("one.pdf").cacheKey(), fileIcon("two.PDF").cacheKey());
   }

   void unknownFileFallback()
   {
      const QImage expected = QFileIconProvider().icon(QFileIconProvider::File).pixmap(16, 16).toImage();
      QVERIFY(!expected.isNull());
      QCOMPARE(fileIcon("file.dlan-unknown-extension").pixmap(16, 16).toImage(), expected);
   }

   void warningBadge()
   {
      const QImage plain = fileIcon("report.pdf").pixmap(16, 16).toImage();
      const QImage warning = fileIcon("report.pdf", true).pixmap(16, 16).toImage();
      QVERIFY(!warning.isNull());
      QCOMPARE(warning.pixelColor(0, 0), plain.pixelColor(0, 0));
      QVERIFY(warning != plain);
      QCOMPARE(fileIcon("other.pdf", true).cacheKey(), fileIcon("report.pdf", true).cacheKey());
      QCOMPARE(fileIcon("report.pdf").pixmap(16, 16).toImage(), plain);
   }

   void directoryIcon()
   {
      Protos::Common::Entry entry;
      entry.set_type(Protos::Common::Entry::DIR);
      entry.set_name("folder.pdf");
      QCOMPARE(GUI::IconProvider::getIcon(entry).pixmap(16, 16).toImage(),
               GUI::IconProvider::getDirectoryIcon().pixmap(16, 16).toImage());
   }
};

int main(int argc, char** argv)
{
   QApplication::setDesktopSettingsAware(false);
   QApplication app(argc, argv);
   TestsIconProvider tests;
   return QTest::qExec(&tests, argc, argv);
}
#include "TestsIconProvider.moc"
