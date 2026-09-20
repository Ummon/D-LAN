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

using GUI::IconProvider;

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
   void init()
   {
      IconProvider::cachedIcons.clear();
      IconProvider::cachedIconsWithWarning.clear();
      IconProvider::cachedIcons.setMaxCost(IconProvider::MAX_CACHED_ICONS);
      IconProvider::cachedIconsWithWarning.setMaxCost(IconProvider::MAX_CACHED_ICONS);
   }

   void cleanupTestCase()
   {
      IconProvider::cachedIcons.clear();
      IconProvider::cachedIconsWithWarning.clear();
   }

#ifdef Q_OS_LINUX
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

   void unknownFileFallback()
   {
      const QImage expected = QFileIconProvider().icon(QFileIconProvider::File).pixmap(16, 16).toImage();
      QVERIFY(!expected.isNull());
      QCOMPARE(fileIcon("file.dlan-unknown-extension").pixmap(16, 16).toImage(), expected);
   }
#endif

#ifdef Q_OS_MACOS
   void nativeFileTypes()
   {
      // Ensure this exercises AppKit, rather than Qt's offscreen fallback.
      QCOMPARE(QGuiApplication::platformName(), QString("cocoa"));
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      const QString pdfPath = directory.filePath("remote-report.pdf");
      QVERIFY(!QFile::exists(pdfPath));
      const QIcon pdf = fileIcon(pdfPath);
      const QIcon png = fileIcon(directory.filePath("remote-image.png"));
      const QImage generic = QFileIconProvider().icon(QFileIconProvider::File).pixmap(32, 32).toImage();
      QVERIFY(!pdf.isNull());
      QVERIFY(!png.isNull());
      QVERIFY(pdf.pixmap(32, 32).toImage() != generic);
      QVERIFY(png.pixmap(32, 32).toImage() != pdf.pixmap(32, 32).toImage());
      QCOMPARE(IconProvider::getIcon(Common::Path(pdfPath)).cacheKey(), pdf.cacheKey());
      QVERIFY(!QFile::exists(pdfPath));

      for (const bool warning : {false, true})
      {
         const QPixmap retina = fileIcon(pdfPath, warning).pixmap(QSize(16, 16), 2.0);
         QCOMPARE(retina.size(), QSize(32, 32));
         QCOMPARE(retina.devicePixelRatio(), 2.0);
      }
   }

   void genericFileFallbacks_data()
   {
      QTest::addColumn<QString>("filename");
      QTest::newRow("unknown") << QString("file.dlan-unknown-extension");
      QTest::newRow("extensionless") << QString("README");
      QTest::newRow("trailing-dot") << QString("file.");
   }

   void genericFileFallbacks()
   {
      QFETCH(QString, filename);
      const QImage generic = QFileIconProvider().icon(QFileIconProvider::File).pixmap(32, 32).toImage();
      QVERIFY(!generic.isNull());
      QCOMPARE(fileIcon(filename).pixmap(32, 32).toImage(), generic);
      const QImage warning = fileIcon(filename, true).pixmap(32, 32).toImage();
      QVERIFY(!warning.isNull());
      QVERIFY(warning != generic);
   }
#endif

   void reusesTypeCache()
   {
      QCOMPARE(fileIcon("one.pdf").cacheKey(), fileIcon("two.PDF").cacheKey());
      QCOMPARE(fileIcon("one.pdf", true).cacheKey(), fileIcon("two.PDF", true).cacheKey());
      QCOMPARE(IconProvider::cachedIcons.size(), 1);
      QCOMPARE(IconProvider::cachedIconsWithWarning.size(), 1);
   }

   void boundedCache_data()
   {
      QTest::addColumn<bool>("warning");
      QTest::newRow("normal") << false;
      QTest::newRow("warning") << true;
   }

   void boundedCache()
   {
      QFETCH(bool, warning);
      auto& cache = warning ? IconProvider::cachedIconsWithWarning : IconProvider::cachedIcons;
      auto& otherCache = warning ? IconProvider::cachedIcons : IconProvider::cachedIconsWithWarning;
      QCOMPARE(cache.maxCost(), 256);

      // A caller's QIcon must remain usable after the cache evicts its copy.
      const QIcon held = fileIcon("held.pdf", warning);
      const QImage heldImage = held.pixmap(16, 16).toImage();
      QVERIFY(!heldImage.isNull());
      const QString heldKey = cache.keys().first();
      for (int i = 0; i < cache.maxCost() + 8; ++i)
      {
#ifdef Q_OS_LINUX
         const QString type = QString("application/x-dlan-cache-test-%1").arg(i);
#else
         const QString type = QString(".dlan-cache-test-%1").arg(i);
#endif
         IconProvider::getIconCacheByType(type, warning);
         QVERIFY(cache.size() <= cache.maxCost());
      }
      QCOMPARE(cache.size(), cache.maxCost());
      QVERIFY(!cache.contains(heldKey));
      QVERIFY(otherCache.isEmpty());
      QCOMPARE(held.pixmap(16, 16).toImage(), heldImage);
      QCOMPARE(fileIcon("held.pdf", warning).pixmap(16, 16).toImage(), heldImage);
      QCOMPARE(cache.size(), cache.maxCost());
   }

   void cacheHitsRefreshRecencyAndRetainNullIcons()
   {
      auto& cache = IconProvider::cachedIcons;
      cache.setMaxCost(2);
      // Null native lookups must still count as hits, including for LRU order.
      cache.insert(".dlan-null", new QIcon());
      cache.insert(".dlan-old", new QIcon());
      const auto* cachedNull = cache.object(".dlan-null");
      cache.object(".dlan-old"); // Make the null entry least recently used again.
      QVERIFY(IconProvider::getIconCacheByType(".dlan-null", false).isNull());
      IconProvider::getIconCacheByType(".dlan-new", false);
      QVERIFY(cache.contains(".dlan-null"));
      QCOMPARE(cache.object(".dlan-null"), cachedNull);
      QVERIFY(!cache.contains(".dlan-old"));
      QVERIFY(cache.contains(".dlan-new"));
      QCOMPARE(cache.size(), 2);
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
#ifndef Q_OS_MACOS
   QApplication::setDesktopSettingsAware(false);
#endif
   QApplication app(argc, argv);
   TestsIconProvider tests;
   return QTest::qExec(&tests, argc, argv);
}
#include "TestsIconProvider.moc"
