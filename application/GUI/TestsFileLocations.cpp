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
#include <QDBusConnection>
#include <QDBusContext>
#include <QDBusMessage>
#include <QDBusPendingCallWatcher>
#include <QDesktopServices>
#include <QTemporaryDir>
#include <QUrl>

#include <Utils.h>
#include <Browse/BrowseModel.h>

class FileManagerService : public QObject, protected QDBusContext
{
   Q_OBJECT
   Q_CLASSINFO("D-Bus Interface", "org.freedesktop.FileManager1")

public:
   QString method;
   QStringList uris;
   QString startupId;
   QDBusMessage request;

public slots:
   void ShowItems(const QStringList& items, const QString& startup)
   {
      method = "ShowItems";
      uris = items;
      startupId = startup;
      request = message();
      setDelayedReply(true);
   }

   void ShowFolders(const QStringList& items, const QString& startup)
   {
      ShowItems(items, startup);
      method = "ShowFolders";
   }
};

class LocalBrowseModel : public GUI::BrowseModel
{
public:
   LocalBrowseModel(const GUI::SharedEntryListModel& shares, const Protos::Common::Entry& sharedRoot, const Protos::Common::Entry& file) :
      BrowseModel({}, shares, {}, false)
   {
      auto* parent = this->root->insertChild(sharedRoot);
      if (!file.path().empty())
      {
         Protos::Common::Entry subdirectory;
         subdirectory.set_type(Protos::Common::Entry::DIR);
         subdirectory.set_name("subdirectory");
         parent = parent->insertChild(subdirectory);
      }
      parent->insertChild(file);
   }
};

class TestsFileLocations : public QObject
{
   Q_OBJECT

private:
   FileManagerService service;
   QDBusConnection serviceBus = QDBusConnection::connectToBus(QDBusConnection::SessionBus, "file-location-tests");
   QList<QUrl> openedUrls;

public slots:
   void recordUrl(const QUrl& url) { openedUrls.append(url); }

private slots:
   void initTestCase()
   {
      QVERIFY(serviceBus.isConnected());
      QVERIFY(serviceBus.registerService("org.freedesktop.FileManager1"));
      QVERIFY(serviceBus.registerObject("/org/freedesktop/FileManager1", &service, QDBusConnection::ExportAllSlots));
      QDesktopServices::setUrlHandler("file", this, "recordUrl");
   }

   void init()
   {
      service.method.clear();
      service.uris.clear();
      openedUrls.clear();
   }

   void revealFile_data()
   {
      QTest::addColumn<QString>("error");
      QTest::addColumn<bool>("folderFails");
      QTest::newRow("selection") << QString() << false;
      QTest::newRow("unsupported") << QString("org.freedesktop.DBus.Error.UnknownMethod") << false;
      QTest::newRow("unavailable") << QString("org.freedesktop.DBus.Error.ServiceUnknown") << true;
      QTest::newRow("failed") << QString("org.freedesktop.DBus.Error.Failed") << false;
   }

   void revealFile()
   {
      QFETCH(QString, error);
      QFETCH(bool, folderFails);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      const QString path = directory.filePath(QString::fromUtf8("résumé #1 %20 'test'.txt"));
      QFile file(path);
      QVERIFY(file.open(QIODevice::WriteOnly));
      file.close();

      GUI::Utils::openLocation(path);
      QTRY_COMPARE(service.uris.size(), 1);
      QCOMPARE(service.method, QString("ShowItems"));
      QCOMPARE(service.uris.first(), QUrl::fromLocalFile(path).toString(QUrl::FullyEncoded));
      QVERIFY(service.startupId.isEmpty());
      // The GUI event loop remains usable while the file manager has not replied.
      QVERIFY(openedUrls.isEmpty());
      QCOMPARE(QCoreApplication::instance()->findChildren<QDBusPendingCallWatcher*>().size(), 1);
      QVERIFY(serviceBus.send(error.isEmpty() ? service.request.createReply() : service.request.createErrorReply(error, "Test failure")));
      if (!error.isEmpty())
      {
         QTRY_COMPARE(service.method, QString("ShowFolders"));
         QCOMPARE(service.uris, QStringList{QUrl::fromLocalFile(directory.path()).toString(QUrl::FullyEncoded)});
         QVERIFY(openedUrls.isEmpty());
         QVERIFY(serviceBus.send(folderFails ? service.request.createErrorReply(error, "Test failure") : service.request.createReply()));
      }
      QTRY_VERIFY(QCoreApplication::instance()->findChildren<QDBusPendingCallWatcher*>().isEmpty());
      if (!folderFails)
         QVERIFY(openedUrls.isEmpty());
      else
         QCOMPARE(openedUrls, QList<QUrl>{QUrl::fromLocalFile(directory.path())});
   }

   void browseFileLocation_data()
   {
      QTest::addColumn<bool>("inSubdirectory");
      QTest::newRow("shared-root") << false;
      QTest::newRow("subdirectory") << true;
   }

   void browseFileLocation()
   {
      QFETCH(bool, inSubdirectory);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      const QString filename = "selected.txt";
      const QString relativeDirectory = inSubdirectory ? "subdirectory/" : "";
      if (inSubdirectory)
         QVERIFY(QDir(directory.path()).mkdir("subdirectory"));
      const QString path = directory.filePath(relativeDirectory + filename);
      QFile file(path);
      QVERIFY(file.open(QIODevice::WriteOnly));
      file.close();

      Common::SharedEntry share;
      share.ID = Common::Hash::rand();
      share.path = Common::Path(directory.path() + '/');
      GUI::SharedEntryListModel shares;
      shares.setEntries({share});
      Protos::Common::Entry sharedRoot;
      sharedRoot.set_type(Protos::Common::Entry::DIR);
      sharedRoot.set_path("");
      sharedRoot.set_name("");
      sharedRoot.mutable_shared_entry()->mutable_id()->set_hash(share.ID.getData(), Common::Hash::HASH_SIZE);
      sharedRoot.mutable_shared_entry()->set_path(share.path.toString().toStdString());
      sharedRoot.mutable_shared_entry()->set_shared_name("Shared directory");
      Protos::Common::Entry entry;
      entry.set_type(Protos::Common::Entry::FILE);
      entry.set_path(relativeDirectory.toStdString());
      entry.set_name(filename.toStdString());
      LocalBrowseModel model(shares, sharedRoot, entry);
      const QModelIndex parent = inSubdirectory ? model.index(0, 0, model.index(0, 0)) : model.index(0, 0);
      const QModelIndex index = model.index(0, 0, parent);
      QVERIFY(index.isValid());
      QCOMPARE(model.getPath(index, true), path);
      GUI::Utils::openLocation(model.getPath(index, true));
      QTRY_COMPARE(service.method, QString("ShowItems"));
      QCOMPARE(service.uris, QStringList{QUrl::fromLocalFile(path).toString(QUrl::FullyEncoded)});
      QVERIFY(serviceBus.send(service.request.createReply()));
      QTRY_VERIFY(QCoreApplication::instance()->findChildren<QDBusPendingCallWatcher*>().isEmpty());
   }

   void openDirectory_data()
   {
      QTest::addColumn<QString>("suffix");
      QTest::addColumn<bool>("fail");
      QTest::newRow("directory") << QString() << false;
      QTest::newRow("shared-directory") << QString("/") << false;
      QTest::newRow("directory-fallback") << QString("/") << true;
   }

   void openDirectory()
   {
      QFETCH(QString, suffix);
      QFETCH(bool, fail);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      const QString path = directory.filePath(QString::fromUtf8("shared résumé #1 %20"));
      QVERIFY(QDir().mkdir(path));
      GUI::Utils::openLocation(path + suffix);
      QTRY_COMPARE(service.uris.size(), 1);
      QCOMPARE(service.method, QString("ShowFolders"));
      const QUrl expectedUrl = QUrl::fromLocalFile(QFileInfo(path + suffix).absoluteFilePath());
      QCOMPARE(service.uris.first(), expectedUrl.toString(QUrl::FullyEncoded));
      QVERIFY(openedUrls.isEmpty());
      QVERIFY(serviceBus.send(fail ? service.request.createErrorReply("org.freedesktop.DBus.Error.UnknownMethod", "Test failure") : service.request.createReply()));
      QTRY_VERIFY(QCoreApplication::instance()->findChildren<QDBusPendingCallWatcher*>().isEmpty());
      if (fail)
         QCOMPARE(openedUrls, QList<QUrl>{expectedUrl});
      else
         QVERIFY(openedUrls.isEmpty());
   }

   void emptyPath()
   {
      GUI::Utils::openLocation(QString());
      QVERIFY(openedUrls.isEmpty());
      QVERIFY(QCoreApplication::instance()->findChildren<QDBusPendingCallWatcher*>().isEmpty());
   }

   void cleanupTestCase()
   {
      QDesktopServices::unsetUrlHandler("file");
      serviceBus.unregisterObject("/org/freedesktop/FileManager1");
      serviceBus.unregisterService("org.freedesktop.FileManager1");
   }
};

QTEST_GUILESS_MAIN(TestsFileLocations)
#include "TestsFileLocations.moc"
