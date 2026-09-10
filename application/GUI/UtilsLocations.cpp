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

#include <Utils.h>
using namespace GUI;

#include <QCoreApplication>
#include <QDesktopServices>
#include <QDir>
#include <QFileInfo>
#include <QProcess>
#include <QUrl>
#include <QWidget>

#ifdef Q_OS_LINUX
   #include <QDBusConnection>
   #include <QDBusMessage>
   #include <QDBusPendingCallWatcher>
   #include <QDBusPendingReply>
   #include <QGuiApplication>
   #include <QTimer>
   #include <QWindow>
   #include <qpa/qplatformwindow.h>
   #include <qpa/qplatformwindow_p.h>
#endif

#ifdef Q_OS_LINUX
namespace
{
   void showInFileManager(const QUrl& url, const QUrl& directoryUrl, bool selectFile, const QString& activationToken)
   {
      const QString method = selectFile ? "ShowItems" : "ShowFolders";
      qDebug() << "File manager request" << method << url << "with activation token:" << !activationToken.isEmpty();
      // Avoid synchronous D-Bus introspection in the GUI thread.
      QDBusMessage request = QDBusMessage::createMethodCall(
         "org.freedesktop.FileManager1", "/org/freedesktop/FileManager1",
         "org.freedesktop.FileManager1", method
      );
      request << QStringList{url.toString(QUrl::FullyEncoded)} << activationToken;
      auto* watcher = new QDBusPendingCallWatcher(QDBusConnection::sessionBus().asyncCall(request, 5000), QCoreApplication::instance());
      QObject::connect(watcher, &QDBusPendingCallWatcher::finished, watcher,
         [url, directoryUrl, selectFile, method, activationToken](QDBusPendingCallWatcher* call)
         {
            const QDBusPendingReply<> reply = *call;
            if (reply.isError())
            {
               qWarning() << "File manager" << method << "failed for" << url << ":" << reply.error().message();
               if (selectFile)
                  showInFileManager(directoryUrl, directoryUrl, false, activationToken);
               else
                  QDesktopServices::openUrl(directoryUrl);
            }
            call->deleteLater();
         }
      );
   }

   void openFileManager(const QUrl& url, const QUrl& directoryUrl, bool selectFile, QWidget* parent)
   {
#if QT_CONFIG(wayland)
      auto* application = qobject_cast<QGuiApplication*>(QCoreApplication::instance());
      // QMenu closes before triggering its action, so focusWindow() can already
      // be null. Use the window that owns the action whenever it is available.
      auto* window = parent ? parent->window()->windowHandle() : application ? application->focusWindow() : nullptr;
      // A context menu may disappear before the compositor returns its token.
      while (window && window->type() == Qt::Popup && window->transientParent())
         window = window->transientParent();
      auto* waylandApp = application ? application->nativeInterface<QNativeInterface::QWaylandApplication>() : nullptr;
      auto* waylandWindow = window ? dynamic_cast<QNativeInterface::Private::QWaylandWindow*>(window->handle()) : nullptr;
      if (waylandApp && waylandWindow)
      {
         // FileManager1 uses its startup ID argument as the Wayland activation token.
         // Without it, an existing file-manager window can remain behind D-LAN.
         auto* timeout = new QTimer(application);
         const auto finish = [timeout, url, directoryUrl, selectFile](const QString& token)
         {
            if (!timeout->isActive())
               return;
            timeout->stop();
            showInFileManager(url, directoryUrl, selectFile, token);
            timeout->deleteLater();
         };
         QObject::connect(waylandWindow, &QNativeInterface::Private::QWaylandWindow::xdgActivationTokenCreated, timeout, finish);
         QObject::connect(waylandWindow, &QObject::destroyed, timeout, [finish] { finish({}); });
         QObject::connect(timeout, &QTimer::timeout, timeout, [finish] { finish({}); });
         timeout->start(1000);
         waylandWindow->requestXdgActivationToken(waylandApp->lastInputSerial());
         return;
      }
#endif
      Q_UNUSED(parent)
      showInFileManager(url, directoryUrl, selectFile, {});
   }
}
#endif

void Utils::openLocations(const QStringList& paths, QWidget* parent)
{
   for (const QString& path : paths)
      Utils::openLocation(path, parent);
}

/**
  * Open a directory, or select a file in its containing directory.
  * If the desktop does not support selection, open the containing directory.
  */
void Utils::openLocation(const QString& path, QWidget* parent)
{
   if (path.isEmpty())
      return;

#ifdef Q_OS_WIN32
   Q_UNUSED(parent)
   QProcess explorer;
   if (!QFileInfo(path).isDir())
      explorer.setArguments(QStringList() << "/select,");
   explorer.setNativeArguments("\"" + QDir::toNativeSeparators(path) + "\"");
   explorer.setProgram("explorer");
   explorer.start();
   explorer.waitForFinished(5000);
#else
   const QFileInfo fileInfo(path);
   const QUrl directoryUrl = QUrl::fromLocalFile(fileInfo.isDir() ? fileInfo.absoluteFilePath() : fileInfo.absolutePath());
#ifdef Q_OS_LINUX
   openFileManager(QUrl::fromLocalFile(fileInfo.absoluteFilePath()), directoryUrl, !fileInfo.isDir(), parent);
#else
   Q_UNUSED(parent)
   QDesktopServices::openUrl(directoryUrl);
#endif
#endif
}
