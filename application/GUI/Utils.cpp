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

#include <Common/Global.h>

#include <Utils.h>
using namespace GUI;

#include <QListView>
#include <QStringBuilder>
#include <QCoreApplication>
#include <QFileDialog>
#include <QDir>
#include <QDesktopServices>
#include <QUrl>
#include <QGridLayout>
#include <QTreeView>
#include <QLabel>

#include <Settings/RemoteFileDialog.h>
#include <RemoteBrowseDialog/RemoteBrowseDialog.h>
#include <Constants.h>

/**
  * Ask the user to choose one or more directories/files.
  * This function doesn't wait for the answer, see 'showModal(..)'.
  * @param selected Called with the chosen paths, there is at least one. It isn't called if the user cancels, nor
  *        once 'parent' is deleted: the dialog belongs to it.
  */
void Utils::askForDirectoriesOrFiles(
   QWidget* parent,
   QSharedPointer<RCC::ICoreConnection> coreConnection,
   const QString& title,
   const std::function<void(const QStringList&)>& selected
)
{
   RemoteBrowseDialog* dialog = new RemoteBrowseDialog(coreConnection, parent);
   dialog->setWindowTitle(title.isEmpty() ? QObject::tr("Select one or more directories and/or files") : title);
   QObject::connect(dialog, &QDialog::accepted, dialog, [dialog, selected] {
      const QStringList selectedPaths = dialog->getSelectedPaths();
      if (!selectedPaths.isEmpty())
         selected(selectedPaths);
   });
   Utils::showModal(dialog);
}

/**
  * Ask the user to choose a directory.
  * @param selected Called with the chosen directory, see 'askForDirectoriesOrFiles(..)'.
  */
void Utils::askForADirectoryToDownloadTo(
   QWidget* parent,
   QSharedPointer<RCC::ICoreConnection> coreConnection,
   const std::function<void(const QString&)>& selected
)
{
   RemoteBrowseDialog* dialog = new RemoteBrowseDialog(coreConnection, parent);
   dialog->setWindowTitle(QObject::tr("Select a directory where to download to"));
   dialog->setModes(RemoteBrowseDialog::DIR);
   QObject::connect(dialog, &QDialog::accepted, dialog, [dialog, selected] {
      const QStringList selectedPaths = dialog->getSelectedPaths();
      if (!selectedPaths.isEmpty())
         selected(selectedPaths.constFirst());
   });
   Utils::showModal(dialog);
}

QString Utils::emoticonsDirectoryPath()
{
   QString defaultPath = Common::Global::getResourceFolder() % "/" % Constants::EMOTICONS_DIRECTORY;
#if DEBUG
   if (!QDir(defaultPath).exists())
      return QCoreApplication::applicationDirPath() % "/../../resources/emoticons";
#endif
   return defaultPath;
}

void Utils::openFile(const QString& path)
{
   QDesktopServices::openUrl(QUrl::fromLocalFile(path));
}
