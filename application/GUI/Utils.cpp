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

#include <QListView>
#include <QFileDialog>
#include <QGridLayout>
#include <QLabel>

#include <Settings/RemoteFileDialog.h>

/**
  * Ask the user to choose one or more directories.
  * TODO: browse the remotes directories (Core) not the local ones.
  */
QStringList Utils::askForDirectories(QSharedPointer<RCC::ICoreConnection> coreConnection, const QString& message)
{
   if (coreConnection->isLocal())
   {
      QFileDialog fileDialog(0, "Choose a directory");
      fileDialog.setOption(QFileDialog::DontUseNativeDialog, true);
      fileDialog.setFileMode(QFileDialog::Directory);

      if (!message.isNull())
      {
         QGridLayout* layout = fileDialog.findChild<QGridLayout*>();
         QLabel* label = new QLabel(message, &fileDialog);
         layout->addWidget(label, layout->rowCount(), 0, 1, -1, Qt::AlignLeft | Qt::AlignVCenter);
      }

      QListView* l = fileDialog.findChild<QListView*>("listView");
      if (l)
         l->setSelectionMode(QAbstractItemView::ExtendedSelection);

      if (fileDialog.exec())
      {
         return fileDialog.selectedFiles();
      }
      return QStringList();
   }
   else
   {
      RemoteFileDialog fileDialog;
      fileDialog.setWindowTitle("Remote directory");
      fileDialog.setText("Remote directory to share : ");
      if (fileDialog.exec())
      {
         return QStringList() << fileDialog.getFolder();
      }
      return QStringList();
   }
}

QStringList Utils::askForDirectoriesToDownloadTo(QSharedPointer<RCC::ICoreConnection> coreConnection)
{
   return Utils::askForDirectories(coreConnection, "<img src=\":/icons/ressources/information.png\" /> <strong>" + QObject::tr("The choosen directory will be shared") + "</strong>");
}
