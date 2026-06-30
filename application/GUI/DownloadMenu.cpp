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

#include <DownloadMenu.h>
using namespace GUI;

#include <QAction>

#include <Utils.h>

/**
  * @class GUI::DownloadMenu
  *
  * Show the list of shared directory as a menu.
  * - The menu can be shown by calling 'show(..)'.
  * - When the user select an action, the signal 'downloadTo(..)' is emitted.
  * - Can be sub-classed to add some entries. In this case 'onShowMenu(..)' must be overridden.
  */

DownloadMenu::DownloadMenu(const SharedEntryListModel& sharedEntryListModel) :
   sharedEntryListModel(sharedEntryListModel)
{
}

void DownloadMenu::show(const QPoint& globalPosition)
{
   QMenu menu;

   const QList<Common::SharedEntry>& sharedDirs = this->sharedEntryListModel.getSharedDirectories();

   if (!sharedDirs.isEmpty())
   {
      QAction* actionDownload = new QAction(
         QIcon(":/icons/resources/download.svg"),
         tr("Download selected items to the first directory folder with enough free space"),
         &menu
      );
      connect(actionDownload, &QAction::triggered, this, &DownloadMenu::download);
      menu.addAction(actionDownload);
   }

   for (const auto& sharedDir : sharedDirs)
   {
      QAction* action = new QAction(
         QIcon(":/icons/resources/download.svg"),
         QString(tr("Download selected items to %1")).arg(sharedDir.path.toString()),
         &menu
      );
      action->setData(QVariant::fromValue(sharedDir.ID));
      connect(action, &QAction::triggered, this, &DownloadMenu::actionTriggered);
      menu.addAction(action);
   }

   QAction* actionChooseAndDownload = new QAction(
      QIcon(":/icons/resources/download.svg"),
      tr("Download selected items to . . ."),
      &menu
   );
   connect(actionChooseAndDownload, &QAction::triggered, this, qOverload<>(&DownloadMenu::downloadTo));
   menu.addAction(actionChooseAndDownload);

   this->onShowMenu(menu);

   menu.exec(globalPosition);
}

void DownloadMenu::actionTriggered()
{
   QAction* action = static_cast<QAction*>(this->sender());
   if (!action->data().isNull())
      emit downloadTo(action->data().value<Common::Hash>());
}
