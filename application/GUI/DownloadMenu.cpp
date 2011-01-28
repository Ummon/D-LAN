/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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

#include <QMenu>
#include <QAction>

DownloadMenu::DownloadMenu(QSharedPointer<RCC::ICoreConnection> coreConnection, const DirListModel& sharedDirsModel) :
   coreConnection(coreConnection), sharedDirsModel(sharedDirsModel)
{
}

void DownloadMenu::show(const QPoint& globalPosition)
{
   QMenu menu;

   for (QListIterator<Common::SharedDir> i(this->sharedDirsModel.getDirs()); i.hasNext();)
   {
      Common::SharedDir sharedDir = i.next();
      QAction* action = new QAction(
         QIcon(":/icons/ressources/download.png"),
         QString("Download selected entries to %1").arg(sharedDir.path),
         &menu
      );
      sharedDir.path = "/"; // A bit dirty, path semantic change, it's now the relative path (not the absolute path).
      action->setData(QVariant::fromValue(sharedDir));
      connect(action, SIGNAL(triggered()), this, SLOT(actionTriggered()));
      menu.addAction(action);
   }

   menu.exec(globalPosition);
}

void DownloadMenu::actionTriggered()
{
   QAction* action = static_cast<QAction*>(this->sender());
   Common::SharedDir sharedDir = action->data().value<Common::SharedDir>();
   emit downloadTo(sharedDir.ID, sharedDir.path);
}
