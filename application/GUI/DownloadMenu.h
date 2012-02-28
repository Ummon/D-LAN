/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#ifndef GUI_DOWNLOADMENU_H
#define GUI_DOWNLOADMENU_H

#include <QObject>
#include <QSharedPointer>
#include <QStringList>
#include <QPoint>
#include <QMenu>

#include <Common/Hash.h>

#include <Settings/DirListModel.h>

namespace GUI
{
   class DownloadMenu : public QObject
   {
      Q_OBJECT
   public:
      DownloadMenu(const DirListModel& sharedDirsModel);
      void show(const QPoint& globalPosition);

   signals:
      void askDirectories(QStringList& dirs);
      void downloadTo(const Common::Hash&, const QString&);

   private slots:
      void actionTriggered();

   private:
      virtual void onShowMenu(QMenu&) {}

      const DirListModel& sharedDirsModel;
   };
}

#endif
