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
  
#ifndef GUI_TASKBARIMPLWIN_H
#define GUI_TASKBARIMPLWIN_H

#include <QEvent>
#include <QObject>
#include <QWidget>
#include <QIcon>

#include <Taskbar/ITaskbarImpl.h>
#include <Taskbar/WinUtil.h>

namespace GUI
{
   class TaskbarImplWin : public QObject, public ITaskbarImpl
   {
      Q_OBJECT

   public:
      TaskbarImplWin();
      ~TaskbarImplWin();

      void setStatus(TaskbarButtonStatus status);
      void setProgress(quint64 completed, quint64 total);
      void setOverlayIcon(const QIcon& icon, const QString& description);

      void setWinHandle(HWND winHandle);
      void winEvent(MSG* message, long* result);

   private:
      void initTaskbarButton();

      HWND winHandle;
      unsigned int iDTaskbarButtonCreated;
      ITaskbarList3* taskbarInterface;
   };
}

#endif
