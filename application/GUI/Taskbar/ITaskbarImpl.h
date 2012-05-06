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
  
#ifndef GUI_ITASKBARIMPL_H
#define GUI_ITASKBARIMPL_H

#include <QtGlobal>

#include <Taskbar/TaskbarTypes.h>

namespace GUI
{
   class ITaskbarImpl
   {
   public:
      virtual ~ITaskbarImpl() {}

      virtual void setStatus(TaskbarButtonStatus status) = 0;
      virtual void setProgress(quint64 completed, quint64 total) = 0;
      virtual void setOverlayIcon(const QIcon& icon, const QString& description) = 0;

#ifdef Q_OS_WIN32
      virtual void setWinHandle(HWND winHandle) = 0;
      virtual void winEvent(MSG* message, long* result) = 0;
#endif
   };
}

#endif
