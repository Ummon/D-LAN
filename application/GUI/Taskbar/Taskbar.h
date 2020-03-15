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
  
#pragma once

#include <QObject>
#include <QWidget>
#include <QEvent>
#include <QSharedPointer>
#include <QIcon>

#include <Taskbar/ITaskbarImpl.h>
#include <Taskbar/TaskbarTypes.h>

#ifdef Q_OS_WIN32
   #include <Taskbar/TaskbarImplWin.h>
#endif

namespace GUI
{
   class Taskbar
   {
   public:
      Taskbar() :
#ifdef Q_OS_WIN32
         impl(new TaskbarImplWin())
#else
         impl(nullptr) // No implementation.
#endif
      {}

      Taskbar(const QSharedPointer<ITaskbarImpl>& other) : impl(other) { }

      void setStatus(TaskbarButtonStatus status) { if (!this->impl.isNull()) this->impl->setStatus(status); }
      void setProgress(quint64 completed, quint64 total) { if (!this->impl.isNull()) this->impl->setProgress(completed, total); }
      void setOverlayIcon(const QIcon& icon, const QString& description) { if (!this->impl.isNull()) this->impl->setOverlayIcon(icon, description); }

#ifdef Q_OS_WIN32
      void setWinHandle(HWND winHandle) { this->impl->setWinHandle(winHandle); }

      /**
        * Only for Windows.
        * Must be called when a 'winEvent' event occurs.
        */
      void winEvent(MSG* message, long* result) { this->impl->winEvent(message, result); }
#endif

   private:
      QSharedPointer<ITaskbarImpl> impl;
   };
}
