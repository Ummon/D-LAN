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
  
#include <QtCore/QtCore> // For the Q_OS_* defines.

#if !defined(FILEMANAGER_WAITCONDITIONLINUX_H) and defined(Q_OS_LINUX)
#define FILEMANAGER_WAITCONDITIONLINUX_H

#include <QMutex>
#include <QWaitCondition>

#include <priv/FileUpdater/WaitCondition.h>

namespace FM
{
   /**
     * Implementation of 'WaitCondition' for the Linux platform.
     * For the moment it uses Qt classes but in the future it should use native
     * primitives to permit DirWatcherLinux to wait on a filesystem event AND on a WaitConditionLinux.
     * See the Windows implementation for more information.
     */
   class WaitConditionLinux : public WaitCondition
   {
   public:
      WaitConditionLinux();
      ~WaitConditionLinux();

      void release();
      bool wait(int timeout = -1);
      void* getHandle();

   private:
      bool released;
      QMutex mutex;
      QWaitCondition waitCondition;
   };
}

#endif
