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
  
#include <priv/FileUpdater/WaitCondition.h>
using namespace FM;

#include <QtCore/QtCore> // For the Q_OS_* defines.

#include <priv/Log.h>

#if defined(Q_OS_WIN32)
   #include <priv/FileUpdater/WaitConditionWin.h>
#elif defined(Q_OS_LINUX)
   #include <priv/FileUpdater/WaitConditionLinux.h>
#endif

WaitCondition* WaitCondition::getNewWaitCondition()
{
#if defined(Q_OS_WIN32)
   return new WaitConditionWin();
#elif defined(Q_OS_LINUX)
   return new WaitConditionLinux();   
#else
   LOG_WARN("Cannot create a WaitCondition for the current platform, no implementation.");
   return 0;
#endif
}


