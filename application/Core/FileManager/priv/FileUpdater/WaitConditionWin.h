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

#ifndef FILEMANAGER_WAITCONDITIONWIN_H
#define FILEMANAGER_WAITCONDITIONWIN_H

#include <priv/FileUpdater/WaitCondition.h>

#include <windows.h>

namespace FM
{
   /**
     * Implementation of 'WaitCondition' for the Windows platform.
     * see : http://msdn.microsoft.com/en-us/library/ms686211%28VS.85%29.aspx add associated pages.
     */
   class WaitConditionWin : public WaitCondition
   {
   public:
      WaitConditionWin();
      ~WaitConditionWin();

      void release();
      bool wait(int timeout = -1);
      void* getHandle();

   private:
      HANDLE handle;
   };
}

#endif
