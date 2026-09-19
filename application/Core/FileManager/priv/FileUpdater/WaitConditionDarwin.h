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

#include <priv/FileUpdater/WaitCondition.h>

namespace FM
{
   class WaitConditionDarwin : public WaitCondition
   {
   public:
      WaitConditionDarwin();
      ~WaitConditionDarwin() override;
      WaitConditionDarwin(const WaitConditionDarwin&) = delete;
      WaitConditionDarwin& operator=(const WaitConditionDarwin&) = delete;

      void release() override;
      bool wait(int timeout = -1) override;
      int getFd() const;

   private:
      // Readability is the release state, also consumed by DirWatcherDarwin.
      int pfd[2];
   };
}
