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
  
#ifndef COMMON_IRUNNABLE_H
#define COMMON_IRUNNABLE_H

#include <QThread>

namespace Common
{
   /**
     * A runnable object is executed in a dedicated thead from a thread pool, see the class Common::ThreadPool.
     * Methods 'init(..)' and 'finished()' are called in the main thread. Method 'run()' is called in a thread dedicated to the runnable object.
     */
   class IRunnable
   {
   public:
      virtual ~IRunnable() {}

      virtual void init(QThread* thread) = 0;

      virtual void run() = 0;

      virtual void finished() = 0;
   };
}

#endif
