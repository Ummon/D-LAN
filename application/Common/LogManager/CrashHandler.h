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

#include <QString>

namespace LM
{
   /**
     * Catches the fatal errors which normally kill the process silently (access violations,
     * unhandled C++ exceptions, abort(), pure virtual calls, ...) and dumps a symbolized
     * stack trace:
     *  - into the current D-LAN log file, as a 'SV_FATAL_ERROR' entry;
     *  - into a standalone '<log dir>/crash_<date>.log' written with plain Win32 calls, so
     *    the report survives even when the log machinery itself is broken;
     *  - optionally into a '<log dir>/crash_<date>.dmp' minidump, openable in a debugger.
     *
     * 'install()' must be called as early as possible in 'main()', but *after*
     * 'Builder::setLogDirName(..)' so the report lands in the right directory.
     */
   class CrashHandler
   {
   public:
      static void install(bool writeMiniDump = true);

      /**
        * Symbolized stack trace of the calling thread, usable outside of a crash
        * (e.g. to log where an unexpected exception was caught).
        * @param framesToSkip Number of innermost frames to hide, this function excluded.
        */
      static QString stackTrace(int framesToSkip = 0);
   };
}
