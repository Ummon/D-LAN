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

#include <priv/QtLogger.h>
using namespace LM;

#include <QtGlobal>

#include <IEntry.h>

/**
  * @class LM::QtLogger
  *
  * A special object is create to handle all Qt message. For example
  * when a signal is connected to an unknown slot, the warning will be
  * caught and logged here.
  * Warning, the Qt messages are not caught during unit testing because 'QTest::qExec(..)'
  * will create its own handle and discard the current one.
  */

namespace
{
   // Internal linkage: nothing outside this compilation unit has any use of it.
   void handler(QtMsgType type, const QMessageLogContext&, const QString& msg)
   {
      Severity s =
         type == QtDebugMsg ? SV_DEBUG :
         type == QtWarningMsg ? SV_WARNING :
         type == QtCriticalMsg ? SV_ERROR :
         type == QtFatalMsg ? SV_FATAL_ERROR : SV_UNKNOWN;

      QtLogger::getInstance().log(msg, s);
   }

   /**
     * Installs the handler as soon as this compilation unit is loaded, like the former 'QtLogger::me' object
     * did through its constructor. Only the tests call 'Builder::initMsgHandler()', the core and the GUI rely
     * on this to have the Qt messages logged.
     * Nothing but a function pointer is installed here, no other static object is used.
     */
   struct MsgHandlerInstaller
   {
      MsgHandlerInstaller() { QtLogger::initMsgHandler(); }
   };

   const MsgHandlerInstaller msgHandlerInstaller;
}

/**
  * Built on the first handled message, thus it can't be used before being initialized whatever the
  * initialization order of the static objects. Never deleted: the handler stays installed until the process
  * ends and Qt can emit messages while the static objects are being destroyed.
  */
const QtLogger& QtLogger::getInstance()
{
   static const QtLogger* const instance = new QtLogger();
   return *instance;
}

/**
  * Fake class method to avoid the case where this compilation unit (.o)
  * is dropped by the linker when using 'libLogManager.a'.
  */
void QtLogger::initMsgHandler()
{
   qInstallMessageHandler(handler);
}

QtLogger::QtLogger() :
   Logger("Qt")
{
}
