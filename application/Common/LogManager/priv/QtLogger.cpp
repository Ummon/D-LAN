/**
  * Aybabtu - A decentralized LAN file sharing software.
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
  
#include <priv/QtLogger.h>
using namespace LM;

#include <QtGlobal>

#include <IEntry.h>

/**
  * @class QtLogger
  * A special objet is create to handle all Qt message. For example
  * when a signal is connected to an unknown slot, the warning will be
  * catched and logged here.
  * Warning, the Qt messages are not catched during unit tesing because 'QTest::qExec(..)'
  * will create its own handle and discard the current one.
  */

void handler(QtMsgType type, const char* msg)
{
   Severity s =
         type == QtDebugMsg ? SV_DEBUG :
         type == QtWarningMsg ? SV_WARNING :
         type == QtCriticalMsg ? SV_ERROR :
         type == QtFatalMsg ? SV_FATAL_ERROR : SV_UNKNOWN;

   QtLogger::me.log(msg, s);
}

const QtLogger QtLogger::me;

/**
  * Fake class method to avoid the case where this compilation unit (.o)
  * is dropped by the linker when using 'libLogManager.a'.
  */
void QtLogger::initMsgHandler()
{
   qInstallMsgHandler(handler);
}

QtLogger::QtLogger() :
   Logger("Qt")
{
   QtLogger::initMsgHandler();
}
