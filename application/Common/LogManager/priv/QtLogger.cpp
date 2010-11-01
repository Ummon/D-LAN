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

QtLogger::QtLogger()
   : Logger("Qt")
{
   QtLogger::initMsgHandler();
}
