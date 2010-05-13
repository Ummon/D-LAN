#include <priv/QtLogger.h>
using namespace LM;

#include <QtGlobal>

#include <IEntry.h>

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
void QtLogger::init()
{
}

QtLogger::QtLogger() :
   Logger("Qt")
{
   qInstallMsgHandler(handler);
}
