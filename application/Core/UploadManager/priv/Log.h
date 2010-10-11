#ifndef UPLOADMANAGER_LOG_H
#define UPLOADMANAGER_LOG_H

#include <Common/LogManager/Builder.h>
#include <Common/LogManager/ILogger.h>

namespace UM
{
   // There is only one logger for the UploadManager component.
   static QSharedPointer<LM::ILogger> logger(LM::Builder::newLogger("UploadManager"));

   #define L_USER(mess) LOG_USER(logger, mess)
   #define L_DEBU(mess) LOG_DEBU(logger, mess)
   #define L_WARN(mess) LOG_WARN(logger, mess)
   #define L_ERRO(mess) LOG_ERRO(logger, mess)
   #define L_FATA(mess) LOG_FATA(logger, mess)
}

#endif
