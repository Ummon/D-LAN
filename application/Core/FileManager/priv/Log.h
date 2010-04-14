#ifndef FILEMANAGER_LOG_H
#define FILEMANAGER_LOG_H

#include <Common/LogManager/Builder.h>
#include <Common/LogManager/ILogger.h>

namespace FM
{
   // There is only one logger for the FileManager component.
   static QSharedPointer<LM::ILogger> logger(LM::Builder::newLogger("FileManager"));

   #define L_USER(mess) LOG_FATAL(logger, mess)
   #define L_DEBUG(mess) LOG_FATAL(logger, mess)
   #define L_WARN(mess) LOG_FATAL(logger, mess)
   #define L_ERR(mess) LOG_FATAL(logger, mess)
   #define L_FATAL(mess) LOG_FATAL(logger, mess)
}

#endif
