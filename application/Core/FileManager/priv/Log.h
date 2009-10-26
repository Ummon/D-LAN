#ifndef FILEMANAGER_LOG_H
#define FILEMANAGER_LOG_H

#include <Common/LogManager/Builder.h>
#include <Common/LogManager/ILogger.h>

namespace FM
{
   static QSharedPointer<LM::ILogger> logger(LM::Builder::newLogger("FileManager"));

#define LOG_USER(mess)  logger->log((mess), LM::EndUser)
#ifdef DEBUG
#  define LOG_DEBUG(mess) logger->log((mess), LM::Debug)
#else
#  define LOG_DEBUG(mess)
#endif
#define LOG_WARN(mess)  logger->log((mess), LM::Warning)
#define LOG_ERR(mess)   logger->log((mess), LM::Error)
#define LOG_FATAL(mess) logger->log((mess), LM::FatalError)
}
#endif
