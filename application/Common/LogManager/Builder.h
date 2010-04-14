#ifndef LOGMANAGER_BUILDER_H
#define LOGMANAGER_BUILDER_H

#include <QDir>
#include <QSharedPointer>

namespace LM
{
   class ILogger;

   class Builder
   {
   public:
      static QSharedPointer<ILogger> newLogger(const QString& name);
   };
}

// Some useful macros.
#ifdef DEBUG
   #define LOG_USER(logger, mess)  logger->log((mess), LM::EndUser, __FILE__, __LINE__)
   #define LOG_DEBUG(logger, mess) logger->log((mess), LM::Debug, __FILE__, __LINE__)
   #define LOG_WARN(logger, mess)  logger->log((mess), LM::Warning, __FILE__, __LINE__)
   #define LOG_ERR(logger, mess)   logger->log((mess), LM::Error, __FILE__, __LINE__)
   #define LOG_FATAL(logger, mess) logger->log((mess), LM::FatalError, __FILE__, __LINE__)
#else
   #define LOG_USER(logger, mess)  logger->log((mess), LM::EndUser)
   #define LOG_DEBUG(logger, mess)
   #define LOG_WARN(logger, mess)  logger->log((mess), LM::Warning)
   #define LOG_ERR(logger, mess)   logger->log((mess), LM::Error)
   #define LOG_FATAL(logger, mess) logger->log((mess), LM::FatalError)
#endif

#endif
