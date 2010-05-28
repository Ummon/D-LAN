#ifndef LOGMANAGER_BUILDER_H
#define LOGMANAGER_BUILDER_H

#include <QDir>
#include <QSharedPointer>

#include "IEntry.h"

namespace LM
{
   class ILogger;

   class Builder
   {
   public:
      static QSharedPointer<ILogger> newLogger(const QString& name);
      static QSharedPointer<IEntry> decode(const QString& line);
      static void initMsgHandler();
   };
}

// Some useful macros.
#ifdef DEBUG
   #define LOG_USER(logger, mess) logger->log((mess), LM::SV_END_USER, __FILE__, __LINE__)
   #define LOG_DEBU(logger, mess) logger->log((mess), LM::SV_DEBUG, __FILE__, __LINE__)
   #define LOG_WARN(logger, mess) logger->log((mess), LM::SV_WARNING, __FILE__, __LINE__)
   #define LOG_ERRO(logger, mess) logger->log((mess), LM::SV_ERROR, __FILE__, __LINE__)
   #define LOG_FATA(logger, mess) logger->log((mess), LM::SV_FATAL_ERROR, __FILE__, __LINE__)
#else
   #define LOG_USER(logger, mess) logger->log((mess), LM::SV_END_USER)
   #define LOG_DEBU(logger, mess)
   #define LOG_WARN(logger, mess) logger->log((mess), LM::SV_WARNING)
   #define LOG_ERRO(logger, mess) logger->log((mess), LM::SV_ERROR)
   #define LOG_FATA(logger, mess) logger->log((mess), LM::SV_FATAL_ERROR)
#endif

#endif
