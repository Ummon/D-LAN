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
  
#ifndef LOGMANAGER_BUILDER_H
#define LOGMANAGER_BUILDER_H

#include <QDir>
#include <QSharedPointer>

#include <Common/LogManager/IEntry.h>
#include <Common/LogManager/ILogger.h>
#include <Common/LogManager/ILoggerHook.h>

namespace LM
{
   class Builder
   {
   public:
      static void setLogDirName(const QString& logDirName);
      static QSharedPointer<ILogger> newLogger(const QString& name);
      static QSharedPointer<ILoggerHook> newLoggerHook(Severity severities);

      static QSharedPointer<IEntry> decode(const QString& line);
      static QSharedPointer<IEntry> newEntry(const QDateTime& dateTime, Severity severity, const QString& message, const QString& name = QString(""), const QString& thread = QString(""), const QString& source = QString(""));

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

// Insert this macro on the top of a class to initialize (and delete) a logger Log::logger, see FileManager.h for example.
#define LOG_INIT_H(NAME) \
   static int logNbRef; \
   struct LogInitializer { LogInitializer() { logNbRef++; Log::logger = LM::Builder::newLogger(NAME); } ~LogInitializer() { if (--logNbRef == 0) Log::logger.clear(); } } logInitializer;

#define LOG_INIT_CPP(CLASS) int CLASS::logNbRef(0);

#endif
