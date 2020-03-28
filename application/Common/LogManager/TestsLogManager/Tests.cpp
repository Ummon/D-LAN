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
  
#include <Tests.h>

#include <iostream>

#include <QRandomGenerator>
#include <QTest>
#include <QtGlobal>

#include <Builder.h>
#include <IEntry.h>
using namespace LM;

/**
  * @class Logger
  *
  * A thread to logg some random messages.
  */

ThreadLogger::ThreadLogger(const QString& name, int delta) :
   logger(Builder::newLogger(name)), delta(delta)
{
}

void ThreadLogger::run()
{
   const QString mess("A random message from thread");

   for (int i = 0; i < 100; i++)
   {
      int severity = QRandomGenerator::global()->bounded(6);
      switch (severity)
      {
      case 0 :
         LOG_WARN(this->logger, mess);
         break;
      case 1 :
         LOG_USER(this->logger, mess);
         break;
      case 2 :
         LOG_DEBU(this->logger, mess);
         break;
      case 3 :
         LOG_WARN(this->logger, mess);
         break;
      case 4 :
         LOG_ERRO(this->logger, mess);
         break;
      case 5 :
         LOG_FATA(this->logger, mess);
         break;
      }

      QTest::qWait(this->delta);
   }
}

Tests::Tests()
{
}

void Tests::initTestCase()
{
}

/**
  * Create some classic loggers and thread loggers.
  */
void Tests::createLoggers()
{
   this->loggers << Builder::newLogger("Logger 1");
   this->loggers << Builder::newLogger("Logger 2");
   this->loggers << Builder::newLogger("Logger 3");

   for (int i = 0; i < 8; i++)
      this->threadLoggers << QSharedPointer<ThreadLogger>(new ThreadLogger(QString("Thread logger %1").arg(i), 100 + i * 10));
}

void Tests::logSomeBasicMessages()
{
   for (int i = 0; i < this->loggers.count(); i++)
   {
      LOG_USER(this->loggers[i], QString("logger%1 user message").arg(i));
      LOG_DEBU(this->loggers[i], QString("logger%1 debug message").arg(i));
      LOG_WARN(this->loggers[i], QString("logger%1 warning message").arg(i));
      LOG_ERRO(this->loggers[i], QString("logger%1 error message").arg(i));
      LOG_FATA(this->loggers[i], QString("logger%1 fatal error message").arg(i));
   }
}

void Tests::logSomeMessagesWithSpecialCharacters()
{
   LOG_USER(this->loggers[0], "line return : \naaa");
   LOG_USER(this->loggers[0], "e-acute : é");
}

void Tests::logFromStdout()
{
   std::cout << "Message from stdout" << std::endl;
   QTest::qWait(1000);
}

void Tests::logFromStderr()
{
   std::cerr << "Message from stderr" << std::endl;
   QTest::qWait(1000);
}

/**
  * Start all the thread loggers.
  */
void Tests::startTheThreadLoggers()
{
   foreach (QSharedPointer<ThreadLogger> logger, this->threadLoggers)
   {
      logger->start();
      QTest::qWait(100);
   }

   foreach (QSharedPointer<ThreadLogger> logger, this->threadLoggers)
   {
      logger->wait();
   }
}
