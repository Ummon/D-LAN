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
  
#ifndef TESTS_H
#define TESTS_H

#include <QObject>
#include <QVector>
#include <QSharedPointer>
#include <QThread>

#include <Libs/MersenneTwister.h>

#include <ILogger.h>
using namespace LM;

class ThreadLogger : public QThread
{
   static MTRand mtrand;
public:
   ThreadLogger(const QString& name, int delta);
   void run();

private:
   QSharedPointer<ILogger> logger;
   int delta;
};

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();
   void createLoggers();
   void logSomeBasicMessages();
   void logSomeMessagesWithSpecialCharacters();
   void startTheThreadLoggers();

private:
   QVector< QSharedPointer<ILogger> > loggers;
   QVector< QSharedPointer<ThreadLogger> > threadLoggers;
};

#endif
