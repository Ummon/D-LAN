/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#ifndef COMMON_THREADPOOL_H
#define COMMON_THREADPOOL_H

#include <QTimer>
#include <QThread>
#include <QWaitCondition>
#include <QMutex>

namespace Common
{
   class Thread;
   class IRunnable;

   class ThreadPool : public QObject
   {
      Q_OBJECT

   public:
      ThreadPool(int nbMinThread, int threadInactiveLifetime);
      ~ThreadPool();
      void run(IRunnable* runnable);

   private slots:
      void runnableFinished();
      void threadTimeout();

   private:
      const int nbMinThread;
      const int threadInactiveLifetime;

      QList<Thread*> activeThreads;
      QList<Thread*> inactiveThreads;
   };

   class Thread : public QThread
   {
      Q_OBJECT
   public:
      Thread(int lifetime);
      ~Thread();
      void setRunnable(IRunnable* runnable);
      void startTimer();
      IRunnable* getRunnable() const;

   signals:
      void timeout();
      void runnableFininshed();

   protected:
      void run();

   private:
      IRunnable* runnable;
      QTimer timer;
      QThread* mainThread;

      mutable QWaitCondition waitCondition;
      mutable QMutex mutex;

      bool toStop;
      bool active;
   };
}

#endif
