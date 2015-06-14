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
  
#include <Common/ThreadPool.h>
using namespace Common;

#include <Common/IRunnable.h>

Thread::Thread(int lifetime, uint stackSize) :
   toStop(false), active(false)
{
   if (stackSize)
      this->setStackSize(stackSize);
   this->timer.setInterval(lifetime);
   this->timer.setSingleShot(true);
   connect(&this->timer, QTimer::timeout, this, timeout);
   this->start();
}

Thread::~Thread()
{
   this->mutex.lock();
   this->toStop = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();

   this->wait();
}

/**
  * Set a runnable object and run it.
  */
void Thread::setRunnable(QWeakPointer<IRunnable> runnable)
{
   this->mutex.lock();
   if (this->active)
   {
      this->mutex.unlock();
      return;
   }

   QSharedPointer<IRunnable> runnableStrongRef = runnable.toStrongRef();
   if (runnableStrongRef.isNull())
   {
      this->mutex.unlock();
      return;
   }

   this->timer.stop();

   this->runnable = runnable;
   runnableStrongRef->init(this);

   this->active = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();
}

/**
  * If a runnable object is running this method will wait until the object 'run()' method is terminated.
  */
void Thread::waitRunnableFinished()
{
   QMutexLocker locker(&this->mutex);
   if (this->active)
      this->waitCondition.wait(&this->mutex);
}

void Thread::run()
{
   forever
   {
      this->mutex.lock();
      if (!this->active && !this->toStop)
         this->waitCondition.wait(&this->mutex);
      if (this->toStop)
         return;
      this->mutex.unlock();

      QSharedPointer<IRunnable> runnableSharedPointer = this->runnable.toStrongRef();
      if (runnableSharedPointer)
         runnableSharedPointer->run();

      this->mutex.lock();
      this->active = false;
      this->waitCondition.wakeAll();
      this->mutex.unlock();

      emit runnableFinished();
   }
}

void Thread::startTimer()
{
   this->timer.start();
   this->runnable.clear();
}

QWeakPointer<IRunnable> Thread::getRunnable() const
{
   return this->runnable;
}

/**
  * @class Common::ThreadPool
  *
  * A ThreadPool object can run runnable objects (see 'Common::IRunnable'), each thread dedicated to a runnable object will be created if needed.
  * At the begining there is no thread, when the first runnable object is given to the method 'run(..)' the first thread is created.
  * After the task of the runnable object is completed the thread will become inactive and can be reused by another runnable object for
  * a given period ('threadInactiveLifetime'). If the thread is not reused after this period and there is more thread than 'nbMinThread'
  * the thread is deleted.
  */

/**
  * Default life time: 1 min.
  */
ThreadPool::ThreadPool(int nbMinThread, int threadInactiveLifetime) :
   nbMinThread(nbMinThread), threadInactiveLifetime(threadInactiveLifetime), stackSize(0)
{
}

/**
  * Will not stop nor delete the runnable objects still running, it should be manually before deleting a thread pool.
  */
ThreadPool::~ThreadPool()
{
   foreach (Thread* thread, this->activeThreads + this->inactiveThreads)
      delete thread;
}

void ThreadPool::setStackSize(uint stackSize)
{
   this->stackSize = stackSize;
}

/**
  * @param runnable A QWeakPointer is needed to know if the object is deleted.
  */
void ThreadPool::run(QWeakPointer<IRunnable> runnable)
{
   Thread* thread;
   if (!this->inactiveThreads.isEmpty())
   {
      thread = this->inactiveThreads.takeLast();
   }
   else
   {
      thread = new Thread(this->threadInactiveLifetime, this->stackSize);
      connect(thread, Thread::runnableFinished, this, runnableFinished, Qt::QueuedConnection);
      connect(thread, Thread::timeout, this, threadTimeout);
   }
   this->activeThreads << thread;
   thread->setRunnable(runnable);
}

/**
  * Wait until the given runnable object is terminated.
  * Do not wait if the runnable object isn't running.
  */
void ThreadPool::wait(QWeakPointer<IRunnable> runnable)
{
   for (QListIterator<Thread*> i(this->activeThreads); i.hasNext();)
   {
      Thread* t = i.next();
      if (t->getRunnable() == runnable)
      {
         t->waitRunnableFinished();
         break;
      }
   }
}

void ThreadPool::runnableFinished()
{
   Thread* thread = static_cast<Thread*>(this->sender());

   // The runnable object may have been deleted right after the call to 'run()'.
   QSharedPointer<IRunnable> runnableSharedPointer = thread->getRunnable().toStrongRef();
   if (!runnableSharedPointer.isNull())
      runnableSharedPointer->finished();

   this->activeThreads.removeOne(thread);
   this->inactiveThreads << thread;
   thread->startTimer();
}

void ThreadPool::threadTimeout()
{
   Thread* thread = static_cast<Thread*>(this->sender());

   if (this->activeThreads.size() + this->inactiveThreads.size() > this->nbMinThread)
   {
      this->inactiveThreads.removeOne(thread);
      delete thread;
   }
}
