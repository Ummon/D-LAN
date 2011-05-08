#include <Common/ThreadPool.h>
using namespace Common;

#include <Common/IRunnable.h>

Thread::Thread(int lifetime) :
   runnable(0), toStop(false), active(false)

{
   this->timer.setInterval(lifetime);
   this->timer.setSingleShot(true);
   connect(&this->timer, SIGNAL(timeout()), this, SIGNAL(timeout()));
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
void Thread::setRunnable(IRunnable* runnable)
{
   this->mutex.lock();
   if (this->active)
   {
      this->mutex.unlock();
      return;
   }

   this->timer.stop();

   this->runnable = runnable;
   this->runnable->init(this);

   this->active = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();
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

      this->runnable->run();

      this->mutex.lock();
      this->active = false;
      this->mutex.unlock();

      emit runnableFininshed();
   }
}

void Thread::startTimer()
{
   this->timer.start();
   this->runnable = 0;
}

IRunnable* Thread::getRunnable() const
{
   return this->runnable;
}

ThreadPool::ThreadPool(int nbMinThread, int threadInactiveLifetime) :
   nbMinThread(nbMinThread), threadInactiveLifetime(threadInactiveLifetime)
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

void ThreadPool::run(IRunnable* runnable)
{
   Thread* thread;
   if (!this->inactiveThreads.isEmpty())
   {
      thread = this->inactiveThreads.takeLast();
   }
   else
   {
      thread = new Thread(this->threadInactiveLifetime);
      connect(thread, SIGNAL(runnableFininshed()), this, SLOT(runnableFinished()), Qt::QueuedConnection);
      connect(thread, SIGNAL(timeout()), this, SLOT(threadTimeout()));
   }
   this->activeThreads << thread;
   thread->setRunnable(runnable);
}

void ThreadPool::runnableFinished()
{
   Thread* thread = dynamic_cast<Thread*>(this->sender());

   thread->getRunnable()->finished();

   this->activeThreads.removeOne(thread);
   this->inactiveThreads << thread;
   thread->startTimer();
}

void ThreadPool::threadTimeout()
{
   Thread* thread = dynamic_cast<Thread*>(this->sender());

   if (this->activeThreads.size() + this->inactiveThreads.size() > this->nbMinThread)
   {
      this->inactiveThreads.removeOne(thread);
      delete thread;
   }
}
