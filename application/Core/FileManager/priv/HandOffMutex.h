#pragma once

#include <atomic>

#include <QMutex>
#include <QThread>

namespace FM
{
   /**
     * A mutex held by a worker thread for long stretches, released only briefly between units of work.
     * 'QMutex' is not fair: a worker relocking right after unlocking almost always wins against the waiter
     * it has just woken, which can starve that waiter for as long as the worker keeps going.
     * Here every 'lock()' is counted as a waiter, and the worker hands the mutex over with 'yieldToWaiters()'.
     * Usable with 'QMutexLocker'.
     */
   class HandOffMutex
   {
   public:
      void lock()
      {
         ++this->waiters;
         this->mutex.lock();
         --this->waiters;
      }

      void unlock() { this->mutex.unlock(); }

      /**
        * The caller must own the mutex. It is released until every thread already waiting for it
        * has taken it, then locked again.
        */
      void yieldToWaiters()
      {
         if (this->waiters.load() == 0)
            return;

         this->mutex.unlock();
         // A waiter takes the mutex right after being woken, this only lasts for the handover.
         while (this->waiters.load() != 0)
            QThread::yieldCurrentThread();
         this->mutex.lock();
      }

      /**
        * For 'QWaitCondition::wait(..)'. Waiting releases the mutex without going through 'yieldToWaiters()'.
        */
      QMutex* native() { return &this->mutex; }

   private:
      QMutex mutex;
      std::atomic<int> waiters { 0 };
   };
}
