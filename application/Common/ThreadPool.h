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
