#pragma once

#include <atomic>
#include <memory>
#include <vector>

#include <QObject>
#include <QThread>
#include <QTimer>
#include <QElapsedTimer>
#include <QStringList>

#include <Config.h>
#include <DiskBudget.h>
#include <CoreSupervisor.h>

namespace StressTests
{
   /**
     * Runs one CoreSupervisor per Core, each in its own thread, during the configured duration.
     * The run fails if at least one Core crashed.
     */
   class StressRun : public QObject
   {
      Q_OBJECT
   public:
      StressRun(const Config& config, const QString& rootDirectory, quint64 seed);
      ~StressRun() override;

   public slots:
      void start();

      /**
        * Stop all the Cores then emit 'finished(..)'. Called at the end of the duration or to interrupt the run.
        */
      void finish();

   signals:
      void finished(int exitCode);

   private:
      void supervisorCreated(int number);
      void supervisorFailure(int number, const QString& description);
      void supervisorStopped(int number);
      void measureDiskUsage();
      void logProgress();
      void report();

      struct Worker
      {
         std::unique_ptr<QThread> thread;
         std::atomic<CoreSupervisor*> supervisor = nullptr; ///< Created and destroyed in 'thread'.
         bool stopped = false;
      };

      const Config config;
      const QString rootDirectory;
      const quint64 seed;

      DiskBudget diskBudget;
      std::vector<std::unique_ptr<Worker>> workers;

      std::atomic<bool> finishing = false;
      QElapsedTimer elapsedTimer;
      QTimer durationTimer;
      QTimer measureTimer;
      QTimer progressTimer;

      QStringList failures;
   };
}
