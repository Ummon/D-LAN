#include <StressRun.h>
using namespace StressTests;

#include <algorithm>
#include <chrono>

#include <QRandomGenerator>
#include <QTextStream>

#include <Log.h>

namespace
{
   const int MEASURE_PERIOD = 2000; // [ms].
   const int PROGRESS_PERIOD = 60000; // [ms].

   QString bytesToGiB(qint64 bytes)
   {
      return QString::number(static_cast<double>(bytes) / (1024.0 * 1024.0 * 1024.0), 'f', 2);
   }

   void print(const QString& message)
   {
      QTextStream out(stdout);
      out << message << Qt::endl;
   }
}

StressRun::StressRun(const Config& config, const QString& rootDirectory, quint64 seed) :
   config(config),
   rootDirectory(rootDirectory),
   seed(seed),
   diskBudget(static_cast<qint64>(config.maxTotalSizeGB * 1024 * 1024 * 1024), config.numberOfCores),
   searchCoordinator(config.numberOfCores)
{
   this->durationTimer.setSingleShot(true);
   connect(&this->durationTimer, &QTimer::timeout, this, &StressRun::finish);

   connect(&this->measureTimer, &QTimer::timeout, this, &StressRun::measureDiskUsage);
   connect(&this->progressTimer, &QTimer::timeout, this, &StressRun::logProgress);
}

StressRun::~StressRun()
{
   // Only if the run hasn't finished normally: the supervisors are deleted with their thread and they kill their Core.
   for (auto& worker : this->workers)
   {
      worker->thread->quit();
      worker->thread->wait();
   }
}

void StressRun::start()
{
   this->elapsedTimer.start();

   L_USER(QString("Stress run started, seed: %1, directory: '%2'").arg(this->seed).arg(this->rootDirectory));
   L_USER(QString("Configuration: %1").arg(this->config.toString()));

   // The non-stoppable Cores are chosen randomly, reproducible with the same seed.
   {
      QList<int> cores;
      for (int i = 0; i < this->config.numberOfCores; i++)
         cores << i;
      QRandomGenerator random(this->seed);
      std::shuffle(cores.begin(), cores.end(), random);
      this->nonStoppableCores = cores.mid(0, qRound(this->config.numberOfCores * this->config.nonStoppableCoresRatio));
      std::sort(this->nonStoppableCores.begin(), this->nonStoppableCores.end());
   }
   L_USER(QString("Non-stoppable Cores: %1").arg(this->nonStoppableCoresToStr()));

   this->measureDiskUsage();
   this->measureTimer.start(MEASURE_PERIOD);
   this->progressTimer.start(PROGRESS_PERIOD);
   this->durationTimer.start(std::chrono::milliseconds(static_cast<qint64>(this->config.durationMinutes * 60 * 1000)));

   for (int i = 0; i < this->config.numberOfCores; i++)
   {
      auto worker = std::make_unique<Worker>();
      worker->thread = std::make_unique<QThread>();
      Worker* const workerPtr = worker.get();
      const QString directory = this->rootDirectory + "/" + QString::number(i);
      const quint64 supervisorSeed = this->seed + static_cast<quint64>(i);
      const bool stoppable = !this->nonStoppableCores.contains(i);

      // No context object: the lambda is executed in the new thread, the supervisor then belongs to this thread.
      connect(worker->thread.get(), &QThread::started, [this, workerPtr, i, stoppable, directory, supervisorSeed]() {
         auto supervisor = new CoreSupervisor(i, stoppable, this->config, this->diskBudget, this->searchCoordinator, directory, supervisorSeed);
         connect(supervisor, &CoreSupervisor::failure, this, &StressRun::supervisorFailure);
         connect(supervisor, &CoreSupervisor::searchChecked, this, &StressRun::searchChecked);
         connect(supervisor, &CoreSupervisor::stopped, this, &StressRun::supervisorStopped);
         connect(QThread::currentThread(), &QThread::finished, supervisor, &QObject::deleteLater);
         workerPtr->supervisor = supervisor;

         supervisor->start();

         // 'finish()' may have been called before the supervisor exists.
         if (this->finishing)
            supervisor->stop();
      });

      worker->thread->start();
      this->workers.push_back(std::move(worker));
   }
}

void StressRun::finish()
{
   if (this->finishing.exchange(true))
      return;

   L_USER(QString("Stopping all the Cores after %1 min . . .").arg(this->elapsedTimer.elapsed() / 60000.0, 0, 'f', 1));
   print("Stopping all the Cores . . .");

   this->durationTimer.stop();

   for (auto& worker : this->workers)
      if (CoreSupervisor* supervisor = worker->supervisor.load())
         QMetaObject::invokeMethod(supervisor, &CoreSupervisor::stop, Qt::QueuedConnection);
}

void StressRun::supervisorFailure(int number, const QString& description)
{
   const QString failure = QString("Core %1: %2").arg(number).arg(description);
   this->failures << failure;
   L_ERRO(QString("Failure #%1: %2").arg(this->failures.size()).arg(failure));
   print(QString("FAILURE: %1").arg(failure));
}

void StressRun::supervisorStopped(int number)
{
   Worker& worker = *this->workers[number];
   worker.stopped = true;
   worker.supervisor = nullptr; // It will be deleted by its thread.
   worker.thread->quit();

   for (const auto& w : this->workers)
      if (!w->stopped)
         return;

   for (const auto& w : this->workers)
      w->thread->wait();
   this->workers.clear();

   this->measureTimer.stop();
   this->progressTimer.stop();
   this->measureDiskUsage();

   this->report();
   emit finished(this->failures.isEmpty() ? 0 : 1);
}

void StressRun::searchChecked(int number, bool ok)
{
   Q_UNUSED(number);
   this->nbSearches++;
   if (!ok)
      this->nbSearchMismatches++;
}

QString StressRun::nonStoppableCoresToStr() const
{
   if (this->nonStoppableCores.isEmpty())
      return "none";

   QStringList cores;
   for (int core : this->nonStoppableCores)
      cores << QString::number(core);
   return cores.join(", ");
}

void StressRun::measureDiskUsage()
{
   const int generation = this->diskBudget.beginMeasure();
   const qint64 measured = DiskBudget::measure(this->rootDirectory);
   this->diskBudget.endMeasure(generation, measured);
}

void StressRun::logProgress()
{
   const QString progress = QString("Progress: %1 / %2 min, disk usage (estimated): %3 / %4 GiB, failures: %5, search mismatches: %6 / %7")
      .arg(this->elapsedTimer.elapsed() / 60000.0, 0, 'f', 1)
      .arg(this->config.durationMinutes)
      .arg(bytesToGiB(this->diskBudget.getUsedBytes()))
      .arg(bytesToGiB(this->diskBudget.getMaxBytes()))
      .arg(this->failures.size())
      .arg(this->nbSearchMismatches)
      .arg(this->nbSearches);
   L_USER(progress);
   print(progress);
}

void StressRun::report()
{
   QStringList lines;
   lines << "===== Stress run report =====";
   lines << QString("Duration: %1 min, number of Cores: %2, seed: %3").arg(this->elapsedTimer.elapsed() / 60000.0, 0, 'f', 1).arg(this->config.numberOfCores).arg(this->seed);
   lines << QString("Non-stoppable Cores: %1").arg(this->nonStoppableCoresToStr());
   lines << QString("Disk usage (estimated): %1 / %2 GiB").arg(bytesToGiB(this->diskBudget.getUsedBytes()), bytesToGiB(this->diskBudget.getMaxBytes()));
   lines << QString("Directory: '%1'").arg(this->rootDirectory);
   lines << QString("Searches checked: %1, mismatches: %2 (not counted as failures, see the warnings \"Search mismatch\" in the log)").arg(this->nbSearches).arg(this->nbSearchMismatches);
   if (this->failures.isEmpty())
   {
      lines << "Result: SUCCESS, no failure";
   }
   else
   {
      lines << QString("Result: FAILURE, %1 failure(s):").arg(this->failures.size());
      for (const QString& failure : this->failures)
         lines << QString(" - %1").arg(failure);
   }

   for (const QString& line : lines)
   {
      L_USER(line);
      print(line);
   }
}
