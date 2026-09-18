#include <QtTest>
#include <QProcess>
#include <QTemporaryDir>

#include <atomic>
#include <cstdlib>
#include <thread>
#include <vector>

#include <Common/Global.h>
#include <Common/LogManager/Builder.h>

namespace
{
   QByteArray readLog(const QString& directory)
   {
      const QDir logDir(directory + "/log");
      const auto files = logDir.entryList({ "*.log" }, QDir::Files);
      if (files.size() != 1)
         return {};
      QFile file(logDir.filePath(files.first()));
      if (!file.open(QIODevice::ReadOnly))
         return {};
      return file.readAll();
   }

   void configure(const QString& directory)
   {
      Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, directory);
      Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, directory);
      LM::Builder::setLogDirName("log");
   }

   void lateLog()
   {
      LM::Builder::newLogger("late-static")->log("late-static-entry", LM::SV_END_USER);
   }

   int childMain(int argc, char** argv, const QString& directory, const QString& mode)
   {
      // Register before the logger's exit callback so this runs after shutdown.
      if (mode == "late-static")
         std::atexit(lateLog);
      const auto logger = LM::Builder::newLogger("child");
      if (mode == "no-app")
      {
         configure(directory);
         return logger->log("buffered-before-exit", LM::SV_END_USER) ? 0 : 1;
      }
      {
         QCoreApplication app(argc, argv);
         configure(directory);
         if (!logger->log("buffered-before-exit", LM::SV_END_USER))
            return 1;
         if (mode == "fatal")
         {
            if (!logger->log("fatal-entry", LM::SV_FATAL_ERROR))
               return 2;
            std::_Exit(0); // No cleanup: the fatal log call itself must flush.
         }
         if (mode == "exit")
            std::exit(0); // No QCoreApplication destructor.
         if (mode == "threshold")
         {
            const QString message(1024, 'x');
            for (int i = 0; i < 64; ++i)
               if (!logger->log(message, LM::SV_END_USER))
                  return 3;
            std::_Exit(0); // Completed batches must already be on disk.
         }
      }
      if (!readLog(directory).contains("buffered-before-exit"))
         return 4;
      if (mode == "late-qt")
      {
         if (!logger->log("after-qt-destruction", LM::SV_END_USER))
            return 5;
         std::_Exit(0); // Late logging must be synchronous after Qt teardown.
      }
      return 0;
   }
}

class BufferedLoggerTests : public QObject
{
   Q_OBJECT
private slots:
   void explicitFlushPreservesTextAndImmediateHooks()
   {
      const auto logger = LM::Builder::newLogger("buffering");
      const auto hook = LM::Builder::newLoggerHook(LM::SV_END_USER);
      QList<QString> messages;
      connect(hook.data(), &LM::ILoggerHook::newLogEntry, this,
         [&](const QSharedPointer<LM::IEntry>& entry) { messages.append(entry->getMessageWithLF()); });
      const QString text = QString::fromUtf8("Unicode: \xc3\xa9 \xe6\x97\xa5 \xf0\x9f\x98\x80\nnext line");
      QVERIFY(logger->log(text, LM::SV_END_USER));
      QCOMPARE(messages, QList<QString> { text });
      QVERIFY(LM::Builder::flush());
      const auto log = readLog(Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL));
      bool found = false;
      for (const auto& line : log.split('\n'))
      {
         if (line.contains("Unicode:"))
         {
            QCOMPARE(LM::Builder::decode(QString::fromUtf8(line))->getMessageWithLF(), text);
            found = true;
         }
      }
      QVERIFY(found);
   }

   void idleEntriesFlushWithoutAnEventLoop()
   {
      const auto logger = LM::Builder::newLogger("idle");
      QVERIFY(logger->log("idle-flush-marker", LM::SV_END_USER));
      const QString directory = Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL);
      QElapsedTimer timer;
      timer.start();
      // Deliberately do not process Qt events: the application can be busy.
      while (!readLog(directory).contains("idle-flush-marker") && timer.elapsed() < 3000)
         QThread::msleep(10);
      QVERIFY(readLog(directory).contains("idle-flush-marker"));
   }

   void concurrentWritersPreserveEveryEntry()
   {
      const auto logger = LM::Builder::newLogger("concurrent");
      std::atomic<bool> success { true };
      std::vector<std::thread> threads;
      for (int thread = 0; thread < 4; ++thread)
         threads.emplace_back([&, thread] {
            for (int i = 0; i < 200; ++i)
               if (!logger->log(QString("writer-%1-entry-%2!").arg(thread).arg(i), LM::SV_END_USER))
                  success = false;
         });
      for (auto& thread : threads)
         thread.join();
      QVERIFY(success);
      QVERIFY(LM::Builder::flush());
      const auto log = readLog(Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL));
      for (int thread = 0; thread < 4; ++thread)
      {
         qsizetype previous = -1;
         for (int i = 0; i < 200; ++i)
         {
            const auto marker = QString("writer-%1-entry-%2!").arg(thread).arg(i).toUtf8();
            QCOMPARE(log.count(marker), 1);
            const auto position = log.indexOf(marker);
            QVERIFY(position > previous);
            previous = position;
         }
      }
   }

   void recursiveHookCanLogAndFlush()
   {
      const auto logger = LM::Builder::newLogger("recursive");
      const auto hook = LM::Builder::newLoggerHook(LM::SV_WARNING);
      bool handled = false;
      connect(hook.data(), &LM::ILoggerHook::newLogEntry, this, [&](const QSharedPointer<LM::IEntry>&) {
         handled = true;
         QVERIFY(logger->log("hook-nested-message", LM::SV_END_USER));
         QVERIFY(LM::Builder::flush());
      });
      QVERIFY(logger->log("hook-outer-message", LM::SV_WARNING));
      QVERIFY(handled);
      QVERIFY(LM::Builder::flush());
      const auto log = readLog(Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL));
      QVERIFY(log.indexOf("hook-nested-message") < log.indexOf("hook-outer-message"));
   }

   void shutdownAndFatalFlush_data()
   {
      QTest::addColumn<QString>("mode");
      for (const QString& mode : { "normal", "fatal", "exit", "no-app", "late-qt", "late-static", "threshold" })
         QTest::newRow(qPrintable(mode)) << mode;
   }

   void shutdownAndFatalFlush()
   {
      QFETCH(QString, mode);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      QProcess process;
      process.start(QCoreApplication::applicationFilePath(), { "--log-child", directory.path(), mode });
      QVERIFY(process.waitForFinished(10000));
      QCOMPARE(process.exitStatus(), QProcess::NormalExit);
      QCOMPARE(process.exitCode(), 0);
      const auto log = readLog(directory.path());
      QVERIFY(log.contains("buffered-before-exit"));
      if (mode == "fatal")
         QVERIFY(log.contains("fatal-entry"));
      else if (mode == "late-qt")
         QVERIFY(log.contains("after-qt-destruction"));
      else if (mode == "late-static")
         QVERIFY(log.contains("late-static-entry"));
      else if (mode == "threshold")
         QVERIFY(log.size() >= 16 * 1024);
   }
};

int main(int argc, char** argv)
{
   qInstallMessageHandler(nullptr);
   if (argc == 4 && QByteArray(argv[1]) == "--log-child")
      return childMain(argc, argv, QString::fromLocal8Bit(argv[2]), QString::fromLocal8Bit(argv[3]));
   QTemporaryDir directory;
   if (!directory.isValid())
      return 1;
   QCoreApplication app(argc, argv);
   configure(directory.path());
   BufferedLoggerTests tests;
   return QTest::qExec(&tests, argc, argv);
}
#include "BufferedLoggerTests.moc"
