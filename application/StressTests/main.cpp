#include <atomic>
#include <csignal>

#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QRandomGenerator>
#include <QTextStream>
#include <QTimer>

#include <google/protobuf/stubs/common.h>

#include <Common/Global.h>
#include <Common/LogManager/Builder.h>
#include <Common/LogManager/CrashHandler.h>

#include <Config.h>
#include <Log.h>
#include <StressRun.h>

#if defined(Q_OS_WIN32)
   #ifndef NOMINMAX
      #define NOMINMAX
   #endif
   #include <windows.h>
#endif

using namespace StressTests;

namespace
{
   const QString CONFIG_FILENAME("StressTests.json");
   const QString ROOT_DIRECTORY_NAME("D-LAN_StressTests");
   const QString LOG_DIRECTORY_NAME("logs");

#if defined(Q_OS_WIN32)
   const QString CORE_EXE_NAME("D-LAN.Core.exe");
#else
   const QString CORE_EXE_NAME("D-LAN.Core");
#endif

   std::atomic<bool> interrupted = false;

   void interruptHandler(int)
   {
      interrupted = true;
   }

   void printUsage(const QString& appName)
   {
      QTextStream out(stdout);
      out << "Usage: " << appName << " [<configuration file>]" << Qt::endl
          << "  Launch some D-LAN Cores and execute random actions on them during a given time." << Qt::endl
          << "  <configuration file>: A JSON file, by default '" << CONFIG_FILENAME << "' next to the executable." << Qt::endl
          << "    Each missing value takes its default value, see 'StressTests.example.json'." << Qt::endl
          << "  All data are put in '" << QDir(QDir::tempPath()).absoluteFilePath(ROOT_DIRECTORY_NAME) << "', this directory is emptied at start." << Qt::endl
          << "  Exit code: 0 if no Core crashed, 1 if at least one Core crashed, 2 if the stress run can't be started." << Qt::endl
          << "  Press Ctrl-C to stop the run before its end." << Qt::endl;
   }
}

int main(int argc, char* argv[])
{
#if defined(Q_OS_WIN32)
   // Inherited by the Cores: a crash must not wait on an error dialog.
   SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX);
#endif

   QCoreApplication app(argc, argv);
   GOOGLE_PROTOBUF_VERIFY_VERSION;

   QTextStream out(stdout);
   QTextStream err(stderr);

   const QStringList arguments = app.arguments();
   if (arguments.contains("-h") || arguments.contains("--help") || arguments.size() > 2)
   {
      printUsage(QFileInfo(arguments.first()).fileName());
      return arguments.size() > 2 ? 2 : 0;
   }

   // Configuration.
   Config config;
   const bool configGiven = arguments.size() == 2;
   const QString configPath = configGiven ? arguments[1] : QDir(app.applicationDirPath()).absoluteFilePath(CONFIG_FILENAME);
   if (configGiven || QFileInfo::exists(configPath))
   {
      const QString error = config.load(configPath);
      if (!error.isEmpty())
      {
         err << error << Qt::endl;
         return 2;
      }
      out << "Configuration read from '" << configPath << "'" << Qt::endl;
   }
   else
   {
      out << "No configuration file '" << configPath << "', the default values are used" << Qt::endl;
   }

   if (config.coreExecutable.isEmpty())
      config.coreExecutable = QDir(app.applicationDirPath()).absoluteFilePath(CORE_EXE_NAME);
   config.coreExecutable = QFileInfo(config.coreExecutable).absoluteFilePath();
   if (!QFileInfo(config.coreExecutable).isExecutable())
   {
      err << "The Core executable can't be found: '" << config.coreExecutable << "'" << Qt::endl;
      return 2;
   }

   quint64 seed = config.seed;
   while (seed == 0)
      seed = QRandomGenerator::system()->generate64();

   // The root directory is emptied at start but not at the end, to be able to investigate.
   const QString rootDirectory = QDir(QDir::tempPath()).absoluteFilePath(ROOT_DIRECTORY_NAME);
   if (QFileInfo::exists(rootDirectory) && !QDir(rootDirectory).removeRecursively())
   {
      err << "Unable to empty the directory '" << rootDirectory << "', is a previous stress run (or one of its Cores) still running?" << Qt::endl;
      return 2;
   }
   if (!QDir().mkpath(rootDirectory + "/" + LOG_DIRECTORY_NAME))
   {
      err << "Unable to create the directory '" << rootDirectory << "'" << Qt::endl;
      return 2;
   }

   // Our logs are put in '<root>/logs'. The Cores have their own data directories given by argument.
   Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, rootDirectory);
   Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, rootDirectory);
   LM::Builder::setLogDirName(LOG_DIRECTORY_NAME);
   LM::CrashHandler::install();
   Log::logger = LM::Builder::newLogger("StressTests");

   out << "Stress run: " << config.numberOfCores << " Core(s) during " << config.durationMinutes << " min, seed: " << seed << Qt::endl
       << "Directory: '" << rootDirectory << "'" << Qt::endl
       << "Logs: '" << rootDirectory << "/" << LOG_DIRECTORY_NAME << "'" << Qt::endl;

   int exitCode;
   {
      StressRun run(config, rootDirectory, seed);
      QObject::connect(&run, &StressRun::finished, &app, [](int code) { QCoreApplication::exit(code); });

      std::signal(SIGINT, interruptHandler);
      QTimer interruptTimer;
      QObject::connect(&interruptTimer, &QTimer::timeout, &run, [&run]() {
         if (interrupted.exchange(false))
            run.finish();
      });
      interruptTimer.start(200);

      QMetaObject::invokeMethod(&run, &StressRun::start, Qt::QueuedConnection);
      exitCode = app.exec();
   }

   LM::Builder::flush();
   Log::logger.clear();
   google::protobuf::ShutdownProtobufLibrary();

   return exitCode;
}
