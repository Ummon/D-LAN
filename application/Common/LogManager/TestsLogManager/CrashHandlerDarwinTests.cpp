#include <QTest>
#include <QDir>
#include <QFile>
#include <QProcess>
#include <QRegularExpression>
#include <QTemporaryDir>

#include <Common/Global.h>
#include <Builder.h>
#include <CrashHandler.h>

#include <csignal>
#include <limits>
#include <stdexcept>
#include <thread>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/resource.h>

// External symbols deliberately exercise the executable's exported symbol table.
__attribute__((noinline)) QString crashTestTraceProbe(int skip)
{
   const QString trace = LM::CrashHandler::stackTrace(skip);
   asm volatile("" ::: "memory"); // Preserve this frame in optimized builds too.
   return trace;
}

__attribute__((noinline)) void crashTestFault()
{
   void* page = mmap(nullptr, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
   if (page == MAP_FAILED)
      _exit(90);
   *static_cast<volatile char*>(page) = 1;
}

__attribute__((noinline)) void crashTestOverflow(int depth)
{
   volatile char space[8192];
   space[0] = 1;
   if (depth < 100000)
      crashTestOverflow(depth + 1);
   space[8191] = space[0]; // Prevent tail-call optimization.
}

class CrashHandlerDarwinTests : public QObject
{
   Q_OBJECT

private slots:
   void stackTrace()
   {
      const QString trace = crashTestTraceProbe(0);
      QVERIFY2(trace.contains("crashTestTraceProbe(int)"), qPrintable(trace));
      QVERIFY(trace.startsWith("#0 "));
      QVERIFY(!trace.contains("LM::CrashHandler::stackTrace"));
      QVERIFY(!crashTestTraceProbe(1).contains("crashTestTraceProbe(int)"));
      QVERIFY(crashTestTraceProbe(-1).contains("crashTestTraceProbe(int)"));
      QVERIFY(crashTestTraceProbe(std::numeric_limits<int>::max()).isEmpty());
   }

   void fatalSignal_data()
   {
      QTest::addColumn<QString>("mode");
      QTest::addColumn<int>("signal");
      QTest::addColumn<QByteArray>("signalName");
      QTest::addColumn<bool>("reportFile");
      QTest::newRow("abort") << QString("abort") << SIGABRT << QByteArray("SIGABRT") << true;
      QTest::newRow("hardware-segfault") << QString("segfault") << SIGSEGV << QByteArray("SIGSEGV") << true;
      QTest::newRow("worker-segfault") << QString("worker") << SIGSEGV << QByteArray("SIGSEGV") << true;
      QTest::newRow("main-stack-overflow") << QString("overflow") << SIGSEGV << QByteArray("SIGSEGV") << true;
      QTest::newRow("uncaught-exception") << QString("exception") << SIGABRT << QByteArray("SIGABRT") << true;
      QTest::newRow("bus-error") << QString("bus") << SIGBUS << QByteArray("SIGBUS") << true;
      QTest::newRow("illegal-instruction") << QString("ill") << SIGILL << QByteArray("SIGILL") << true;
      QTest::newRow("arithmetic-error") << QString("fpe") << SIGFPE << QByteArray("SIGFPE") << true;
      QTest::newRow("breakpoint") << QString("trap") << SIGTRAP << QByteArray("SIGTRAP") << true;
      QTest::newRow("hardware-illegal-instruction") << QString("hardware-ill") << 0 << QByteArray() << true;
      QTest::newRow("hardware-breakpoint") << QString("hardware-trap") << SIGTRAP << QByteArray("SIGTRAP") << true;
      QTest::newRow("worker-breakpoint") << QString("worker-trap") << SIGTRAP << QByteArray("SIGTRAP") << true;
      QTest::newRow("unavailable-report-directory") << QString("abort") << SIGABRT << QByteArray("SIGABRT") << false;
   }

   void fatalSignal()
   {
      QFETCH(QString, mode);
      QFETCH(int, signal);
      QFETCH(QByteArray, signalName);
      QFETCH(bool, reportFile);
      QTemporaryDir temporary;
      QVERIFY(temporary.isValid());
      if (!reportFile)
      {
         QFile obstruction(temporary.filePath("reports"));
         QVERIFY(obstruction.open(QIODevice::WriteOnly));
      }
      // Compare termination with an unhandled crash on this architecture. For
      // example, Darwin may report a page protection fault as SIGBUS.
      QProcess baseline;
      baseline.start(QCoreApplication::applicationFilePath(), {"--baseline-helper", temporary.path(), mode});
      QVERIFY(baseline.waitForStarted());
      QVERIFY(baseline.waitForFinished(10000));
      QCOMPARE(baseline.exitStatus(), QProcess::CrashExit);
      signal = baseline.exitCode();
      switch (signal)
      {
      case SIGSEGV: signalName = "SIGSEGV"; break;
      case SIGBUS: signalName = "SIGBUS"; break;
      case SIGILL: signalName = "SIGILL"; break;
      case SIGTRAP: signalName = "SIGTRAP"; break;
      default: break;
      }
      QProcess child;
      child.start(QCoreApplication::applicationFilePath(), {"--crash-helper", temporary.path(), mode});
      QVERIFY(child.waitForStarted());
      QVERIFY(child.waitForFinished(10000));
      QCOMPARE(child.exitStatus(), QProcess::CrashExit);
      QCOMPARE(child.exitCode(), signal);
      const QByteArray standardError = child.readAllStandardError();
      const QByteArray expectedSignal = "Signal: " + signalName;
      QVERIFY2(standardError.contains(expectedSignal), standardError.constData());
      QVERIFY(standardError.contains("Stack trace"));
      const QDir reports(temporary.filePath("reports"));
      const QStringList files = reports.entryList({"crash_*.log"}, QDir::Files);
      QCOMPARE(files.size(), reportFile ? 1 : 0);
      if (!reportFile)
         return;
      QFile report(reports.filePath(files.front()));
      QVERIFY(report.open(QIODevice::ReadOnly));
      const QByteArray contents = report.readAll();
      QVERIFY(contents.contains(expectedSignal));
      QVERIFY(contents.contains("Executable: "));
      QVERIFY(contents.contains("Version: "));
      QVERIFY(contents.contains("PID: "));
      QVERIFY(contents.contains("Mach thread: "));
      QVERIFY(contents.contains("D-LAN macOS crash report"));
      QVERIFY(contents.contains("Binary images at install (load address, UUID, path):"));
      QVERIFY(contents.contains("Architecture: "));
      QVERIFY(contents.contains("Stack trace"));
      QVERIFY(contents.contains("TestsCrashHandlerDarwin"));
      QVERIFY(!(report.permissions() & (QFile::ReadGroup | QFile::ReadOther | QFile::WriteGroup | QFile::WriteOther)));
      if (mode == "segfault" || mode == "worker")
      {
         QVERIFY(contents.contains("Fault address: 0x"));

#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
         QVERIFY(contents.contains("Instruction pointer: 0x"));
#endif
      }
      // Resolve the captured addresses using Apple's symbolicator, verifying
      // the trace actually identifies the fault rather than the handler.
      const QString text = QString::fromUtf8(contents);
      const QRegularExpression image("(?m)^0x([0-9a-f]+) ([0-9a-f]{32}) (.*TestsCrashHandlerDarwin)$");
      const auto binary = image.match(text);
      QVERIFY2(binary.hasMatch(), contents.constData());
      QStringList arguments = {"-o", QCoreApplication::applicationFilePath(), "-l", "0x" + binary.captured(1)};
      const QRegularExpression frames("(?m)^#[0-9]+ (0x[0-9a-f]+)$");
      auto matches = frames.globalMatch(text);
      int frameCount = 0;
      while (matches.hasNext())
      {
         arguments << matches.next().captured(1);
         ++frameCount;
      }
      QVERIFY(frameCount >= 2);
      QProcess atos;
      atos.start("/usr/bin/atos", arguments);
      QVERIFY(atos.waitForFinished(10000));
      QCOMPARE(atos.exitCode(), 0);
      const QByteArray symbols = atos.readAllStandardOutput();
      if (mode == "segfault" || mode == "worker")
         QVERIFY2(symbols.contains("crashTestFault"), symbols.constData());
      if (mode == "overflow")
         QVERIFY2(symbols.contains("crashTestOverflow"), symbols.constData());
   }
};

int main(int argc, char** argv)
{
   if (argc == 4 && (QByteArray(argv[1]) == "--crash-helper" || QByteArray(argv[1]) == "--baseline-helper"))
   {
      // Never leave core files behind, and don't let Qt Test replace our handlers.
      const rlimit limit = {0, 0};
      if (setrlimit(RLIMIT_CORE, &limit) != 0)
         return 91;
      Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, QString::fromLocal8Bit(argv[2]));
      LM::Builder::setLogDirName("reports");
      if (QByteArray(argv[1]) == "--crash-helper")
      {
         LM::CrashHandler::install(); // Also exercise installation before QCoreApplication.
         LM::CrashHandler::install(); // Must be harmless when repeated.
      }
      const QByteArray mode(argv[3]);
      if (mode == "segfault")
         crashTestFault();
      else if (mode == "worker")
         std::thread(crashTestFault).join();
      else if (mode == "overflow")
         crashTestOverflow(0);
      else if (mode == "exception")
         std::thread([] { throw std::runtime_error("controlled crash"); }).join();
      else if (mode == "bus")
         raise(SIGBUS);
      else if (mode == "ill")
         raise(SIGILL);
      else if (mode == "fpe")
         raise(SIGFPE);
      else if (mode == "trap")
         raise(SIGTRAP);
      else if (mode == "hardware-trap")
         __builtin_debugtrap();
      else if (mode == "worker-trap")
         std::thread([] { __builtin_debugtrap(); }).join();
      else if (mode == "hardware-ill")
#if defined(__aarch64__)
         asm volatile(".inst 0x00000000"); // UDF, unlike __builtin_trap's BRK.
#else
         __builtin_trap();
#endif
      else
         std::abort();
      return 92;
   }
   QCoreApplication app(argc, argv);
   CrashHandlerDarwinTests tests;
   return QTest::qExec(&tests, argc, argv);
}

#include "CrashHandlerDarwinTests.moc"
