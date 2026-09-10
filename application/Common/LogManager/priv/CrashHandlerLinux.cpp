#include <CrashHandler.h>

#include <QDir>
#include <QFile>

#include <Common/Global.h>
#include <priv/Logger.h>

#include <atomic>
#include <cerrno>
#include <cstdint>
#include <cstdlib>
#include <cxxabi.h>
#include <dlfcn.h>
#include <execinfo.h>
#include <fcntl.h>
#include <mutex>
#include <signal.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <time.h>
#include <ucontext.h>
#include <unistd.h>

namespace
{
   constexpr int MAX_FRAMES = 64;
   constexpr int FATAL_SIGNALS[] = {SIGSEGV, SIGBUS, SIGABRT, SIGILL, SIGFPE};
   std::once_flag installed;
   std::atomic_flag reporting = ATOMIC_FLAG_INIT;
   int reportDirectory = -1; // Kept open for the lifetime of the process.
   const char* descriptionData = nullptr;
   size_t descriptionSize = 0;

   // No allocation, formatting library or Qt calls in the initial report path.
   struct Buffer
   {
      char data[1024];
      size_t size = 0;

      void append(const char* text)
      {
         while (*text && size < sizeof(data) - 1)
            data[size++] = *text++;
         data[size] = '\0';
      }

      void number(uintptr_t value, unsigned base = 10)
      {
         char digits[3 * sizeof(value) + 1];
         size_t count = 0;
         do
         {
            digits[count++] = "0123456789abcdef"[value % base];
            value /= base;
         } while (value);
         while (count && size < sizeof(data) - 1)
            data[size++] = digits[--count];
         data[size] = '\0';
      }
   };

   void writeAll(int fd, const char* data, size_t size)
   {
      if (fd < 0)
         return;
      while (size)
      {
         const ssize_t written = ::write(fd, data, size);
         if (written < 0 && errno == EINTR)
            continue;
         if (written <= 0)
            break;
         data += written;
         size -= written;
      }
   }

   const char* signalName(int signal)
   {
      switch (signal)
      {
      case SIGSEGV: return "SIGSEGV";
      case SIGBUS: return "SIGBUS";
      case SIGABRT: return "SIGABRT";
      case SIGILL: return "SIGILL";
      case SIGFPE: return "SIGFPE";
      default: return "unknown";
      }
   }

   // Restore the default and queue the signal on this thread. Returning from the
   // handler unmasks it, retaining the signal exit status and normal core dumps.
   void terminateWithSignal(int signal)
   {
      struct sigaction action = {};
      action.sa_handler = SIG_DFL;
      sigemptyset(&action.sa_mask);
      sigaction(signal, &action, nullptr);
      if (raise(signal) != 0)
         _exit(128 + signal);
   }

   void signalHandler(int signal, siginfo_t* info, void* context)
   {
      if (reporting.test_and_set(std::memory_order_relaxed))
      {
         terminateWithSignal(signal);
         return;
      }

      timespec now = {};
      clock_gettime(CLOCK_REALTIME, &now);
      Buffer filename;
      filename.append("crash_");
      filename.number(now.tv_sec);
      filename.append("_");
      filename.number(now.tv_nsec);
      filename.append("_");
      filename.number(getpid());
      filename.append(".log");
      const int fd = reportDirectory < 0 ? -1 :
         openat(reportDirectory, filename.data, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC | O_NOFOLLOW, 0600);

      Buffer header;
      header.append("D-LAN Linux crash report\nSignal: ");
      header.append(signalName(signal));
      header.append("\nPID: ");
      header.number(getpid());
      header.append("\nTID: ");
      header.number(syscall(SYS_gettid));
      header.append("\nUnix time: ");
      header.number(now.tv_sec);
      header.append("\nSignal code: ");
      const int code = info ? info->si_code : 0;
      if (code < 0)
         header.append("-");
      header.number(code < 0 ? -static_cast<int64_t>(code) : code);
      // si_addr is meaningful only for a synchronous hardware fault.
      if (info && code > 0 && signal != SIGABRT)
      {
         header.append("\nFault address: 0x");
         header.number(reinterpret_cast<uintptr_t>(info->si_addr), 16);
      }
      const auto* machine = static_cast<const ucontext_t*>(context);
#if defined(__x86_64__)
      header.append("\nInstruction pointer: 0x");
      header.number(machine->uc_mcontext.gregs[REG_RIP], 16);
#elif defined(__i386__)
      header.append("\nInstruction pointer: 0x");
      header.number(machine->uc_mcontext.gregs[REG_EIP], 16);
#elif defined(__aarch64__)
      header.append("\nInstruction pointer: 0x");
      header.number(machine->uc_mcontext.pc, 16);
#else
      (void)machine;
#endif
      header.append("\n");
      writeAll(fd, header.data, header.size);
      writeAll(STDERR_FILENO, header.data, header.size);
      writeAll(fd, descriptionData, descriptionSize);
      writeAll(STDERR_FILENO, descriptionData, descriptionSize);

      // Save mappings before attempting unwinding, so even a damaged stack leaves
      // the fault PC and enough ASLR information for offline investigation.
      constexpr char mapsHeader[] = "\nMemory mappings (/proc/self/maps):\n";
      writeAll(fd, mapsHeader, sizeof(mapsHeader) - 1);
      const int maps = open("/proc/self/maps", O_RDONLY | O_CLOEXEC);
      if (maps >= 0)
      {
         char data[4096];
         ssize_t count;
         while ((count = read(maps, data, sizeof(data))) != 0)
         {
            if (count < 0)
            {
               if (errno == EINTR)
                  continue;
               break;
            }
            writeAll(fd, data, count);
         }
         close(maps);
      }

      constexpr char stackHeader[] = "\nStack trace (best effort; C++ names may be mangled):\n";
      writeAll(fd, stackHeader, sizeof(stackHeader) - 1);
      writeAll(STDERR_FILENO, stackHeader, sizeof(stackHeader) - 1);
      // Prewarmed at install, but glibc unwinding/symbol lookup is not guaranteed
      // async-signal-safe. Never use Qt, the logger, dladdr or demangling here.
      void* frames[MAX_FRAMES];
      const int count = backtrace(frames, MAX_FRAMES);
      if (fd >= 0)
         backtrace_symbols_fd(frames, count, fd);
      backtrace_symbols_fd(frames, count, STDERR_FILENO);
      if (fd >= 0)
         close(fd);
      terminateWithSignal(signal);
   }

}

void LM::CrashHandler::install(bool)
{
   std::call_once(installed, []
   {
      char executable[4096];
      const ssize_t length = readlink("/proc/self/exe", executable, sizeof(executable));
      // Intentionally retained, including during static destruction at shutdown.
      const auto* description = new QByteArray("Executable: " + (length > 0 ? QByteArray(executable, length) : QByteArray("unknown")) +
         "\nVersion: " + Common::Global::getVersionFull().toUtf8() + "\n");
      descriptionData = description->constData();
      descriptionSize = description->size();
      try
      {
         QDir directory(Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL));
         const QString name = Logger::getLogDirName();
         if (directory.mkpath(name))
            reportDirectory = open(QFile::encodeName(directory.filePath(name)).constData(), O_RDONLY | O_DIRECTORY | O_CLOEXEC);
      }
      catch (const Common::Global::UnableToGetFolder&)
      {
         // stderr is still available when the data directory cannot be created.
      }

      void* frames[MAX_FRAMES];
      const int count = backtrace(frames, MAX_FRAMES);
      const int nullOutput = open("/dev/null", O_WRONLY | O_CLOEXEC);
      if (nullOutput >= 0)
      {
         backtrace_symbols_fd(frames, count, nullOutput);
         close(nullOutput);
      }

      // sigaltstack is thread-local. Preserve an existing stack installed by a
      // host/runtime; otherwise keep our allocation alive until process exit.
      stack_t previous = {};
      if (sigaltstack(nullptr, &previous) == 0 && (previous.ss_flags & SS_DISABLE))
      {
         const size_t size = 128 * 1024;
         void* memory = mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
         if (memory != MAP_FAILED)
         {
            stack_t stack = {};
            stack.ss_sp = memory;
            stack.ss_size = size;
            if (sigaltstack(&stack, nullptr) != 0)
               munmap(memory, size);
         }
      }

      struct sigaction action = {};
      action.sa_sigaction = signalHandler;
      sigemptyset(&action.sa_mask);
      action.sa_flags = SA_SIGINFO | SA_ONSTACK | SA_RESETHAND;
      for (int signal : FATAL_SIGNALS)
         sigaction(signal, &action, nullptr);
   });
}

QString LM::CrashHandler::stackTrace(int framesToSkip)
{
   void* frames[MAX_FRAMES];
   const int count = backtrace(frames, MAX_FRAMES);
   // Clamp before adding one, including for INT_MAX and negative inputs.
   const int start = qBound(0, framesToSkip, MAX_FRAMES) + 1;
   QString result;
   for (int i = start; i < count; ++i)
   {
      const auto address = reinterpret_cast<uintptr_t>(frames[i]);
      Dl_info info = {};
      result += QString("#%1 0x%2 ").arg(i - start).arg(address, 0, 16);
      if (dladdr(frames[i], &info))
      {
         result += QString::fromLocal8Bit(info.dli_fname) + "+0x" +
            QString::number(address - reinterpret_cast<uintptr_t>(info.dli_fbase), 16);
         if (info.dli_sname)
         {
            int status;
            char* name = abi::__cxa_demangle(info.dli_sname, nullptr, nullptr, &status);
            result += " (" + QString::fromLocal8Bit(status == 0 ? name : info.dli_sname) + "+0x" +
               QString::number(address - reinterpret_cast<uintptr_t>(info.dli_saddr), 16) + ")";
            std::free(name);
         }
      }
      result += '\n';
   }
   return result;
}
