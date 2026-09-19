#include <CrashHandler.h>

#include <QDir>
#include <QFile>

#include <Common/Global.h>
#include <priv/Logger.h>

#include <atomic>
#include <cerrno>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <cxxabi.h>
#include <dlfcn.h>
#include <execinfo.h>
#include <fcntl.h>
#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <mach-o/dyld.h>
#include <mach-o/loader.h>
#include <mutex>
#include <ptrauth.h>
#include <signal.h>
#include <sys/mman.h>
#include <time.h>
#include <sys/ucontext.h>
#include <unistd.h>

namespace
{
   constexpr int MAX_FRAMES = 64;
   constexpr int FATAL_SIGNALS[] = {SIGSEGV, SIGBUS, SIGABRT, SIGILL, SIGFPE, SIGTRAP};
   std::once_flag installed;
   std::atomic_flag reporting = ATOMIC_FLAG_INIT;
   int reportDirectory = -1; // Retained until process exit, including static destruction.
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
      case SIGTRAP: return "SIGTRAP";
      default: return "unknown";
      }
   }


   void output(int fd, const char* data, size_t size)
   {
      writeAll(fd, data, size);
      writeAll(STDERR_FILENO, data, size);
   }

   bool hardwareFault(int signal, const ucontext_t* context)
   {
      // Darwin supplies hardware-style si_code values even for raise(SIGBUS)
      // and raise(SIGILL). Use the saved CPU exception state instead.
      if (!context || !context->uc_mcontext)
         return false;
#if defined(__aarch64__)
      const unsigned ec = context->uc_mcontext->__es.__esr >> 26;
      switch (signal)
      {
      case SIGSEGV: case SIGBUS:
         return ec == 0x20 || ec == 0x21 || ec == 0x22 || ec == 0x24 || ec == 0x25 || ec == 0x26;
      case SIGILL: return ec == 0; // Undefined instruction.
      case SIGFPE: return ec == 0x2c; // Floating point exception.
      case SIGTRAP: return ec == 0x30 || ec == 0x31 || ec == 0x32 || ec == 0x33 || ec == 0x34 || ec == 0x35 || ec == 0x3c;
      default: return false;
      }
#elif defined(__x86_64__)
      const unsigned trap = context->uc_mcontext->__es.__trapno;
      switch (signal)
      {
      case SIGSEGV: case SIGBUS: return trap == 13 || trap == 14; // Protection/page fault.
      case SIGILL: return trap == 6; // Invalid opcode.
      case SIGFPE: return trap == 0 || trap == 16 || trap == 19;
      case SIGTRAP: return trap == 3; // INT3 (single-step traps are re-raised).
      default: return false;
      }
#else
      return false;
#endif
   }

   void finishSignal(int signal, ucontext_t* context)
   {
      struct sigaction action = {};
      action.sa_handler = SIG_DFL;
      sigemptyset(&action.sa_mask);
      sigaction(signal, &action, nullptr);
      // Return to the original faulting instruction to preserve Apple's exception
      // context. Software signals must be queued again so they still terminate.
      const bool repeat = !hardwareFault(signal, context);
#if defined(__x86_64__)
      // INT3 resumes after its one-byte instruction; execute it again so the
      // system report retains the breakpoint location and exception type.
      if (!repeat && signal == SIGTRAP)
         --context->uc_mcontext->__ss.__rip;
#endif
      if (repeat && raise(signal) != 0)
         _exit(128 + signal);
   }

   uintptr_t codeAddress(uintptr_t address)
   {
      // Strip PAC from system-library return addresses in ordinary arm64 builds
      // too, where the ptrauth.h macro would otherwise be a no-op. XPACLRI is
      // encoded as a backwards-compatible hint on CPUs without PAC support.
#if defined(__aarch64__) && !__has_feature(ptrauth_intrinsics)
      register uintptr_t lr asm("x30") = address;
      asm("hint #7" : "+r"(lr));
      return lr;
#else
      return reinterpret_cast<uintptr_t>(ptrauth_strip(reinterpret_cast<void*>(address), ptrauth_key_return_address));
#endif
   }

   void frameLine(int fd, int index, uintptr_t address)
   {
      Buffer line;
      line.append("#");
      line.number(index);
      line.append(" 0x");
      line.number(codeAddress(address), 16);
      line.append("\n");
      output(fd, line.data, line.size);
   }

   void signalHandler(int signal, siginfo_t* info, void* context)
   {
      if (reporting.test_and_set(std::memory_order_relaxed))
      {
         finishSignal(signal, static_cast<ucontext_t*>(context));
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

      uintptr_t pc = 0, fp = 0, sp = 0, lr = 0;
      const auto* machine = static_cast<const ucontext_t*>(context);
      if (machine && machine->uc_mcontext)
      {
#if defined(__aarch64__)
         pc = __darwin_arm_thread_state64_get_pc(machine->uc_mcontext->__ss);
         fp = __darwin_arm_thread_state64_get_fp(machine->uc_mcontext->__ss);
         sp = __darwin_arm_thread_state64_get_sp(machine->uc_mcontext->__ss);
         lr = __darwin_arm_thread_state64_get_lr(machine->uc_mcontext->__ss);
#elif defined(__x86_64__)
         pc = machine->uc_mcontext->__ss.__rip;
         fp = machine->uc_mcontext->__ss.__rbp;
         sp = machine->uc_mcontext->__ss.__rsp;
#endif
      }
      Buffer header;
      header.append("D-LAN macOS crash report\nSignal: ");
      header.append(signalName(signal));
      header.append("\nPID: ");
      header.number(getpid());
      header.append("\nMach thread: ");
      const mach_port_t thread = mach_thread_self();
      header.number(thread);
      mach_port_deallocate(mach_task_self(), thread);
      header.append("\nUnix time: ");
      header.number(now.tv_sec);
      header.append("\nSignal code: ");
      const int code = info ? info->si_code : 0;
      if (code < 0)
         header.append("-");
      header.number(code < 0 ? -static_cast<int64_t>(code) : code);
      if (info && hardwareFault(signal, machine))
      {
         header.append("\nFault address: 0x");
         header.number(reinterpret_cast<uintptr_t>(info->si_addr), 16);
      }
      header.append("\nInstruction pointer: 0x");
      header.number(codeAddress(pc), 16);
      header.append("\nStack pointer: 0x");
      header.number(sp, 16);
      header.append("\nFrame pointer: 0x");
      header.number(fp, 16);
      if (machine && machine->uc_mcontext)
      {
#if defined(__aarch64__)
         header.append("\nException syndrome: 0x");
         header.number(machine->uc_mcontext->__es.__esr, 16);
         header.append("\nException number: ");
         header.number(machine->uc_mcontext->__es.__exception);
#elif defined(__x86_64__)
         header.append("\nTrap number: ");
         header.number(machine->uc_mcontext->__es.__trapno);
#endif
      }
      if (lr)
      {
         header.append("\nLink register: 0x");
         header.number(codeAddress(lr), 16);
      }
      header.append("\n");
      output(fd, header.data, header.size);
      output(fd, descriptionData, descriptionSize);

      constexpr char stackHeader[] = "\nStack trace (raw addresses, best effort; #0 is the fault PC):\n";
      output(fd, stackHeader, sizeof(stackHeader) - 1);
      frameLine(fd, 0, pc);
      // Start from the interrupted context, not from the signal handler. Never
      // call backtrace, dladdr, Qt, malloc or the logger here. Mach reads fail
      // cleanly on unreadable stack memory instead of causing another fault.
      for (int i = 1; i < MAX_FRAMES && fp >= sp && fp != 0 && fp % sizeof(uintptr_t) == 0; ++i)
      {
         struct { uintptr_t previous; uintptr_t address; } frame = {};
         mach_vm_size_t copied = 0;
         if (mach_vm_read_overwrite(mach_task_self(), fp, sizeof(frame),
               reinterpret_cast<mach_vm_address_t>(&frame), &copied) != KERN_SUCCESS || copied != sizeof(frame))
            break;
         if (!frame.address)
            break;
         frameLine(fd, i, frame.address);
         if (frame.previous <= fp)
            break;
         fp = frame.previous;
      }
      if (fd >= 0)
         close(fd);
      finishSignal(signal, static_cast<ucontext_t*>(context));
   }

   QByteArray imageDescription()
   {
      // Snapshot before worker threads start. No dyld locks or mutable image
      // lists are touched by the handler. Libraries loaded later are not listed.
      QByteArray result("\nBinary images at install (load address, UUID, path):\n");
      const uint32_t count = _dyld_image_count();
      for (uint32_t i = 0; i < count; ++i)
      {
         const mach_header* header = _dyld_get_image_header(i);
         const char* name = _dyld_get_image_name(i);
         if (!header || header->magic != MH_MAGIC_64 || !name)
            continue;
         QByteArray uuid("unknown");
         const auto* command = reinterpret_cast<const load_command*>(reinterpret_cast<const mach_header_64*>(header) + 1);
         for (uint32_t j = 0; j < header->ncmds; ++j)
         {
            if (command->cmd == LC_UUID)
            {
               const auto* id = reinterpret_cast<const uuid_command*>(command);
               uuid = QByteArray(reinterpret_cast<const char*>(id->uuid), sizeof(id->uuid)).toHex();
               break;
            }
            command = reinterpret_cast<const load_command*>(reinterpret_cast<const char*>(command) + command->cmdsize);
         }
         result += "0x" + QByteArray::number(reinterpret_cast<quintptr>(header), 16) + " " + uuid + " " + name + "\n";
      }
      return result;
   }
}

void LM::CrashHandler::install(bool)
{
   std::call_once(installed, []
   {
      uint32_t size = 0;
      _NSGetExecutablePath(nullptr, &size);
      QByteArray executable(size, '\0');
      if (_NSGetExecutablePath(executable.data(), &size) != 0)
         executable = "unknown";
      else
         executable.resize(std::strlen(executable.constData()));
#if defined(__aarch64__)
      const char* architecture = "arm64";
#elif defined(__x86_64__)
      const char* architecture = "x86_64";
#else
      const char* architecture = "unknown";
#endif
      // Intentionally retained through static destruction.
      const auto* description = new QByteArray("Executable: " + executable +
         "\nVersion: " + Common::Global::getVersionFull().toUtf8() +
         "\nArchitecture: " + architecture + "\n" + imageDescription());
      descriptionData = description->constData();
      descriptionSize = description->size();
      try
      {
         QDir directory(Common::Global::getLogFolder());
         const QString name = Logger::getLogDirName();
         if (directory.mkpath(name))
            reportDirectory = open(QFile::encodeName(directory.filePath(name)).constData(), O_RDONLY | O_DIRECTORY | O_CLOEXEC);
      }
      catch (const Common::Global::UnableToGetFolder&)
      {
         // stderr is still available when the data directory cannot be created.
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
