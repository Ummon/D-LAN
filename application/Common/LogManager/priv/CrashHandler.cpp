/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <CrashHandler.h>
using namespace LM;

#include <QDir>
#include <QSharedPointer>

#include <Common/Global.h>

#include <Builder.h>
#include <ILogger.h>
#include <priv/Logger.h>

/**
  * @class LM::CrashHandler
  *
  * Windows implementation, based on 'SetUnhandledExceptionFilter(..)' + the CRT hooks which
  * bypass it (signals, 'std::terminate', pure virtual calls, invalid CRT parameters).
  *
  * The stack is walked with 'StackWalk64(..)' and symbolized with DbgHelp. DbgHelp only reads
  * PDB/CodeView information: with llvm-mingw the executables must therefore be built with
  * '-gcodeview' and linked with '-Wl,--pdb=<file>' (see the top-level CMakeLists.txt),
  * otherwise only 'module+0xRVA' is printed. That form remains usable offline:
  *    llvm-addr2line -e D-LAN.Core.exe -f -C 0x<RVA>
  *
  * Everything the handler needs (output directory, logger, symbol tables) is resolved during
  * 'install()': once the process is dying we only call 'CreateFileW(..)'/'WriteFile(..)' and
  * DbgHelp, and we never touch the (possibly smashed) stack for our own buffers.
  */

#ifdef Q_OS_WIN

#ifndef WIN32_LEAN_AND_MEAN
   #define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
   #define NOMINMAX
#endif
#include <windows.h>
#include <dbghelp.h>

#include <atomic>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <csignal>
#include <cstring>
#include <exception>
#include <typeinfo>

namespace
{
   constexpr int MAX_FRAMES = 62;
   constexpr int REPORT_CAPACITY = 64 * 1024;

   std::atomic<bool> installed(false);
   std::atomic<bool> reporting(false); ///< Re-entrancy guard: a crash inside the handler must not loop.

   bool miniDumpEnabled = true;
   bool symbolsReady = false;

   wchar_t crashDir[2 * MAX_PATH] = L"";  ///< Resolved by 'install()', no allocation needed afterwards.
   char processName[MAX_PATH] = "?";
   char processVersion[128] = "?";

   QSharedPointer<LM::ILogger> crashLogger; ///< Built by 'install()' so the handler doesn't have to allocate one.

   /**
     * A fixed size text buffer. Instantiated as a static so a corrupted stack doesn't prevent
     * the report from being built.
     */
   struct Buffer
   {
      char data[REPORT_CAPACITY];
      int size;

      void clear() { this->size = 0; this->data[0] = '\0'; }

      void append(const char* str)
      {
         while (*str && this->size < REPORT_CAPACITY - 1)
            this->data[this->size++] = *str++;
         this->data[this->size] = '\0';
      }

      void appendf(const char* format, ...)
      {
         const int remaining = REPORT_CAPACITY - this->size;
         if (remaining <= 1)
            return;

         va_list args;
         va_start(args, format);
         const int n = std::vsnprintf(this->data + this->size, remaining, format, args);
         va_end(args);

         if (n > 0)
            this->size += n < remaining ? n : remaining - 1;
         this->data[this->size] = '\0';
      }
   };

   Buffer reportBuffer;

   void ensureSymbols()
   {
      if (symbolsReady)
         return;

      SymSetOptions(SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES | SYMOPT_FAIL_CRITICAL_ERRORS);
      symbolsReady = SymInitialize(GetCurrentProcess(), nullptr, TRUE) != FALSE;
   }

   /**
     * 'context' is taken by value on purpose: 'StackWalk64(..)' modifies it.
     */
   void walkStack(Buffer& out, CONTEXT context, int framesToSkip)
   {
      ensureSymbols();

      STACKFRAME64 frame;
      ZeroMemory(&frame, sizeof(frame));

#if defined(__x86_64__) || defined(_M_X64)
      const DWORD machine = IMAGE_FILE_MACHINE_AMD64;
      frame.AddrPC.Offset = context.Rip;
      frame.AddrFrame.Offset = context.Rbp;
      frame.AddrStack.Offset = context.Rsp;
#elif defined(__i386__) || defined(_M_IX86)
      const DWORD machine = IMAGE_FILE_MACHINE_I386;
      frame.AddrPC.Offset = context.Eip;
      frame.AddrFrame.Offset = context.Ebp;
      frame.AddrStack.Offset = context.Esp;
#elif defined(__aarch64__) || defined(_M_ARM64)
      const DWORD machine = IMAGE_FILE_MACHINE_ARM64;
      frame.AddrPC.Offset = context.Pc;
      frame.AddrFrame.Offset = context.Fp;
      frame.AddrStack.Offset = context.Sp;
#else
   #error "LM::CrashHandler: unsupported architecture."
#endif
      frame.AddrPC.Mode = AddrModeFlat;
      frame.AddrFrame.Mode = AddrModeFlat;
      frame.AddrStack.Mode = AddrModeFlat;

      const HANDLE process = GetCurrentProcess();
      const HANDLE thread = GetCurrentThread();

      // 'SYMBOL_INFO::Name' is a variable length array, hence the oversized buffer.
      alignas(SYMBOL_INFO) char symbolStorage[sizeof(SYMBOL_INFO) + MAX_SYM_NAME];
      SYMBOL_INFO* const symbol = reinterpret_cast<SYMBOL_INFO*>(symbolStorage);

      int printed = 0;
      for (int i = 0; i < MAX_FRAMES + framesToSkip; i++)
      {
         if (!StackWalk64(machine, process, thread, &frame, &context, nullptr, SymFunctionTableAccess64, SymGetModuleBase64, nullptr))
            break;

         if (frame.AddrPC.Offset == 0)
            break;

         if (i < framesToSkip)
            continue;

         const DWORD64 address = frame.AddrPC.Offset;

         // Module + RVA: always available, and enough to symbolize offline with 'llvm-addr2line'.
         const DWORD64 moduleBase = SymGetModuleBase64(process, address);
         char moduleName[MAX_PATH] = "???";
         if (moduleBase)
         {
            char modulePath[MAX_PATH];
            if (GetModuleFileNameA(reinterpret_cast<HMODULE>(moduleBase), modulePath, MAX_PATH))
            {
               const char* const separator = std::strrchr(modulePath, '\\');
               const char* const baseName = separator ? separator + 1 : modulePath;
               std::snprintf(moduleName, sizeof(moduleName), "%s", baseName);
            }
         }

         out.appendf(
            "  #%-2d 0x%016llX  %s+0x%llX",
            printed++,
            static_cast<unsigned long long>(address),
            moduleName,
            static_cast<unsigned long long>(moduleBase ? address - moduleBase : 0)
         );

         ZeroMemory(symbolStorage, sizeof(symbolStorage));
         symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
         symbol->MaxNameLen = MAX_SYM_NAME;
         DWORD64 symbolOffset = 0;
         if (SymFromAddr(process, address, &symbolOffset, symbol))
            out.appendf("  %s+0x%llX", symbol->Name, static_cast<unsigned long long>(symbolOffset));

         IMAGEHLP_LINE64 line;
         ZeroMemory(&line, sizeof(line));
         line.SizeOfStruct = sizeof(IMAGEHLP_LINE64);
         DWORD lineOffset = 0;
         if (SymGetLineFromAddr64(process, address, &lineOffset, &line))
            out.appendf("  [%s:%lu]", line.FileName, line.LineNumber);

         out.append("\r\n");
      }

      if (printed == 0)
         out.append("  <no frame could be walked>\r\n");
   }

   void buildCrashFilePath(wchar_t* path, size_t pathSize, const SYSTEMTIME& time, const wchar_t* extension)
   {
      _snwprintf(
         path, pathSize, L"%scrash_%04u_%02u_%02u-%02u_%02u_%02u.%s",
         crashDir, time.wYear, time.wMonth, time.wDay, time.wHour, time.wMinute, time.wSecond, extension
      );
      path[pathSize - 1] = L'\0';
   }

   void writeFile(const wchar_t* path, const char* data, DWORD length)
   {
      const HANDLE file = CreateFileW(path, GENERIC_WRITE, FILE_SHARE_READ, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
      if (file == INVALID_HANDLE_VALUE)
         return;

      DWORD written = 0;
      WriteFile(file, data, length, &written, nullptr);
      FlushFileBuffers(file);
      CloseHandle(file);
   }

   void writeMiniDump(const wchar_t* path, EXCEPTION_POINTERS* exceptionPointers)
   {
      const HANDLE file = CreateFileW(path, GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
      if (file == INVALID_HANDLE_VALUE)
         return;

      MINIDUMP_EXCEPTION_INFORMATION information;
      information.ThreadId = GetCurrentThreadId();
      information.ExceptionPointers = exceptionPointers;
      information.ClientPointers = FALSE;

      MiniDumpWriteDump(
         GetCurrentProcess(),
         GetCurrentProcessId(),
         file,
         static_cast<MINIDUMP_TYPE>(MiniDumpWithIndirectlyReferencedMemory | MiniDumpScanMemory | MiniDumpWithThreadInfo),
         exceptionPointers ? &information : nullptr,
         nullptr,
         nullptr
      );

      CloseHandle(file);
   }

   const char* accessViolationOperation(ULONG_PTR operation)
   {
      switch (operation)
      {
      case 0: return "read from";
      case 1: return "write to";
      case 8: return "execute at";
      default: return "access";
      }
   }

   /**
     * Builds the report, writes it everywhere, and never returns twice for the same process.
     */
   void report(const char* reason, EXCEPTION_POINTERS* exceptionPointers)
   {
      bool expected = false;
      if (!reporting.compare_exchange_strong(expected, true))
         return;

      SYSTEMTIME now;
      GetLocalTime(&now);

      reportBuffer.clear();
      reportBuffer.appendf(
         "======== %s crashed on %04u-%02u-%02u %02u:%02u:%02u ========\r\n",
         processName, now.wYear, now.wMonth, now.wDay, now.wHour, now.wMinute, now.wSecond
      );
      reportBuffer.appendf("Version : %s\r\n", processVersion);
      reportBuffer.appendf("Reason  : %s\r\n", reason);
      reportBuffer.appendf("Process : %lu\r\n", GetCurrentProcessId());
      reportBuffer.appendf("Thread  : %lu\r\n", GetCurrentThreadId());

      if (exceptionPointers && exceptionPointers->ExceptionRecord)
      {
         const EXCEPTION_RECORD* const record = exceptionPointers->ExceptionRecord;
         reportBuffer.appendf("Code    : 0x%08lX\r\n", record->ExceptionCode);
         reportBuffer.appendf("Address : 0x%016llX\r\n", static_cast<unsigned long long>(reinterpret_cast<ULONG_PTR>(record->ExceptionAddress)));

         if ((record->ExceptionCode == EXCEPTION_ACCESS_VIOLATION || record->ExceptionCode == EXCEPTION_IN_PAGE_ERROR) && record->NumberParameters >= 2)
            reportBuffer.appendf(
               "Detail  : attempt to %s 0x%016llX\r\n",
               accessViolationOperation(record->ExceptionInformation[0]),
               static_cast<unsigned long long>(record->ExceptionInformation[1])
            );
      }

      reportBuffer.append("\r\nStack trace (innermost frame first):\r\n");

      // Modules loaded after 'install()' (Qt plugins, ...) must be known to DbgHelp.
      ensureSymbols();
      SymRefreshModuleList(GetCurrentProcess());

      CONTEXT context;
      if (exceptionPointers && exceptionPointers->ContextRecord)
      {
         context = *exceptionPointers->ContextRecord;
      }
      else
      {
         ZeroMemory(&context, sizeof(context));
         context.ContextFlags = CONTEXT_FULL;
         RtlCaptureContext(&context);
      }
      walkStack(reportBuffer, context, 0);

      reportBuffer.append("========================================\r\n");

      // 1) The standalone file, written with raw Win32 calls: this one works even if Qt is dead.
      if (crashDir[0])
      {
         wchar_t path[2 * MAX_PATH];
         buildCrashFilePath(path, sizeof(path) / sizeof(path[0]), now, L"log");
         writeFile(path, reportBuffer.data, static_cast<DWORD>(reportBuffer.size));

         if (miniDumpEnabled)
         {
            buildCrashFilePath(path, sizeof(path) / sizeof(path[0]), now, L"dmp");
            writeMiniDump(path, exceptionPointers);
         }
      }

      // 2) stderr, for '-e' (console) mode.
      std::fwrite(reportBuffer.data, 1, static_cast<size_t>(reportBuffer.size), stderr);
      std::fflush(stderr);

      // 3) The regular D-LAN log. 'Logger::mutex' is recursive so re-entering from the very
      //    thread which was logging when it died is safe. 'Entry' turns the '\n' into "<lf>",
      //    which keeps one entry per line and is restored by the LogViewer; the '\r' must go
      //    though, it would end up verbatim in the file.
      if (!crashLogger.isNull())
      {
         QString message = QString::fromLatin1(reportBuffer.data, reportBuffer.size);
         message.remove('\r');
         crashLogger->log(message, LM::SV_FATAL_ERROR);
      }
   }

   LONG WINAPI unhandledExceptionFilter(EXCEPTION_POINTERS* exceptionPointers)
   {
      report("Unhandled Windows exception", exceptionPointers);
      return EXCEPTION_EXECUTE_HANDLER; // Terminate the process, don't hand it over to WER.
   }

   void signalHandler(int signalNumber)
   {
      const char* reason;
      switch (signalNumber)
      {
      case SIGSEGV: reason = "SIGSEGV (segmentation fault)"; break;
      case SIGABRT: reason = "SIGABRT (abort() called, e.g. by qFatal or an unhandled C++ exception)"; break;
      case SIGFPE:  reason = "SIGFPE (arithmetic error)"; break;
      case SIGILL:  reason = "SIGILL (illegal instruction)"; break;
      default:      reason = "unknown signal"; break;
      }

      report(reason, nullptr);

      std::signal(signalNumber, SIG_DFL);
      std::raise(signalNumber);
   }

   void terminateHandler()
   {
      static char reason[1024];
      std::snprintf(reason, sizeof(reason), "std::terminate() called");

      // Best effort: name the exception which is being propagated. Note that the stack has
      // already been unwound at this point, so the trace below points at the terminate
      // machinery rather than at the 'throw' site.
      if (std::exception_ptr exception = std::current_exception())
      {
         try
         {
            std::rethrow_exception(exception);
         }
         catch (const std::exception& e)
         {
            std::snprintf(reason, sizeof(reason), "Unhandled C++ exception, type: %s, what: %s", typeid(e).name(), e.what());
         }
         catch (...)
         {
            std::snprintf(reason, sizeof(reason), "Unhandled C++ exception of unknown type");
         }
      }

      report(reason, nullptr);

      std::signal(SIGABRT, SIG_DFL);
      std::abort();
   }

#ifdef __MINGW32__
   void purecallHandler()
   {
      report("Pure virtual function call", nullptr);
      std::signal(SIGABRT, SIG_DFL);
      std::abort();
   }

   void invalidParameterHandler(const wchar_t*, const wchar_t*, const wchar_t*, unsigned int, uintptr_t)
   {
      report("Invalid parameter passed to a CRT function", nullptr);
      std::signal(SIGABRT, SIG_DFL);
      std::abort();
   }
#endif
}

void CrashHandler::install(bool writeMiniDumpToo)
{
   bool expected = false;
   if (!installed.compare_exchange_strong(expected, true))
      return;

   miniDumpEnabled = writeMiniDumpToo;

   // Resolve everything now, while the process is still healthy.
   GetModuleFileNameA(nullptr, processName, MAX_PATH);
   std::snprintf(processVersion, sizeof(processVersion), "%s", Common::Global::getVersionFull().toLatin1().constData());

   crashLogger = Builder::newLogger("CrashHandler");

   try
   {
      QDir appDir(Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL));
      const QString logDirName = Logger::getLogDirName();
      if (!appDir.exists(logDirName))
         appDir.mkdir(logDirName);

      const QString dir = QDir::toNativeSeparators(appDir.absoluteFilePath(logDirName)) + '\\';
      if (dir.size() < static_cast<int>(sizeof(crashDir) / sizeof(crashDir[0])))
         dir.toWCharArray(crashDir); // 'crashDir' is zero initialized, no need to append a terminator.
   }
   catch (const Common::Global::UnableToGetFolder&)
   {
      // No output directory: the report will still go to stderr and to the log.
   }

   ensureSymbols();

   SetUnhandledExceptionFilter(unhandledExceptionFilter);

   std::set_terminate(terminateHandler);

   std::signal(SIGSEGV, signalHandler);
   std::signal(SIGABRT, signalHandler);
   std::signal(SIGFPE, signalHandler);
   std::signal(SIGILL, signalHandler);

#ifdef __MINGW32__
   _set_purecall_handler(purecallHandler);
   _set_invalid_parameter_handler(invalidParameterHandler);
#endif

   crashLogger->log("Crash handler installed", LM::SV_DEBUG);
}

QString CrashHandler::stackTrace(int framesToSkip)
{
   CONTEXT context;
   ZeroMemory(&context, sizeof(context));
   context.ContextFlags = CONTEXT_FULL;
   RtlCaptureContext(&context);

   Buffer buffer;
   buffer.clear();
   walkStack(buffer, context, framesToSkip + 1); // +1: hide 'CrashHandler::stackTrace' itself.

   return QString::fromLatin1(buffer.data, buffer.size);
}

#else // Q_OS_WIN

void CrashHandler::install(bool)
{
   // Not implemented on this platform yet. Candidates: 'sigaction' + 'backtrace_symbols' (glibc)
   // or '<stacktrace>' once libc++ implements it.
}

QString CrashHandler::stackTrace(int)
{
   return QString();
}

#endif // Q_OS_WIN
