# Development reference

See [README](../README.md) for the project overview, prerequisites and basic build instructions.
This document covers dependency builds, packaging, profiling and platform details.
Unless stated otherwise, paths are relative to the repository root.

* [Build BLAKE3](#build-blake3)
* [Build Protobuf](#build-protobuf)
* [CMake options](#cmake-options)
* [Build a release](#build-a-release)
* [Linux AppImage](#linux-appimage)
* [Profiling](#profiling)
* [macOS filesystem monitoring](#macos-filesystem-monitoring)
* [macOS settings, data, caches and logs](#macos-settings-data-caches-and-logs)
* [macOS disk space](#macos-disk-space)
* [Linux settings and data](#linux-settings-and-data)
* [macOS crash reports and stack traces](#macos-crash-reports-and-stack-traces)
* [Linux crash reports](#linux-crash-reports)

## Build BLAKE3

Run these commands from the directory `BLAKE3-1.8.7/c/`:

```nushell
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=OFF -DBUILD_TESTING=OFF -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY="../lib"
cmake --build build --parallel
```

## Build Protobuf

Run these commands from the directory `protobuf-36.1/`:

```nushell
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_STANDARD=20 -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DCMAKE_INSTALL_PREFIX="." -DCMAKE_INSTALL_LIBDIR=lib -DBUILD_SHARED_LIBS=OFF -Dprotobuf_BUILD_SHARED_LIBS=OFF -Dprotobuf_BUILD_TESTS=OFF -Dprotobuf_BUILD_PROTOC_BINARIES=ON -Dprotobuf_FORCE_FETCH_DEPENDENCIES=ON
cmake --build build --parallel
cmake --install build
```

## CMake options

| Option | Default | Purpose |
|---|---|---|
| `DLAN_BUILD_TESTS` | `ON` | Test executables (+ CTest) |
| `DLAN_BUILD_TOOLS` | `ON` | LogViewer, FileIndexer, PasswordHasher |
| `DLAN_PROFILING` | `OFF` | gprof `-pg`; requires a compatible compiler and runtime (checked at configure time) |
| `DLAN_PDB` | `ON` | Windows Clang: emit PDB symbols in non-Debug builds for crash reports and profiling |
| `DLAN_BLAKE3_ROOT` | `C:/BLAKE3-1.8.7/c` | BLAKE3 location |
| `DLAN_PROTOBUF_ROOT` | `C:/protobuf-36.1` | protobuf location |

## Build a release

The Windows release workflow uses LLVM-MinGW and requires these additional tools on `PATH`:

* Git for version information.
* [Nushell](https://www.nushell.sh/) (`nu`) for the build scripts.
* Qt LinguistTools (Qt 6.7 or newer) for translations.
* Qt's `windeployqt.exe` and Inno Setup's `iscc` for packaging.

Add your Qt `bin` directory to `PATH`, for example
`C:\Qt\6.11.2\llvm-mingw_64\bin`, adjusting the version and kit to your installation.

Run [application/build.nu](../application/build.nu) from the `application` directory. From the repository root:

```nushell
cd application
nu build.nu
```

Before packaging, check the `libwinpthread-1.dll` source path in the script's `make-setup` subcommand. It currently uses `C:/Qt/Tools/llvm-mingw1706_64/bin/libwinpthread-1.dll`; adjust it to your LLVM-MinGW installation.

Keep `DLAN_BUILD_TESTS` and `DLAN_BUILD_TOOLS` enabled for the full release workflow. The script will:

* Update translations.
* Clean and build the executables.
* Run the tests.
* Create the Windows installer.

Release executables are written to `application/build/release/output`, and installers to `application/Setups/Windows/Installations`.

## Linux AppImage

Build a Release configuration in Qt Creator, then run from `application`:

```sh
nu build.nu make-setup
```

On Linux this packages the Release directory selected by `get_release_directory`
in `build.nu`. It includes `D-LAN.GUI`, `D-LAN.Core`, translations, styles,
emoticons, and Qt dependencies (including SQLite, available Wayland platform
plugins, and GTK desktop theme integration when the Qt SDK provides `libqgtk3.so`).
Qt LinguistTools must be installed to compile translations.

The first run requires `curl` and internet access to download the official
linuxdeploy and Qt plugin continuous builds. Tools are cached under
`application/build/appimage-tools`; remove that cache to download newer versions.
Packaging uses extract-and-run mode, so the build host does not need FUSE.
The script finds qmake through the selected build's `Qt6_DIR`; set `QMAKE` to
the matching qmake executable if your Qt installation uses a different layout.

The result is `application/Setups/AppImage/D-LAN-<version>-<build-time>-<architecture>.AppImage`,
using `BUILD_TIME` from `application/Common/Version.h` (for example, `2026-09-11_22-00`).
Staging files stay under `application/build/appimage`. Supported packaging host
architectures are x86-64 and AArch64; the Release binaries must match the host.
Build on the oldest Linux environment you intend to support, and test the result
on your target distributions: bundled libraries do not remove the host glibc
requirement.

## Profiling

For CPU profiling with the Windows LLVM-MinGW kit, set `DLAN_PROFILING=OFF`, keep
`DLAN_PDB=ON`, and rebuild in Release mode. Keep the generated `.pdb` files
beside the matching executables in the build's `output` directory. Record CPU
activity with Windows Performance Recorder, then open the trace in
[Windows Performance Analyzer](https://learn.microsoft.com/en-us/windows-hardware/test/wpt/cpu-analysis)
and use the CPU Usage (Sampled) table, filtered to `D-LAN.Core.exe` or
`D-LAN.GUI.exe`. Add the absolute path to the `output` directory under
**Trace > Configure Symbol Paths > Paths**, then select **Trace > Load Symbols**
to resolve function names. The **SymCache** tab is for WPA's processed symbol
cache. Sampling does not require `-pg`.

The build embeds only the PDB filename in each executable. Older builds embedded
an absolute path with forward slashes, which WPA can treat as a single filename
and reject with `PDB Not Found` (the symbol loader may report `filename cannot
exceed 100 characters`). After rebuilding with this fix, restart the rebuilt
executable and record a new trace: existing traces retain the old PDB reference.
Keep the PDB from the exact build used for the recording.

For gprof output (`gmon.out`), use a toolchain with working gprof support and
matching Qt and third-party libraries, then enable `DLAN_PROFILING`.

## macOS filesystem monitoring

Shared directories and individually shared files are monitored with FSEvents.
Changes trigger scans of affected directories; bursts of changes are coalesced.
Dropped notifications trigger a full rescan. If a watched root or its ancestor
is moved, or its volume is unmounted, the updater falls back to periodic scanning
of the original path. Missing shared roots are removed by the existing scanner.
Symlinked directories are not traversed.

The macOS build includes `TestsDirWatcherDarwin`, registered with CTest. Its
filesystem tests require access to the system FSEvents service; a restrictive
process sandbox can prevent watch registration.

## macOS settings, data, caches and logs

`DataFolderType` keeps its two values, `ROAMING` and `LOCAL`. On macOS both
default to `~/Library/Application Support/D-LAN/`: configuration uses `ROAMING`,
while the download queue and chat history use `LOCAL`. These are persistent
files, not disposable caches.

The rebuildable hash database and its SQLite sidecar files use
`~/Library/Caches/D-LAN/`. GUI and core logs use
`~/Library/Logs/D-LAN/log_gui/` and `~/Library/Logs/D-LAN/log_core/`.
The core, GUI and tools share these locations regardless of executable name.

The core's `-r` option overrides `ROAMING`; `-l` overrides `LOCAL` and keeps the
hash database and logs within that chosen directory as well. Override
directories must already exist. Files in the old `~/.d-lan/` directory are
neither migrated nor loaded automatically.

## macOS disk space

Disk-space checks use the target volume's available allocation blocks, excluding
reserved blocks. Destinations that do not exist yet use their nearest existing
parent directory. If the filesystem query fails, D-LAN retains its existing
unknown-space fallback; normal write errors still apply.

## Linux settings and data

Settings (`ROAMING`) use `~/.config/d-lan/`, and local data (`LOCAL`), including
logs and caches, use `~/.local/share/d-lan/`. `XDG_CONFIG_HOME` and `XDG_DATA_HOME`
override the respective base directories. Existing files in `~/.d-lan/` are not
migrated or loaded automatically.

## macOS crash reports and stack traces

The core and GUI install a fatal-signal handler on Apple Silicon and Intel Macs.
Reports named `crash_<Unix seconds>_<nanoseconds>_<pid>.log` are written beside
their normal logs in `~/Library/Logs/D-LAN/log_core/` and `log_gui/`. An explicit
`LOCAL` directory override also applies to crash reports. Reports have owner-only
permissions, remain local, and are also written to stderr if the directory is
unavailable. Normal log rotation can remove old crash logs; copy reports you want
to keep.

The handler covers `SIGSEGV`, `SIGBUS`, `SIGABRT` (including uncaught C++
exceptions), `SIGILL`, `SIGFPE` and `SIGTRAP`. Reports include the executable,
version, architecture, signal, process/thread identifiers, machine context and
up to 64 raw stack frames from the interrupted thread. A startup snapshot of
Mach-O load addresses, UUIDs and paths supports offline symbolication. Libraries
loaded later (for example Qt plugins) are absent from this snapshot.

`LM::CrashHandler::stackTrace()` now returns demangled function names and module
offsets during normal execution. Crash-time tracing uses a bounded frame-pointer
walk with Mach memory reads; it avoids Qt, the logger, allocation and symbol
lookup. Corrupt or omitted frames can truncate the trace. An alternate signal
stack protects the installing thread (normally the main thread); worker-thread
stack overflow may prevent a report. macOS builds retain D-LAN frame pointers.

The process still terminates, and Apple's crash reporter remains enabled. D-LAN
reports supplement the system's diagnostic reports; they are not memory dumps
(`writeMiniDump` remains Windows-only). The implementation follows Apple's
[crash-reporting guidance](https://developer.apple.com/forums/thread/113742)
on capturing the interrupted context and deferring symbolication.

For source filenames and line numbers, build with `Debug` or `RelWithDebInfo` and
retain the matching executable and debug symbols (including its dSYM if
generated). Match the UUID in the report against `dwarfdump --uuid <binary>`.
Given an image's load address and a raw frame address from the report:

```sh
atos -arch arm64 -o /path/to/matching/binary -l 0xLOAD_ADDRESS 0xFRAME_ADDRESS
```

Use `-arch x86_64` for Intel reports. The macOS `TestsCrashHandlerDarwin` suite
exercises real crashes in subprocesses, verifies signal termination against
unhandled crashes, and checks offline symbolication using `atos`.

## Linux crash reports

The GUI and core automatically install a fatal-signal handler. Reports named
`crash_<Unix seconds>_<nanoseconds>_<pid>.log` are saved beside their normal logs,
usually in `~/.local/share/d-lan/log_gui/` and `~/.local/share/d-lan/log_core/`. They contain the executable
path, D-LAN version, signal, process/thread IDs, fault address (for hardware faults),
instruction pointer on x86/x86-64/AArch64, memory mappings and a stack trace.
Signal details and the trace are also written to stderr, including when the report
directory is unavailable. Reports use owner-only permissions and remain local.
Copy reports you want to keep: normal log rotation also removes old crash logs.

The process still terminates with the original signal; system core-dump settings
continue to apply. `writeMiniDump` is Windows-only. On Linux, use the system's core
dump with GDB when a complete memory snapshot is needed.

Executable symbols are exported for function names. For source filenames and line
numbers, build with `-DCMAKE_BUILD_TYPE=Debug` or `RelWithDebInfo` and retain the
matching executable and debug symbols. C++ names in fatal reports can be decoded
with `c++filt < crash_....log`. For a trace entry of the form
`/path/to/binary(+0xOFFSET)`, resolve the offset using:

```sh
addr2line -e /path/to/the/matching/binary -f -C -i 0xOFFSET
```

For entries containing a function name, the following offset is relative to that
function, not the binary. The bracketed runtime address and saved mappings allow
ASLR adjustment for offline debugging. `LM::CrashHandler::stackTrace()` can also
be called during normal execution and returns demangled names and module offsets.

Crash-time unwinding is best effort: corrupted stacks or allocator/loader failures
can prevent a complete trace. The signal details and mappings are written before
unwinding, and the signal handler avoids Qt and the normal logger. The unwinder is
preloaded at startup but is not guaranteed async-signal-safe. An alternate signal
stack helps with stack overflow on the installing thread (normally the main
thread); worker-thread stack overflows may not produce a report.
