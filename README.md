# D-LAN

D-LAN is open source, decentralized LAN file sharing software.

Easily share files and folders on a local area network, such as at a LAN party. After launching D-LAN, you automatically discover other users and their shared files without special configuration or a central server.

* [Website](http://www.d-lan.net)
* [Development wiki](http://dev.d-lan.net/projects/pmp/wiki)
* [Forums](http://dev.d-lan.net/projects/pmp/boards)
* [Issues](http://dev.d-lan.net/projects/pmp/issues)


## Features

* Share files and folders in a local area network environment (LAN).
* Distributed transfers to increase performance and reliability.
* Very easy to use: no configuration, no central server.
* Fast indexed search among all other peers.
* Browse all files and folders of any other peer.
* Manage the download queue. It includes adding, deleting, pausing or reordering.
* Persistent global chat with channels, formatting, and emoticons.
* D-LAN can run without a graphical user interface (GUI) and be controlled remotely.
* Open source. Source code distributed under the [GPLv3 license](COPYING).
* Free of ads and malware.


## Development

The release workflow below targets Windows with LLVM-MinGW.

### Prerequisites

* CMake 3.21 or newer and Ninja, available on `PATH`.
* An LLVM-MinGW toolchain compatible with your Qt installation. Add its `bin` directory to `PATH` so that `clang`, `clang++`, and `llvm-rc` are available.
* [Qt Framework](https://www.qt.io/development/download): Install Qt 6 with the Core, Network, Xml, Sql, Widgets, and SvgWidgets modules, plus Test for tests. On Linux, the GUI also requires DBus and the matching Qt Core/Gui private headers for Wayland window activation (included with the Qt SDK; distribution packages commonly call these `qt6-base-private-dev`). Rebuild against the Qt version used at runtime. File-manager integration tests use `dbus-run-session`. The release workflow also requires LinguistTools and Qt 6.7 or newer for the translation commands. Add the Qt `bin` directory to `PATH`, for example `C:\Qt\6.11.2\llvm-mingw_64\bin`, adjusting the version and kit to your installation. Set `CMAKE_PREFIX_PATH` to the Qt installation directory if CMake cannot locate it.
* [BLAKE3](https://github.com/BLAKE3-team/BLAKE3): Install or build the C library. Set `DLAN_BLAKE3_ROOT` to the directory containing `blake3.h` and the `lib` directory.
* [Protobuf](https://github.com/protocolbuffers/protobuf): Install or build the library and the matching `protoc` compiler. Set `DLAN_PROTOBUF_ROOT` to the installation directory, with `protoc` in its `bin` directory or on `PATH`. If the installation has no CMake package configuration, the build requires pkg-config and Protobuf's `.pc` file.
* Git, available on `PATH`, for the release script's version information.
* [Nushell](https://www.nushell.sh/), available as `nu`, to run the build scripts.
* For Windows packaging: Qt's `windeployqt.exe` and Inno Setup's `iscc`, both available on `PATH`.

CMake locates existing BLAKE3 and Protobuf installations; it does not download or build them. Use libraries compatible with the selected compiler and architecture. Their default locations are defined in [application/CMakeLists.txt](application/CMakeLists.txt) and can be overridden using the cache options below.

### Build BLAKE3

Run these commands from the directory `BLAKE3-1.8.7/c/`:

```nushell
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=OFF -DBUILD_TESTING=OFF -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY="../lib"
cmake --build build --parallel
```

### Build Protobuf

Run these commands from the directory `protobuf-36.1/`:

```nushell
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_STANDARD=20 -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DCMAKE_INSTALL_PREFIX="." -DCMAKE_INSTALL_LIBDIR=lib -DBUILD_SHARED_LIBS=OFF -Dprotobuf_BUILD_SHARED_LIBS=OFF -Dprotobuf_BUILD_TESTS=OFF -Dprotobuf_BUILD_PROTOC_BINARIES=ON -Dprotobuf_FORCE_FETCH_DEPENDENCIES=ON
cmake --build build --parallel
cmake --install build
```

### Qt Creator

In Qt Creator: File → Open File or Project → select
`application/CMakeLists.txt`, then pick the LLVM-MinGW kit matching your Qt installation.

Useful cache options:

| Option | Default | Purpose |
|---|---|---|
| `DLAN_BUILD_TESTS` | `ON` | Test executables (+ CTest) |
| `DLAN_BUILD_TOOLS` | `ON` | LogViewer, FileIndexer, PasswordHasher |
| `DLAN_PROFILING` | `OFF` | gprof `-pg` |
| `DLAN_BLAKE3_ROOT` | `C:/BLAKE3-1.8.5/c` | BLAKE3 location |
| `DLAN_PROTOBUF_ROOT` | `C:/protobuf` | protobuf location |

### Build a release

Run [application/build.nu](application/build.nu) from the `application` directory. From the repository root:

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

### Linux AppImage

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

### Linux settings and data

Settings (`ROAMING`) use `~/.config/d-lan/`, and local data (`LOCAL`), including
logs and caches, use `~/.local/share/d-lan/`. `XDG_CONFIG_HOME` and `XDG_DATA_HOME`
override the respective base directories. Existing files in `~/.d-lan/` are not
migrated or loaded automatically.

### Linux crash reports

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
