# Development reference

See [README](../README.md) for the project overview, prerequisites and basic build instructions.
This document covers dependency builds, packaging, profiling and platform details.
Unless stated otherwise, paths are relative to the repository root.

* [Build BLAKE3](#build-blake3)
* [Build Protobuf](#build-protobuf)
* [Build OpenSSL for macOS](#build-openssl-for-macos)
* [CMake options](#cmake-options)
* [Build a release](#build-a-release)
* [macOS release testing](#macos-release-testing)
* [macOS application packaging](#macos-application-packaging)
* [Linux AppImage](#linux-appimage)
* [Profiling](#profiling)
* [IPv6 peer discovery](#ipv6-peer-discovery)
* [Remote-control TLS](#remote-control-tls)
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

For macOS packages, add `-DCMAKE_OSX_ARCHITECTURES=arm64` and
`-DCMAKE_OSX_DEPLOYMENT_TARGET=26.0` to **both** dependency configure commands.
Use fresh build directories when rebuilding dependencies that previously targeted
a newer macOS version. Protobuf's bundled Abseil and utf8_range must be rebuilt
with the same settings. Changing only D-LAN's deployment target does not make
previously compiled static libraries compatible with macOS 26.

## Build OpenSSL for macOS

Homebrew libraries can require a newer macOS version than D-LAN's deployment
target. For example, a `libcrypto.3.dylib` built for macOS 27 produces linker
warnings when building D-LAN for macOS 26. Build a separate OpenSSL 3 installation
with the same minimum version as D-LAN.

Download and verify an [OpenSSL 3 source release](https://openssl-library.org/source/),
then run these commands from its extracted source directory, choosing an absolute
installation path:

```sh
./Configure darwin64-arm64-cc shared -mmacosx-version-min=26.0 --prefix=/absolute/path/to/openssl-macos26 --libdir=lib
make -j8
make test
make install_sw
```

In each Qt Creator build configuration, set `OPENSSL_ROOT_DIR` to that installation
path. Clear the cached `OPENSSL_CRYPTO_LIBRARY`, `OPENSSL_SSL_LIBRARY` and
`OPENSSL_INCLUDE_DIR` entries, then run CMake again and rebuild. Alternatively,
update an existing build from the repository root:

```sh
cmake -S application -B application/build/Qt_6_11_2_for_macOS_Debug -U 'OPENSSL_*' -DOPENSSL_ROOT_DIR=/absolute/path/to/openssl-macos26
```

Each build directory has its own CMake cache. If using
`nu build.nu --build-dir build/release` from `application`, also reconfigure
`application/build/release` with the command above, replacing the `-B` path.
The default `build.nu` command already cleans compiled files; cleaning does not
reset cached dependency paths.

Verify the actual minimum OS version (the `minos` field, not `sdk`):

```sh
xcrun vtool -show-build /absolute/path/to/openssl-macos26/lib/libcrypto.3.dylib
```

Changing D-LAN's deployment target or suppressing linker warnings does not make
an existing OpenSSL binary compatible with macOS 26.

## CMake options

| Option | Default | Purpose |
|---|---|---|
| `DLAN_BUILD_TESTS` | `ON` | Test executables (+ CTest) |
| `DLAN_BUILD_TOOLS` | `ON` | LogViewer, FileIndexer, PasswordHasher |
| `DLAN_PROFILING` | `OFF` | gprof `-pg`; requires a compatible compiler and runtime (checked at configure time) |
| `DLAN_PDB` | `ON` | Windows Clang: emit PDB symbols in non-Debug builds for crash reports and profiling |
| `DLAN_BLAKE3_ROOT` | `C:/BLAKE3-1.8.7/c` | BLAKE3 location |
| `DLAN_PROTOBUF_ROOT` | `C:/protobuf-36.1` | protobuf location |
| `OPENSSL_ROOT_DIR` | CMake search paths | OpenSSL 3 headers and libraries |
| `DLAN_OPENSSL_RUNTIME_DIR` | Inferred from the crypto library's installation | Windows packaging: directory containing the matching libcrypto and libssl DLLs |

## Remote-control TLS

The GUI/Core control connection uses TLS 1.2 or later for non-local addresses on
the existing remote-control port. Local connections remain plaintext, using
`Common::Global::isLocal()` (loopback and this machine's interface addresses).
There is no remote plaintext fallback; older clients need updating. Peer file
transfers are independent of this control connection.

On startup, the Core generates a self-signed RSA-3072/SHA-256 identity if none
exists, and saves its private key and certificate together in
`remote-control-tls/core.pem` under `Global::getDataFolder(DataFolderType::ROAMING)`.
Generation uses libcrypto directly; no `openssl` command is required. The file
is written atomically with owner read/write permissions on Unix. On Windows,
the roaming directory's ACL must restrict access to the account running the
Core (the service account when running as a service). Keep this private file
out of shared folders. The Core logs the certificate's SHA-256 fingerprint.

Certificates last ten years and are reused across restarts. An unreadable,
invalid, expired or mismatched existing identity disables remote access and is
logged; it is never silently overwritten. Local access still works.

The GUI uses trust on first use: it remembers the certificate after a successful
password login in its own roaming folder, as
`remote-control-tls/peer-<endpoint SHA-256>.pem`. The endpoint is the normalized
entered hostname/IP and port, so aliases have separate pins. Subsequent
connections must present exactly that certificate, including certificates
otherwise trusted by a public CA. A persistence failure aborts the connection.

Make the first connection over a trusted network: the password challenge does
not independently verify the Core, and first-use trust cannot detect an active
interceptor during pairing. To verify a fingerprint out of band, compare the
Core's logged fingerprint with the saved peer certificate, for example using
`openssl x509 -in <peer-file> -noout -fingerprint -sha256`.

For an intentional identity replacement, stop the Core, back up/remove its
`core.pem`, and restart it to generate a new identity. Verify the new fingerprint
through a trusted channel before replacing the affected GUI pin (the refusal
log identifies its path) with the new public certificate. Do not copy the
private key to GUI machines. Deleting a pin instead repeats first-use trust.

The build requires OpenSSL 3 development libraries. Windows packaging copies
libcrypto/libssl DLLs alongside Qt's TLS plugins; set `DLAN_OPENSSL_RUNTIME_DIR`
when they are not in the installation's `bin` directory. macOS packaging uses
Qt's Secure Transport plugin and deploys the linked libcrypto dependency.
`macdeployqt` copies `libcrypto.3.dylib` into `Contents/Frameworks` and rewrites
the executable dependencies to the bundled copy. No Homebrew installation is
needed on the destination machine. The macOS bundle includes the OpenSSL license
in `Contents/Resources/licenses/OpenSSL.txt`. The RSA/SHA-256 certificate code
uses OpenSSL's built-in default provider, so it needs no external provider module
or `openssl` executable; `libssl` is not needed with Secure Transport.
Linux AppImage packaging explicitly includes Qt's OpenSSL plugin and the shared
libssl library, which Qt loads dynamically. Use dependencies for the target
compiler/architecture; do not add another toolchain's system headers to the
compiler search path when configuring `OPENSSL_INCLUDE_DIR`.

`TestsRemoteControlTls` exercises real TLS and plaintext sockets, persistence,
certificate changes, authentication failures, cancellation and handshake limits.
It uses Qt's default backend; set `DLAN_TEST_TLS_BACKEND` to a backend name to
exercise another installed backend explicitly.

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

## macOS release testing

Install [Nushell](https://www.nushell.sh/) (`nu` on `PATH`) in addition to the
build prerequisites in README. Configure a separate Release build with tests
enabled. From the repository root, adjust the dependency paths:

```sh
cmake -S application -B application/build/release -DCMAKE_BUILD_TYPE=Release -DCMAKE_OSX_ARCHITECTURES=arm64 -DCMAKE_OSX_DEPLOYMENT_TARGET=26.0 -DCMAKE_PREFIX_PATH="/path/to/Qt" -DDLAN_BLAKE3_ROOT="/path/to/blake3/c" -DDLAN_PROTOBUF_ROOT="/path/to/protobuf" -DDLAN_BUILD_TESTS=ON
cd application
nu build.nu release-test --build-dir build/release
```

You can also configure a Release kit in Qt Creator and pass its build directory.
Without `--build-dir`, the script inspects CMake caches under `application/build/`
and selects the sole Release build, including lowercase `build/release`. It
rejects Debug builds and asks for an explicit selection if several Release
builds exist. Multi-configuration generators use the `Release` configuration.

The command builds the configured targets, compiles translations without editing
the source `.ts` files, and runs all registered tests through CTest. Qt
LinguistTools is required for translations; use `--no-translations` to skip that
step when it is unavailable. Add `--clean` to clean the selected build first.
Builds use at most eight parallel jobs; override this with `--jobs 4` (or `-j 4`),
for example on machines with less memory.
`release-test` finishes after testing and remains useful for development and CI.
On macOS, `nu build.nu` runs a clean build, tests, then packaging; `build-all`
does the same without cleaning unless requested. A failed test prevents packaging.

To rerun the tests without rebuilding:

```sh
nu build.nu run-tests --build-dir build/release
```

`run-tests` uses CTest on every platform, including macOS executables without
an `.exe` suffix. It preserves registered test environments and working
directories, prints failing test output, and fails if tests are disabled, no
tests are registered, or any test fails. Build and translation failures also
stop the workflow. Tests run sequentially with a default timeout of 300 seconds;
individual CTest timeout properties take precedence. Results are available in
the build directory's `Testing/Temporary/LastTest.log` and `LastTestsFailed.log`
(when tests fail).

Run from a normal macOS developer session with access to FSEvents and local
network sockets. A restrictive sandbox can prevent some integration tests from
running. The workflow reports these failures rather than skipping them.

## macOS application packaging

The package targets **macOS 26.0 or newer, Apple Silicon (arm64) only**. It is a
compressed, read-only DMG containing `D-LAN.app` and an Applications shortcut.
Open the DMG, drag D-LAN onto Applications, then eject the image and launch D-LAN
from Applications. The bundle contains the GUI, core, optional PasswordHasher
tool, Qt frameworks and plugins
(including Cocoa and SQLite), translations, styles, emoticons and the license.
Read-only assets live in `Contents/Resources`; the GUI launches the adjacent
core in `Contents/MacOS`. Configuration and logs retain their usual Library paths.

Configure a Release build with `-DCMAKE_OSX_ARCHITECTURES=arm64` and
`-DCMAKE_OSX_DEPLOYMENT_TARGET=26.0`, using dependencies built for macOS 26.0 or
earlier. D-LAN defaults to 26.0 when no deployment target is specified. Use a
fresh D-LAN build directory when switching dependency installations so cached
library paths cannot select the old versions. The packager records the highest
minimum macOS version required by the bundled binaries and rejects a bundle
requiring anything newer than 26.0. This check cannot detect the original target
of a static library after linking; rebuilding those dependencies is essential.
Runtime compatibility should also be tested on a macOS 26 machine.

If Qt Creator reports that a library was built for a newer macOS version, check
the library path in the warning. Set `DLAN_PROTOBUF_ROOT` and `DLAN_BLAKE3_ROOT`
to the rebuilt dependencies in each build configuration. For an existing build,
also clear the cached `protobuf_DIR`, `absl_DIR`, `utf8_range_DIR`, `DLAN_PROTOC`,
`DLAN_BLAKE3_LIBRARY` and `DLAN_BLAKE3_INCLUDE_DIR` entries before reconfiguring;
changing the root paths alone does not replace cached lookup results. Keep the
deployment target at 26.0 to retain compatibility with macOS 26.
For warnings naming `libcrypto` or `libssl`, follow
[Build OpenSSL for macOS](#build-openssl-for-macos) and clear the OpenSSL cache
entries listed there.

The packager uses `macdeployqt` and plugins from the Qt SDK selected by the build's
`Qt6_DIR`, plus Apple's command-line tools (`lipo`, `otool`, `codesign`, `sips`,
`iconutil`, `plutil` and `hdiutil`). Qt LinguistTools is required for translations.
The DMG opens in icon view with large app and Applications icons and an install
arrow. Packaging renders its background using Swift/AppKit and saves the layout
through Finder. Run it in a logged-in macOS desktop session and allow the terminal
to control Finder if macOS requests Automation permission.
From `application`, build, test and package with:

```sh
nu build.nu build-all --build-dir build/release --jobs 4
```

To package an already-built release independently of the test workflow:

```sh
nu build.nu make-setup --build-dir build/release
```

The result is
`application/Setups/macOS/Installations/D-LAN-<version>-<build-time>-arm64.dmg`
(the version includes its tag, if any). Packaging uses a temporary staging
directory, leaving the SDK and build outputs unchanged. Universal dependencies
are thinned to arm64, Intel-only binaries are rejected, and dependency paths and
code signatures are checked before the disk image is created. The image checksum
is verified before publishing the output file. No downloads occur.

By default, the app is **ad-hoc signed, not notarized**. For public distribution,
set `DLAN_MACOS_SIGN_IDENTITY` to your installed Developer ID Application identity
before packaging; this enables hardened-runtime signing of the app and signs
both the app and disk image with a secure timestamp.
Notarization and stapling remain separate steps requiring your Apple credentials.
An ad-hoc signature does not establish Gatekeeper trust for downloaded apps.
See [Qt deployment](https://doc.qt.io/qt-6/macos-deployment.html) for the deployment
tool and signing options.

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

The AppImage starts `D-LAN.GUI`. To start `D-LAN.Core` instead, put `--core` as the
first argument; the remaining arguments go to the Core, for example
`./D-LAN-<...>.AppImage --core --help`. `--help` lists the AppImage arguments and the
Core arguments.

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

## IPv6 peer discovery

D-LAN uses UDP multicast on port 59486 by default. Peers must select the same
IP protocol; IPv6 peers must also use the same discovery channel. The default
`main` channel uses `ff12:0:318b:bc3f:d75a:c873:ec0d:2b18`.

On macOS, automatic discovery excludes Apple's `awdl*` and `llw*` interfaces
and tunnel interfaces (`utun*`, `gif*`, `stf*`, and point-to-point adapters).
These interfaces also do not count when deciding whether IPv6 LAN discovery
is available. In Settings, enable **Show tunnel interfaces** and select a
tunnel address to use it explicitly; the selected tunnel stays visible even
when the checkbox is cleared. Tunnel discovery requires multicast support.
The core classifies interfaces, so the same policy applies when controlled
by a GUI on another platform. Wildcard TCP and unicast UDP listening remain
unchanged; this discovery filter is not an incoming-connection access rule.

The second 16-bit word is zero on every platform. Darwin temporarily stores
the interface scope there and clears it before transmission; see Apple's
[scope handling in XNU](https://github.com/apple-oss-distributions/xnu/blob/main/bsd/netinet6/scope6.c).
Previously, D-LAN placed channel-hash bytes in that word, so Windows sent to
`ff12:e726:318b:bc3f:d75a:c873:ec0d:2b18` while macOS sent to the address above.
Local multicast loopback tests could pass despite this difference.

**Rebuild the Windows and Linux peers as well as macOS after this change.**
Older builds on those platforms still use the old IPv6 group. IPv4 discovery
and the message format are unchanged. `TestsNetworkListener` checks the exact
group address and verifies that a received IPv6 datagram retains the intended
destination group after the kernel processes it.

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
