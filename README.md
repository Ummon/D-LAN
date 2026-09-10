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
* [Qt Framework](https://www.qt.io/development/download): Install Qt 6 with the Core, Network, Xml, Sql, Widgets, and SvgWidgets modules, plus Test for tests. The release workflow also requires LinguistTools and Qt 6.7 or newer for the translation commands. Add the Qt `bin` directory to `PATH`, for example `C:\Qt\6.11.2\llvm-mingw_64\bin`, adjusting the version and kit to your installation. Set `CMAKE_PREFIX_PATH` to the Qt installation directory if CMake cannot locate it.
* [BLAKE3](https://github.com/BLAKE3-team/BLAKE3): Install or build the C library. Set `DLAN_BLAKE3_ROOT` to the directory containing `blake3.h` and the `lib` directory.
* [Protobuf](https://github.com/protocolbuffers/protobuf): Install or build the library and the matching `protoc` compiler. Set `DLAN_PROTOBUF_ROOT` to the installation directory, with `protoc` in its `bin` directory or on `PATH`. If the installation has no CMake package configuration, the build requires pkg-config and Protobuf's `.pc` file.
* Git, available on `PATH`, for the release script's version information.
* [Nushell](https://www.nushell.sh/), available as `nu`, to run the build scripts.
* For Windows packaging: Qt's `windeployqt.exe` and Inno Setup's `iscc`, both available on `PATH`.

CMake locates existing BLAKE3 and Protobuf installations; it does not download or build them. Use libraries compatible with the selected compiler and architecture. Their default locations are defined in [application/CMakeLists.txt](application/CMakeLists.txt) and can be overridden using the cache options below.

### Build BLAKE3

Run these commands from the directory `BLAKE3-1.8.7/c/`:

```
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=OFF -DBUILD_TESTING=OFF -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY="../lib"
cmake --build build --parallel
```

### Build Protobuf

Run these commands from the directory `protobuf-36.1/`:

```
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_STANDARD=20 -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DCMAKE_INSTALL_PREFIX="." -DCMAKE_INSTALL_LIBDIR=lib -DBUILD_SHARED_LIBS=OFF -Dprotobuf_BUILD_SHARED_LIBS=OFF -Dprotobuf_BUILD_TESTS=OFF -Dprotobuf_BUILD_PROTOC_BINARIES=ON -Dprotobuf_FORCE_FETCH_DEPENDENCIES=ON
cmake --build build --parallel 4
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

```sh
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
