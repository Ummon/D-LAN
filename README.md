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

The code is in `application`: [Core](application/Core) handles sharing and transfers,
[GUI](application/GUI) provides the Qt interface, and [Common](application/Common)
contains shared infrastructure. CMake builds the core, GUI, tools and tests.

### Prerequisites

* CMake 3.21 or newer, Ninja, and a C++20 compiler compatible with your Qt installation. Use LLVM-MinGW on Windows, Xcode Command Line Tools on macOS, or GCC/Clang on Linux.
* [Qt 6](https://www.qt.io/development/download) with Core, Network, Xml, Sql, Widgets, SvgWidgets, and Test. Install LinguistTools and Qt 6.7 or newer to build translations.
* On Linux: Qt DBus and the matching Qt Core/Gui private headers (commonly packaged as `qt6-base-private-dev`), plus `dbus-run-session` for file-manager integration tests. Build against the Qt version used at runtime.
* [BLAKE3](https://github.com/BLAKE3-team/BLAKE3)'s C library and [Protobuf](https://github.com/protocolbuffers/protobuf) with a matching `protoc` compiler. If Protobuf has no CMake package configuration, install pkg-config and its `.pc` file.

Use dependencies built for the same compiler and architecture. CMake locates them;
it does not download or build them. See [dependency build instructions](doc/DEVELOPMENT.md#build-blake3)
if you need to build BLAKE3 or Protobuf yourself.

### Configure and build

From the repository root, replace the dependency paths with your installations:

```sh
cmake -S application -B application/build/debug -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_PREFIX_PATH="/path/to/Qt" -DDLAN_BLAKE3_ROOT="/path/to/blake3/c" -DDLAN_PROTOBUF_ROOT="/path/to/protobuf"
cmake --build application/build/debug --parallel
ctest --test-dir application/build/debug --output-on-failure
```

Put the compiler, CMake, Ninja and `protoc` on `PATH`. `DLAN_BLAKE3_ROOT` must
contain `blake3.h` and `lib/`; `DLAN_PROTOBUF_ROOT` points to the Protobuf installation.
Executables are written to `application/build/debug/output`.

Alternatively, open `application/CMakeLists.txt` in Qt Creator, select a kit matching
your compiler and Qt installation, and set the same dependency paths in its CMake configuration.

Tests and tools are enabled by default. Set `DLAN_BUILD_TESTS=OFF` or
`DLAN_BUILD_TOOLS=OFF` to omit them. Use `Release` instead of `Debug` for an optimized build.

## Further documentation

See the [development reference](doc/DEVELOPMENT.md) for macOS release testing, release packaging (Windows
installer and Linux AppImage), profiling, platform-specific behavior, storage
locations, and crash reports and stack traces.
