# D-LAN

D-LAN is an open source decentralized LAN file sharing software.

The goal is to easily share some files and folders on a local area network environment like a LAN-Party. After you launched D-LAN, you will see all other people and theirs sharing automatically without special configuration or central server.

* "Website":http://www.d-lan.net
* "Development wiki":http://dev.d-lan.net/projects/pmp/wiki
* "Forums":http://dev.d-lan.net/projects/pmp/boards
* "Issues":http://dev.d-lan.net/projects/pmp/issues


## Features

* Share files and folders in a local area network environment (LAN).
* Distributed transfers to increase performance and reliability.
* Very easy to use: no configuration, no central server.
* Fast indexed search among all other peers.
* Browse all files and folders of any other peer.
* Manage the download queue. It includes adding, deleting, pausing or reordering.
* A global persisted chat with channels, formatting and smiles features.
* D-LAN can run without graphic interface (GUI) and be controlled remotely.
* Open source. Code source distributed under GPLv3 license.
* Free of any sort of ads or malwares.


## Development

### Configure & build

```
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build build
ctest --test-dir build        # run the tests
```

In Qt Creator: File → Open File or Project → select the top-level
`CMakeLists.txt`, pick the llvm-mingw kit.

Useful cache options:

| Option | Default | Purpose |
|---|---|---|
| `DLAN_BUILD_TESTS` | `ON` | Test executables (+ CTest) |
| `DLAN_BUILD_TOOLS` | `ON` | LogViewer, FileIndexer, PasswordHasher, ProtoBinReader |
| `DLAN_PROFILING` | `OFF` | gprof `-pg` (was `CONFIG += prof`) |
| `DLAN_BLAKE3_ROOT` | `C:/BLAKE3-1.8.5/c` | BLAKE3 location (was `blake3.pri`) |
| `DLAN_PROTOBUF_ROOT` | `C:/protobuf` | protobuf location (was `protobuf.pri`) |

D-LAN can be built under Windows or Linux (Ubuntu), see here for more information: http://dev.d-lan.net/projects/pmp/wiki/#Technical
