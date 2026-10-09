# D-LAN Website

This is the website of the [D-LAN software](http://www.d-lan.net).

It's built with [Wisp](https://gleam-wisp.github.io/wisp/) and [Mist](https://mist.hexdocs.pm/).

When developping you can start a web server with this command (using [Nushell](https://www.nushell.sh/)): `nu run.nu` . The modules are automatically reloaded when their code is modified, it uses [radiate](https://radiate.hexdocs.pm/) in dev mode.

## Building on Windows

Two dependencies contain native code (_esqlite_ and _jargon_), they need [MSYS2](https://www.msys2.org/) with the packages `make` and `mingw-w64-x86_64-gcc`, and `C:\msys64\usr\bin` in the `PATH`.

The first build has to be launched with `nu run.nu build` (or any other `run.nu` command): it sets the environment needed to compile the native code. After that `gleam build` or `gleam test` can be used directly.

A shipment exported on Windows contains Windows native libraries (_.dll_) and can't be run on Linux as is: `nu run.nu deploy` replaces them by Linux ones cross-compiled with [Zig](https://ziglang.org/) (`zig` has to be in the `PATH`). The host must be an _x86_64_ or _aarch64_ Linux with _glibc_ 2.28 or newer.

## Folder descriptions

* *priv/static/colobox*: [Colobox](https://www.jacklmoore.com/colorbox/) is a _jQuery_ plugin to show images in a modal window.
* *priv/static/img*: Images needed by the website.
* *priv/releases/{platform}*: Empty at started, may be filled with releases of _D-LAN_.
* *scss/*: SASS stylesheets, can be built with the command `nu run.nu build-css` or `nu run.nu watch-css`
* *src/*: Server-side code in [Gleam](https://gleam.run/).
* *test/*: Tests that can be run with `gleam test`