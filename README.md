# D-LAN Website

This is the website of the [D-LAN software](http://www.d-lan.net) .

It's built with [Wisp](https://gleam-wisp.github.io/wisp/) and [Mist](https://mist.hexdocs.pm/).

When developping you can start a web server with this command (using [Nushell](https://www.nushell.sh/)): `nu run.nu` . The modules are automatically reloaded when their code is modified, it uses [radiate](https://radiate.hexdocs.pm/) in dev mode.

## Folder descriptions

* *priv/static/colobox*: [Colobox](https://www.jacklmoore.com/colorbox/) is a _jQuery_ plugin to show images in a modal window.
* *priv/releases/{platform}*: Empty at started, may be filled with releases of _D-LAN_.
* *priv/static/img*: Images needed by the website.
* *src/*: Server-side code in [Gleam](https://gleam.run/).