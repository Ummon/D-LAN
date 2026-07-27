# D-LAN Website

This is the website of the [D-LAN software](http://www.d-lan.net).

It's built with [Wisp](https://gleam-wisp.github.io/wisp/) and [Mist](https://mist.hexdocs.pm/).

When developping you can start a web server with this command (using [Nushell](https://www.nushell.sh/)): `nu run.nu` . The modules are automatically reloaded when their code is modified, it uses [radiate](https://radiate.hexdocs.pm/) in dev mode.

## Folder descriptions

* *priv/static/colobox*: [Colobox](https://www.jacklmoore.com/colorbox/) is a _jQuery_ plugin to show images in a modal window.
* *priv/static/img*: Images needed by the website.
* *priv/releases/{platform}*: Empty at started, may be filled with releases of _D-LAN_.
* *scss/*: SASS stylesheets, can be built with the command `nu run.nu build-css` or `nu run.nu watch-css`
* *src/*: Server-side code in [Gleam](https://gleam.run/).
* *test/*: Tests that can be run with `gleam test`