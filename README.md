# wish

This is a demonstration program that is able to load a web page up and
render it like in the early 90's. It supports laying out text,
different font sizes for headings, inline and block elements,
hyperlinks, bold and italics. It supports mousewheel scrolling,
too.

The project uses a few established Haskell packages:

* [http-client](https://www.stackage.org/package/http-client) for downloading via HTTP.
* [http-client-tls](https://www.stackage.org/package/http-client-tls) for TLS support.
* [html-conduit](https://www.stackage.org/package/html-conduit) for parsing the HTML in "quirks mode", i.e. supporting
  garbage HTML.
* [xml-conduit](https://www.stackage.org/package/xml-conduit) for dealing with the XML tree.
* [sdl2](https://www.stackage.org/package/sdl2) for creating a window and rendering context, handling mouse
  events.
* [sdl2-cairo](https://www.stackage.org/package/sdl2-cairo) and
  [cairo](https://www.stackage.org/package/sdl2-cairo) for rendering text
  onto the SDL surface.
* [network-uri](https://www.stackage.org/package/network-uri) for parsing URIs and combining them.

This project is intended to be easy to build. If you succeed in
building this package on a platform that isn't listed in the
*Building* section, please open a PR with those instructions. It uses
the cross-platform SDL package, and the probably-less-so
cross-platform Cairo package for rendering text.

There's some groundwork for rendering boxes, padding, etc. but no
attempt has been made to implement that. One could use the
[language-css](http://hackage.haskell.org/package/language-css)
package to parse CSS and add styling to elements.

## Demo

![Demo video](http://i.imgur.com/189nfP4.gif)

## Building

[Get Stack](https://haskell-lang.org/get-started) for building Haskell
projects.

OS X instructions:

    $ brew install sdl2 cairo
    $ stack install --install-ghc

Ubuntu Linux instructions:

    $ sudo apt-get install libsdl2-dev libcairo2-dev
    $ stack install --install-ghc

## Running

It accepts an initial home page URL:

    $ wish <complete URL including https or http>

It doesn't support back/forward or other history features.
