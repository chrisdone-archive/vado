# wish

This is a demonstration program that is able to load a web page up and
render it like in the early 90's. It supports laying out text,
different font sizes for headings, inline and block elements,
hyperlinks, bold and italics. It supports mousewheel scrolling, too.

The project uses a few established Haskell packages:

* http-client for downloading via HTTP.
* http-client-tls for TLS support.
* html-conduit for parsing the HTML in "quirks mode", i.e. supporting
  garbage HTML.
* xml-conduit for dealing with the XML tree.
* sdl2 for creating a window and rendering context, handling mouse
  events.
* sdl2-cairo and cairo for rendering text onto the SDL surface.
* network-uri for parsing URIs and combining them.

This project is intended to be easy to build. If you succeed in
building this package on a platform that isn't listed in the
*Building* section, please open a PR with those instructions. It uses
the cross-platform SDL package, and the probably-less-so
cross-platform Cairo package for rendering text.

## Demo

![Demo video](http://i.imgur.com/189nfP4.gif)

## Building

OS X instructions:

    $ brew install sdl2 cairo
    $ stack install

## Running

It accepts an initial home page URL:

    $ wish <complete URL including https or http>

It doesn't support back/forward or other history features.
