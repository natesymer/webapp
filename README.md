# Web.App - WAI web framework

[![Build Status](https://travis-ci.org/natesymer/webapp.svg?branch=master)](https://travis-ci.org/natesymer/webapp)

Web.App is a general, minimalist Haskell web framework. See Haddock documentation and `example.hs`.

## Usage notes

1. Web.App can either be ran standalone (i.e. look ma, no nginx!) or behind other server programs.
  - To bind to privileged ports, your program must be executable as root. Privileges are resigned after the port is bound.
  - Web.App uses Warp under the hood to serve a WAI app based on provided routes.
  - SSL & HTTP2 are supported.
2. Web.App provides a function called `webappMain` (as well as a series of other similarly named functions) that start your app.
  - Your program's `main` function should finish with a call to one of them.
3. Web.App also provides command line options for controlling the HTTP server.
  - It also provides 