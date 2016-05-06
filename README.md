# webapp - WAI web framework

Webapp is a web framework that is designed to provide everything needed to define & deploy a web app. For how to use, see Haddock documentation. For an example, see `example.hs`.

Webapp provides a function called `webappMain` (as well as a series of other similarly named functions) that start the built-in webserver. Your web application's `main` function should include a call to one of them at the end.

# Using a webapp web app

Once you've written your web app, deploying is up to you. Webapp will probably work with services like Heroku or complicated load balancers, but it designed to be a standalone server (i.e. no need to run behind something like nginx).

Webapp works by first binding to an IPv4 TCP port, immediately after which resigning privileges†. Then it builds a `WAI` app from your `WebAppT` app, applies middleware, and runs `Warp`.

† The effective GID & UID are set to match the real GID & UID.