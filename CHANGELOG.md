# webapp changelog

v 0.0.1
	
- Initial release

v 0.0.2

- Fix omitted module in `webapp.cabal`: `Web.App.Monad.Internal`

v 0.1.0

- Included a new example: a counter app with an additional CLI parser.
- Implemented a "util" CLI subcommand where you can "mount" an optparse-applicative @Parser@. See example.
- Fixed termination handlers. Previously, they weren't installed when using HTTPS due to a bug in warp-tls. This has been remedied. Additionally, these handlers would destroy the initial state, rather than the current state from the @TVar@.

v 0.1.0

v 0.2.0
	
- Complete rewrite
- Basic WAI-based web framework
	- Supports routing
	- Streaming body based around `writeBody` function
	- HTTP/2 server push via `push` function. Gracefully fails with HTTP/1