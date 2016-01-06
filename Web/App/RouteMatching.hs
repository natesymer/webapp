{-# LANGUAGE OverloadedStrings #-}
module Web.App.RouteMatching
(
  -- * Route predicates
  literal,
  captured,
  regex,
  -- * HTTP matching actions
  get,
  post,
  put,
  patch,
  delete,
  options,
  anyRequest
)
where
  
import Web.App.Monad.WebAppT
import Web.App.State
import Web.App.Monad.RouteT
import Web.App.Path
  
import Network.Wai (Request(..))
import Network.HTTP.Types.Method

{-# INLINE matchMethod #-}
matchMethod :: Method -> Predicate
matchMethod meth = \r -> (requestMethod r) == meth

{- Monadic matchers -}

-- |Match a `GET` request.
get :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
get p act = route (matchMethod methodGet) p act

-- |Match a `POST` request.
post :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
post p act = route (matchMethod methodPost) p act

-- |Match a `PUT` request.
put :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
put p act = route (matchMethod methodPut) p act

-- |Match a `PATCH` request.
patch :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
patch p act = route (matchMethod methodPatch) p act

-- |Match a `DELETE` request.
delete :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
delete p act = route (matchMethod methodDelete) p act

-- |Match a `OPTIONS` request.
options :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
options p act = route (matchMethod methodOptions) p act

-- |Match any request.
anyRequest :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
anyRequest = route (const True)