{-|
Module      : Web.App.State
Copyright   : (c) Nathaniel Symer, 2016
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Typeclass all types to be used as state for a WebApp
must have an instance of.
-}

module Web.App.State
(
  WebAppState(..)
)
where
  
-- |Defines a common interface for an app's state.
class WebAppState s where
  -- |Create an app state
  initState :: IO s
  -- |Destroy an app state
  destroyState :: s -- ^ the state to destroy
               -> IO ()
               
instance WebAppState () where
  initState = return ()
  destroyState _ = return ()