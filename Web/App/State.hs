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