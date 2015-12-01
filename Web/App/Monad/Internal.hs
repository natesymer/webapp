{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
 
module Web.App.Monad.Internal
(
  WebApp(..)
)
where
  
import Web.App.FileCache (FileCache)

data WebApp s = WebApp {
  webAppCache :: FileCache,
  webAppState :: s
}