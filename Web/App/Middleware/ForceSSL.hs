{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-|
Module      : Web.App.Middleware.ForceSSL
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : Cross-Platform

WAI middlware to force ssl.
-}

module Web.App.Middleware.ForceSSL
(
  forceSSL
)
where

import Network.Wai
import Data.Maybe (fromJust)
import Data.Monoid
import Network.HTTP.Types.Status (status301)

-- TODO: specify port
-- |Middleware to force SSL traffic.
forceSSL :: Int -> Middleware
forceSSL port app = \req respond -> if isSecure req
  then app req respond
  else let url = "https://"
               <> (fromJust $ requestHeaderHost req)
               <> rawPathInfo req
               <> rawQueryString req
       in respond $ responseLBS status301 [("Location", url)] ""