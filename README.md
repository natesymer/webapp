# webapp - simple webapp scaffolding with state

Basic example:

    module Main where
    
    import Web.App
    import Web.Scotty.Trans
    
    instance WebAppState Integer where
      initState = return 0
      destroyState st = do
        putStr "Counted: "
        print st

    main = startHTTP app 3000

    app :: (ScottyError e) => ScottyT e (WebAppM Integer) ()
    app = do
      get "/add" $ do
      	modifyState (+1)
        
      get "/subtract" $ do
        (State count) <- getState
        putState (State count-1)
        
      get "/assets/:file" $ param "file" >>= loadAsset
    
    
There is also a module called `FileCache` which is available for loading cached files. It loads files into memory, MD5 sums them, compresses them, and then stores them in the cache.