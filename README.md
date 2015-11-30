# webapp - simple webapp scaffolding with state

Basic example:

    module Main where
    
    import Web.App
    import Web.Scotty.Trans

    data State = State {
      stateCounter :: Integer
    }
    
    instance WebAppState State where
      initState = return $ State 0
      destroyState st = do
        putStr "Counted: "
        print $ stateCounter st

    main = startHTTP app 3000

    app :: (ScottyError e) => ScottyT e (WebAppM State) ()
    app = do
      get "/" $ do
        (State count) <- getState
        putState (State count+1)
    
    
There is also a module called `FileCache` which is available for loading cached files. It loads files into memory, MD5 sums them, compresses them, and then stores them in the cache.