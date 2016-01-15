# webapp - WAI web framework

Webapp is a web framework that is designed to provide everything needed to define & deploy a web app.

Basic example:

    module Main where
    
    import Web.App
	import qualified Control.Monad.State.Class as S
    
    instance WebAppState Integer where
      initState = return 0
      destroyState st = do
        putStr "Counted: "
        print st

    main = webappMainIO' app "My Web App"

    app :: WebAppT Integer IO ()
    app = do
      get "/" $ do
        addHeader "Content-Type" "text/plain"
   		S.get >>= writeBody . show
    
      get "/add" $ do
      	S.state (((),) . (+) 1)
   		redirect "/"
        
      get "/subtract" $ do
        S.get >>= S.put . ((-) 1)
        redirect "/"