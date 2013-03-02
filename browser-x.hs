import System.Environment
import Network.Browser
import Network.HTTP
 
 -- | 'main' runs the main program
main = do
    urls <- getArgs
    (_,rsp) <- browse $ do
        setAllowRedirects True
        setProxy NoProxy
        request $ getRequest $ head urls
    print $ rspBody rsp
