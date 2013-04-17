module BrowserX.Network (fetchURL,checkProtocol,downloadfile) where 

import Network.Browser
import Network.HTTP
import Network.URI
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as S
import Data.Conduit
import Data.Conduit.Binary as CB
import Network.HTTP.Conduit
import Data.List.Split
import BrowserX.Db

fetchURL :: String -> IO String
fetchURL url = do
    (cookies,rsp) <- browse $ do
        setAllowRedirects True
        --prev_cookies <- get_cookies
        --setCookies prev_cookies
        (_,rsp) <- request $ getRequest url
        cookies <- getCookies
        addCookies cookies
        return (cookies,rsp)
    put_cookieDB cookies
    return(rspBody rsp)

checkProtocol :: String -> String
checkProtocol url = 
    case (parseURI url) of
        Nothing     -> "http://" ++ url
        Just uri    ->
            if (scheme == "http:") then url 
            else
                error (scheme ++ "Protocol not supported")
            where scheme = uriScheme uri
            
downloadfile url path = withManager $ \manager -> do
    req <- parseUrl url
    res <- http req manager
    responseBody res $$+- printProgress =$ CB.sinkFile path

printProgress :: Conduit S.ByteString (ResourceT IO) S.ByteString
printProgress =
    loop 0
  where
    loop len = await >>= maybe (return ()) (\bs -> do
        let len' = len + S.length bs
        liftIO $ putStrLn $ "Bytes consumed: " ++ show len'
        yield bs
        loop len')
