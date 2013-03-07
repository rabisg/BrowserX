module BrowserX.Network (fetchURL) where 

import Network.Browser
import Network.HTTP

fetchURL :: String -> IO String
fetchURL url = do
    (_,rsp) <- browse $ do
        setAllowRedirects True
        request $ getRequest url
    return(rspBody rsp)