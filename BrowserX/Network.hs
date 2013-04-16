module BrowserX.Network (fetchURL) where 

import BrowserX.Options
import Network.Browser
import Network.HTTP

fetchURL :: Options -> String -> IO String
fetchURL settings url = do
    (_,rsp) <- browse $ do
        setAllowRedirects True
        setProxy (optProxy settings)
        request $ getRequest url
    return(rspBody rsp)