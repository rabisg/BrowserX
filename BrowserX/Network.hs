module BrowserX.Network (fetchURL,checkProtocol) where 

import Network.Browser
import Network.HTTP
import Network.URI

fetchURL :: String -> IO String
fetchURL url = do
    (_,rsp) <- browse $ do
        setAllowRedirects True
        request $ getRequest url
    return(rspBody rsp)

checkProtocol :: String -> String
checkProtocol url = 
    case (parseURI url) of
        Nothing     -> "http://" ++ url
        Just uri    -> if (uriScheme uri == "http:") then url else error "Protocol not supported"
