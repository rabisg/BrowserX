import Network.Browser
import Network.HTTP
import Network.URI

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame

import System.Process
import System.Environment
 
 -- | 'main' runs the main program
main :: IO ()
main = do
    -- Get program arguments.
    args <- getArgs
    case args of
        -- Display help
        ["--help"] ->
            putStrLn $ "Welcome to Browser-X\n\n" ++
                    "Usage: browser-x [uri]"
        -- Start program.
        [arg]    -> browser arg                     -- entry user input url
        _        -> browser "http://www.google.com" -- entry default url

-- | Internal browser fucntion.
browser url = do
    initGUI
    window <- windowNew
    set window [windowTitle := "Browser-X"]
    windowSetDefaultSize window 900 600
    windowSetPosition window WinPosCenter
    
    -- Create WebKit view.
    webView <- webViewNew    

    -- Create window box.
    winBox <- vBoxNew False 0

    -- Create address bar.
    addressBar <- entryNew

    -- Create scroll window.
    scrollWin <- scrolledWindowNew Nothing Nothing
    
    -- Load uri.
    do
        let furl = checkProtocol url
        content <- fetchURL furl      
        webViewLoadHtmlString webView content furl
        
    entrySetText addressBar url
    
    -- Open uri when user press `return` at address bar.
    onEntryActivate addressBar $ do
        uri <- entryGetText addressBar               -- get uri from address bar
        let furi = checkProtocol uri
        content <- fetchURL furi
        webViewLoadHtmlString webView content furi    -- load new uri
        
    -- Add current uri to address bar when load start.
    webView `on` loadStarted $ \frame -> do
        currentUri <- webFrameGetUri frame
        case currentUri of
            Just uri -> let furi = checkProtocol uri in
                        entrySetText addressBar furi
            Nothing  -> return ()

    -- Open all link in current window.
    webView `on` createWebView $ \frame -> do
        newUri <- webFrameGetUri frame
        case newUri of
            Just uri -> do
                let furi = checkProtocol uri
                content <- fetchURL furi
                webViewLoadHtmlString webView content furi
            Nothing  -> return ()
        return webView        

    -- Connect and show.
    boxPackStart winBox addressBar PackNatural 0
    boxPackStart winBox scrollWin PackGrow 0
    window `containerAdd` winBox
    scrollWin `containerAdd` webView
    window `onDestroy` mainQuit
    widgetShowAll window

    mainGUI

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
        Just uri    -> if (uriScheme uri == "http:") then url else "http://google.com"
