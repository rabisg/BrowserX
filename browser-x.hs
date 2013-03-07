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
    webViewSetMaintainsBackForwardList webView True

    -- Create window boxes.
    winBox <- vBoxNew False 0
    topBox <- hBoxNew False 0

    -- Create address bar.
    addressBar <- entryNew

    -- Create toolbar.
    back <- actionNew "BACK" "Back" (Just "") (Just stockGoBack)
    forw <- actionNew "FORW" "Forward" (Just "") (Just stockGoForward)
    relo <- actionNew "RELO" "Reload" (Just "") (Just stockRedo)
    exit <- actionNew "EXIT" "Exit" (Just "") (Just stockQuit)

    agr <- actionGroupNew "AGR"
    mapM_ (\act -> actionGroupAddActionWithAccel agr act Nothing)[back,forw,relo,exit]

    ui <- uiManagerNew
    uiManagerAddUiFromString ui uiDecl
    uiManagerInsertActionGroup ui agr 0

    maybeToolbar <- uiManagerGetWidget ui "/ui/toolbar"
    let toolbar = case maybeToolbar of
                    (Just x) -> x
                    Nothing -> error "Cannot get toolbar"

    onActionActivate exit (widgetDestroy window)
    onActionActivate back (webViewGoBack webView)
    onActionActivate forw (webViewGoForward webView)
    onActionActivate relo (webViewReload webView)

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
    boxPackStart topBox addressBar PackGrow 0
    boxPackStart topBox toolbar PackGrow 0
    boxPackStart winBox topBox PackNatural 0
    boxPackStart winBox scrollWin PackGrow 0
    window `containerAdd` winBox
    scrollWin `containerAdd` webView
    window `onDestroy` mainQuit
    widgetShowAll window

    mainGUI

uiDecl=  "<ui>\
\           <toolbar>\
\            <toolitem action=\"BACK\" />\
\            <toolitem action=\"FORW\" />\
\            <separator />\
\            <toolitem action=\"RELO\" />\
\            <separator />\
\            <toolitem action=\"EXIT\" />\
\           </toolbar>\
\          </ui>"

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
