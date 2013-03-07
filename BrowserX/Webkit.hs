module BrowserX.Webkit (browser) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame

import BrowserX.Network

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
        content <- fetchURL url      
        webViewLoadHtmlString webView content url
        
    entrySetText addressBar url
    
    -- Open uri when user press `return` at address bar.
    onEntryActivate addressBar $ do
        uri <- entryGetText addressBar               -- get uri from address bar
        content <- fetchURL uri
        webViewLoadHtmlString webView content uri    -- load new uri
        
    -- Add current uri to address bar when load start.
    webView `on` loadStarted $ \frame -> do
        currentUri <- webFrameGetUri frame
        case currentUri of
            Just uri -> entrySetText addressBar uri
            Nothing  -> return ()

    -- Open all link in current window.
    webView `on` createWebView $ \frame -> do
        newUri <- webFrameGetUri frame
        case newUri of
            Just uri -> do
                content <- fetchURL uri
                webViewLoadHtmlString webView content uri
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