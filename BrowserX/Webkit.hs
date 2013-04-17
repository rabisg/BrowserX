module BrowserX.Webkit (browser) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame

import BrowserX.Network

-- | Internal browser fucntion.
browser settings url = do
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
    home <- actionNew "HOME" "Home" (Just "") (Just stockHome)
    back <- actionNew "BACK" "Back" (Just "") (Just stockGoBack)
    forw <- actionNew "FORW" "Forward" (Just "") (Just stockGoForward)
    relo <- actionNew "RELO" "Reload" (Just "") (Just stockRedo)
    save <- actionNew "SAVE" "Save" (Just "") (Just stockSave)
    exit <- actionNew "EXIT" "Exit" (Just "") (Just stockQuit)

    agr <- actionGroupNew "AGR"
    mapM_ (\act -> actionGroupAddActionWithAccel agr act Nothing)[home,back,forw,relo,save,exit]

    ui <- uiManagerNew
    uiManagerAddUiFromString ui uiDecl
    uiManagerInsertActionGroup ui agr 0

    maybeToolbar <- uiManagerGetWidget ui "/ui/toolbar"
    let toolbar = case maybeToolbar of
                    (Just x) -> x
                    Nothing -> error "Cannot get toolbar"

    onActionActivate home (loadView webView addressBar settings "http://google.com")
    onActionActivate exit (widgetDestroy window)
    onActionActivate back (webViewGoBack webView)
    onActionActivate forw (webViewGoForward webView)
    onActionActivate relo (webViewReload webView)
    onActionActivate save (savedialog addressBar)

    -- Create scroll window.
    scrollWin <- scrolledWindowNew Nothing Nothing
    
    -- Load uri.
    do
        let furl = checkProtocol url
        loadView webView addressBar settings furl
    
    -- Open uri when user press `return` at address bar.
    onEntryActivate addressBar $ do
        uri <- entryGetText addressBar               -- get uri from address bar
        let furi = checkProtocol uri
        loadView webView addressBar settings furi
        
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
                loadView webView addressBar settings furi
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

loadView webView addressBar settings url = do
    content <- fetchURL settings url
    entrySetText addressBar url
    webViewLoadHtmlString webView content url

uiDecl=  "<ui>\
\           <toolbar>\
\            <toolitem action=\"HOME\" />\
\            <separator />\
\            <toolitem action=\"BACK\" />\
\            <toolitem action=\"FORW\" />\
\            <separator />\
\            <toolitem action=\"RELO\" />\
\            <separator />\
\            <toolitem action=\"SAVE\" />\
\            <separator />\
\            <toolitem action=\"EXIT\" />\
\           </toolbar>\
\          </ui>"

savedialog addressBar = do
     url <- entryGetText addressBar
     fchdal <- fileChooserDialogNew (Just "Save As..") Nothing
                                     FileChooserActionSave
                                     [("Cancel", ResponseCancel),
                                      ("Save", ResponseAccept)]
 
     fileChooserSetDoOverwriteConfirmation fchdal True
     widgetShow fchdal
     response <- dialogRun fchdal
     case response of
          ResponseCancel -> putStrLn "You cancelled."
          ResponseAccept -> do nwf <- fileChooserGetFilename fchdal
                               case nwf of
                                    Nothing -> putStrLn "Nothing"
                                    Just path -> downloadfile url path
          ResponseDeleteEvent -> putStrLn "You closed the dialog window."
     widgetDestroy fchdal
