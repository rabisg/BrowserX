import Network.Browser
import Network.HTTP
import Graphics.UI.Gtk
 
 -- | 'main' runs the main program
main = do
    initGUI
    window <- windowNew
    set window [windowTitle := "Browser-X"]
    vb <- vBoxNew False 0
    containerAdd window vb

    hb <- hBoxNew False 0
    boxPackStart vb hb PackNatural 0

    txtfield <- entryNew
    boxPackStart hb txtfield PackNatural 5
    button <- buttonNew 
    set button [buttonLabel := "Go"]
    boxPackStart hb button PackNatural 0

    txtstack <- statusbarNew
    boxPackStart vb txtstack PackNatural 0
    id <- statusbarGetContextId txtstack "Line"

    widgetShowAll window

    onPressed button (fetchURL txtfield button txtstack id)
    onDestroy window mainQuit
    mainGUI

fetchURL :: Entry -> Button -> Statusbar -> ContextId -> IO ()
fetchURL fld b stk id = do
    txt <- entryGetText fld
    (_,rsp) <- browse $ do
        setAllowRedirects True
        request $ getRequest txt
    msgid <- statusbarPush stk id $ rspBody rsp
    return ()
