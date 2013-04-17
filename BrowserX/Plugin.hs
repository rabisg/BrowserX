module BrowserX.Plugin (plugin) where

import Control.Monad
import Language.Haskell.Interpreter

plugin :: String -> String -> IO String
plugin hook d = do r <- runInterpreter $ processHooks hook d
                   case r of
                      Left err -> return $ show err
                      Right str -> return str


processHooks :: String -> String -> Interpreter String
processHooks hook d =
    do
      loadModules ["BrowserX/Plugins/Sample.hs"]
      setTopLevelModules ["BrowserX.Plugins.Sample"]
      setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M")]
      let exp3 = "hook_"++ hook ++ " " ++ show d
      interpret exp3 (as:: String)