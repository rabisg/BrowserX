import System.Process
import System.Environment
import System.Exit

import Control.Monad
import System.Console.GetOpt

import BrowserX.Webkit
import BrowserX.Network
import BrowserX.Options

 -- | 'main' runs the main program
main :: IO ()
main = do
  argv <- getArgs
  (settings, params) <- parse argv
  when (optShowHelp settings) $ putStrLn (usageInfo "Usage: BrowserX [flags] [METHOD] URL [ITEM [ITEM]]" options) >> exitSuccess
  when (optShowVersion settings) $ putStrLn "BrowserX: version: 0.1" >> exitSuccess
  putStrLn $ show settings
  putStrLn $ show params

console :: String -> IO String
console url = fetchURL url

