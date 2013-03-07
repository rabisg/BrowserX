import System.Process
import System.Environment
import System.Exit

import Control.Monad
import System.Console.GetOpt

import BrowserX.Webkit
import BrowserX.Network

import Data.Maybe ( fromMaybe )

data PrintOptions =
  Header
  | Body
  | Both

instance Show PrintOptions where
  show p = case p of
    Header -> "Header"
    Body   -> "Body"
    _      -> "Both"

data Options = Options
  { optShowVersion :: Bool
  , optShowHelp    :: Bool
  , optAuth        :: Maybe String
  , optProxy       :: Maybe String
  , optPrint       :: PrintOptions
  } deriving Show

defaultOptions     = Options
  { optShowVersion = False
  , optShowHelp    = False
  , optAuth        = Nothing
  , optProxy       = Nothing
  , optPrint       = Both
  }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v','?'] ["version"]
     (NoArg (\ opts -> opts { optShowVersion = True }))
     "show version number"
 , Option ['a']     ["auth"]
     (ReqArg (\ f opts -> opts { optAuth = Just f }) "username:password")
     "HTTP Basic Authentication"
 , Option []      ["proxy"]
     (ReqArg (\ f opts -> opts { optProxy = Just f }) "http://user:pass@example.com:80")
     "Proxy"
 , Option ['p']     ["print"]
     (ReqArg (\ f opts -> case head f of
                            'h' -> opts { optPrint = Header }
                            'b' -> opts { optPrint = Body }
                            _   -> opts
              ) "h|b")
     "Output Mode: headers|body. Defaults to BOTH"
 , Option ['h']   ["help"]
     (NoArg  (\ opts -> opts { optShowHelp = True }))
     "Show help"
 ]

parse :: [String] -> IO (Options, [String])
parse argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: BrowserX [flags] [METHOD] URL [ITEM [ITEM]]"

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

