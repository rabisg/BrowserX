module BrowserX.Options (
  PrintOptions
  , Options(..)
  , parse
  , options
  ) where

import System.Exit
import System.Console.GetOpt
import Data.List

import Network.HTTP.Auth
import Network.URI
import Network.Browser

data PrintOptions =
  Header
  | Body
  | Both
  deriving Show

instance Show Authority where
  show a = case a of 
    AuthBasic{} -> "(Basic Auth:: Username: " ++ auUsername a ++ "  Password: " ++auPassword a ++ ")"
    _ -> "Digest Auth"

{--
  Not converting the proxy string into Proxy
  because of wide variety of type of results 
  returned by Proxy Module.
  //TODO: Handle proxy in network
--}
data Options = Options
  { optShowVersion :: Bool
  , optShowHelp    :: Bool
  , optAuth        :: Maybe Authority
  , optProxy       :: Maybe String
  , optPrint       :: PrintOptions
  , optDebug       :: Bool
  } deriving Show

defaultOptions     = Options
  { optShowVersion = False
  , optShowHelp    = False
  , optAuth        = Nothing
  , optProxy       = Nothing
  , optPrint       = Both
  , optDebug       = False
  }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v','?'] ["version"]
     (NoArg (\ opts -> opts { optShowVersion = True }))
     "show version number"
 , Option "a"     ["auth"]
     (ReqArg 
      (\ f opts -> let 
        (user,pass') = break (\s -> s==':') f 
        pass = if null pass' then [] else tail pass'
        in opts { optAuth = Just (AuthBasic "Basic Realm" user pass nullURI) } )
      "username:password")
     "HTTP Basic Authentication"
 , Option []      ["proxy"]
     (ReqArg (\ f opts -> opts { optProxy = Just f } ) "http://example.com:80 | none | AUTO")
     "Proxy"
 , Option "p"     ["print"]
     (ReqArg (\ f opts -> case head f of
                            'h' -> opts { optPrint = Header }
                            'b' -> opts { optPrint = Body }
                            _   -> opts
              ) "h|b")
     "Output Mode: headers|body. Defaults to BOTH"
 , Option "h"   ["help"]
     (NoArg  (\ opts -> opts { optShowHelp = True }))
     "Show help"
 , Option "d"   ["debug"]
     (NoArg  (\ opts -> opts { optDebug = True }))
     "Show help"
 ]

parse :: [String] -> IO (Options, [String])
parse argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: BrowserX [flags] [METHOD] URL [ITEM [ITEM]]"