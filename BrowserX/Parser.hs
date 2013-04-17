module BrowserX.Parser (parseHTML) where

import System.IO
import System.Cmd (system)

import Data.String.Utils (replace)

parseHTML :: String -> IO String
parseHTML html = do
  writeFile inpFile $ bold html
  system $ "cat " ++ inpFile ++ " | w3m -dump -T text/html > " ++ outFile
  readFile outFile
  where
    inpFile = "/tmp/bro_input.html"
    outFile = "/tmp/bro_output.html"

bold :: String -> String
bold inp = replace "</b>" "\\033[0m" $ replace "<b>" "\\033[1m" inp

anchor :: String -> String
anchor inp = replace "</a>" "\\033[0m" $ replace "<b>" "\\033[4m" inp