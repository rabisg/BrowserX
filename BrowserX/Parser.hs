module BrowserX.Parser (parseHTML) where
import System.Cmd
import Data.Text.IO hiding (readFile)

parseHTML :: String -> IO String
parseHTML filePath = do
	system $ "cat " ++ filePath ++ " | w3m -dump -T text/html > /tmp/bro_output.txt"
	readFile "/tmp/bro_output.txt"