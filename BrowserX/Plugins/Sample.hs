module BrowserX.Plugins.Sample(hook_scss) where

import Text.XML.HXT.Core
import Text.HandsomeSoup

hook_scss :: String -> String
hook_scss html = html ++ "\n\n ***********I'm inside the plugin************\n\n"