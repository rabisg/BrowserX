#!/usr/bin/env runhaskell

{-# LANGUAGE TemplateHaskell #-}

import System.Process
import System.Environment
import System.Exit

import Control.Monad

import HFlags

import BrowserX.Webkit
import BrowserX.Network
 
 -- Defining Flags
defineFlag "h:help" False "Prints the help text"
defineFlag "d:debug" False "Renders output on the terminal (No Webkit)"

 -- | 'main' runs the main program
main :: IO ()
main = do
    _ <- $(initHFlags "Browser-X version: 0.1")
    when (flags_help) $ exitSuccess
    if flags_debug == True 
        then console "http://www.google.com" >>= putStrLn
        else browser "http://www.google.com"

--
console :: String -> IO String
console url = fetchURL url

