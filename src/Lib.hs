{-# LANGUAGE OverloadedStrings #-}

module Lib (someFunc) where

import ConversationParser
import Data.String.Conversions
import Output
import Scraper
import System.Environment (getArgs)
import System.Exit
import System.IO

errPrint :: String -> IO ()
errPrint = hPutStrLn stderr

errBye :: String -> IO ()
errBye message = do
  errPrint message
  errPrint "Program halted due to an error. Bye!"
  exitWith $ ExitFailure 1

someFunc :: IO ()
someFunc = do
  argv <- getArgs
  if null argv
    then
      errBye "The first argument must be a URL"
    else do
      test <- scrapeRawJSON $ head argv
      case test of
        Just x -> case getConversation (convertString x) of
          Right conv -> printConversation conv
          Left err -> errBye $ "Err: Parsing error. " ++ err
        Nothing -> errBye "Err: Failed to scrape the JSON file."
