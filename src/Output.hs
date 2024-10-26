{-# LANGUAGE OverloadedStrings #-}

module Output (printConversation) where

import BranchLogic
import Control.Monad
import qualified Data.Text as T
import Types

printConversation :: Conversation -> IO ()
printConversation x = forM_ (generateAllConvBranches x) $
  \branch -> do
    printFirstBranch branch
    putStrLn "\n----------------\n"

printFirstBranch :: Conversation -> IO ()
printFirstBranch (Conversation title content) = do
  putStrLn $ "<!-- " ++ T.unpack title ++ " -->"
  printSpeech content
  where
    printSpeech ConversationEnd = return ()
    printSpeech (ConversationBranch (Speech (speaker, say)) (y : _)) = do
      putStr $ "**" ++ T.unpack speaker ++ ":** "
      putStrLn $ T.unpack say ++ "\n"
      printSpeech y
    printSpeech (ConversationBranch NullSpeech (y : _)) = printSpeech y
    printSpeech _ = return ()
