module BranchLogic (generateAllSpeechBranches, generateAllTreeBranches, generateAllConvBranches) where

import Types

generateAllSpeechBranches :: ConversationTree -> [[Speech]]
generateAllSpeechBranches (ConversationBranch speech []) = [[speech]]
generateAllSpeechBranches (ConversationBranch speech children) = do
  let tmp = generateAllSpeechBranches =<< children
  map (speech :) tmp
generateAllSpeechBranches ConversationEnd = [[]]

generateAllTreeBranches :: ConversationTree -> [ConversationTree]
generateAllTreeBranches x = foo <$> generateAllSpeechBranches x
  where
    foo (b : bs) = ConversationBranch b [foo bs]
    foo [] = ConversationBranch NullSpeech []

generateAllConvBranches :: Conversation -> [Conversation]
generateAllConvBranches x = do
  let title = conversationTitle x
  convBranch <- generateAllTreeBranches $ conversationContent x
  return $ Conversation title convBranch
