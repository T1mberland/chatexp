{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types where

import qualified Data.Text as T

data Speech
  = Speech (T.Text, T.Text) -- (Speaker, Content)
  | NullSpeech
  deriving (Show)

data ConversationTree
  = ConversationBranch Speech [ConversationTree]
  | ConversationEnd
  deriving (Show)

data Conversation = Conversation
  { conversationTitle :: T.Text,
    conversationContent :: ConversationTree
  }
  deriving (Show)
