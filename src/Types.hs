module Types
    ( Article (..)
    , Comment (..)
    , Listing (..)
    ) where

import Data.Text.Lazy

data Article = Article {
    aId :: Text
  , aUrl :: Text
  , aTitle :: Text
  , aAuthor :: Text
  , aText :: Text
  , aComments :: [Comment]
  } deriving Show

data Comment = Comment {
    cId :: Text
  , cAuthor :: Text
  , cText :: Text
  , cChildren :: [Comment]
  } deriving Show

newtype Listing = Listing {
    lListing :: [Article]
  } deriving Show
