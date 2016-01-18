{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
module JsonParser
    ( parseCommentsSection
    ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as T
import Control.Monad
import HTMLEntities.Decoder
import Internal

instance FromJSON Article where
  parseJSON = withObject "Article" $ \obj -> do
    articleData <- obj .: "data"
    id          <- articleData .: "id"
    url         <- articleData .: "url"
    title       <- articleData .:? "title"         .!= "err: noTitle"
    text        <- articleData .:? "selftext_html" .!= "err: noText"
    let title' = T.toLazyText . htmlEncodedText $ title
    let text'  = T.toLazyText . htmlEncodedText $ text
    return $ Article id url title' undefined text' []

instance FromJSON Listing where
  parseJSON = withObject "Listing" $ \obj -> do
    articles <- mapM parseJSON <=< (.: "children") <=< (.: "data") $ obj
    return $ Listing articles

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \obj -> do
    commentData  <- obj .: "data"
    id           <- commentData .: "id"
    author       <- commentData .:? "author"    .!= "err: noAuthor"
    text         <- commentData .:? "body_html" .!= "err: noText"
    repliesField <- commentData .:? "replies"   .!= String ""
    children <- case repliesField of
      Object repliesObj -> parseCommentsRecursive (Object repliesObj)
      String _          -> return []
    let text' = T.toLazyText . htmlEncodedText $ text
    return $ Comment id author text' children

parseCommentsRecursive :: Value -> Parser [Comment]
parseCommentsRecursive = withObject "commentReplies" $ \obj ->
  let parseComments = mapM parseJSON <=< (.: "children") <=< (.: "data") in
  withObject "CommentRoot" parseComments (Object obj)

parseCommentsSection :: Value -> Parser [Comment]
parseCommentsSection = withArray "CommentListing" $ \arr ->
  let commentRoot = arr V.! 1 in
  parseCommentsRecursive commentRoot
