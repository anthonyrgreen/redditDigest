{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
module Lib
    ( downloadSubredditToHtml
    , downloadSubredditToJson
    ) where

import qualified Network.HTTP.Conduit as C
import Network.HTTP.Client.Conduit
import qualified Data.ByteString.Lazy as B
import Text.StringLike
import Control.Monad
import Prelude as P
import Control.Monad.Except
import Control.Monad.Reader
import HtmlCreator
import Types
import JsonParser

subredditUrl :: StringLike a => a -> String
subredditUrl subreddit = fullUrl
  where
    fullUrl = "http://www.reddit.com/r/" ++ toString subreddit ++ "/hot.json"

articleCommentsUrl :: Article -> String
articleCommentsUrl article = fullUrl
  where
    articleId' = toString (aId article)
    fullUrl = "http://www.reddit.com/comments/" ++ articleId' ++ ".json"

hoistEither :: (Monad m) => Either s a -> ReaderT r (ExceptT s m) a
hoistEither = lift . ExceptT . return

getArticleComments :: Article -> ReaderT Manager (ExceptT String IO) Article
getArticleComments article = do
  articleReq      <- C.parseUrl $ articleCommentsUrl article
  articleRes      <- liftM C.responseBody $ httpLbs articleReq
  articleComments <- hoistEither $ parseComments articleRes
  return $ article { aComments = articleComments }

getSubredditListing :: String -> ReaderT Manager (ExceptT String IO) Listing
getSubredditListing subreddit = do
  listingReq             <- C.parseUrl $ subredditUrl subreddit
  listingRes             <- liftM C.responseBody $ httpLbs listingReq
  listingWithoutComments <- hoistEither $ parseListing listingRes
  liftM Listing $ mapM getArticleComments (lListing listingWithoutComments)

downloadSubredditToHtml :: String -> String -> String -> IO ()
downloadSubredditToHtml subreddit filename title = result >>= \case
  Right listing -> B.writeFile filename . renderListingAsHtml listing $ title
  Left error    -> P.putStrLn error
  where
    result = runExceptT . withManager . getSubredditListing $ subreddit

downloadSubredditToJson :: String -> String -> IO ()
downloadSubredditToJson subreddit filename = do
  subredditJson <- C.simpleHttp $ subredditUrl subreddit
  B.writeFile filename subredditJson
