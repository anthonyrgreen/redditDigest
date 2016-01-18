{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
module Lib
    ( downloadSubredditToHtml
    , downloadSubredditToJson
    ) where

import qualified Network.HTTP.Conduit as C
import Network.HTTP.Client.Conduit
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Text.StringLike
import Control.Monad
import Prelude as P
import Control.Monad.Except
import Control.Monad.Reader
import HtmlCreator
import Internal
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
  articleReq  <- C.parseUrl $ articleCommentsUrl article
  articleData <- liftM C.responseBody $ httpLbs articleReq
  articleJSON <- hoistEither $ eitherDecode articleData
  comments    <- hoistEither $ parseEither parseCommentsSection articleJSON
  return $ article { aComments = comments }

getSubredditListing :: String -> ReaderT Manager (ExceptT String IO) Listing
getSubredditListing subreddit = do
  listingReq             <- C.parseUrl $ subredditUrl subreddit
  listingData            <- liftM C.responseBody $ httpLbs listingReq
  listingWithoutComments <- hoistEither $ eitherDecode listingData
  liftM Listing $ mapM getArticleComments (lListing listingWithoutComments)

writeListingToHtml :: String -> Listing -> IO ()
writeListingToHtml filename listing = B.writeFile filename renderedHtml
  where
    renderedHtml = renderListingAsHtml listing

downloadSubredditToHtml :: String -> String -> IO ()
downloadSubredditToHtml subreddit filename = result >>= \case
  Right listing -> writeListingToHtml filename listing
  Left error    -> P.putStrLn error
  where
    result = runExceptT . withManager . getSubredditListing $ subreddit

downloadSubredditToJson :: String -> String -> IO ()
downloadSubredditToJson subreddit filename = do
  subredditJson <- C.simpleHttp $ subredditUrl subreddit
  B.writeFile filename subredditJson
