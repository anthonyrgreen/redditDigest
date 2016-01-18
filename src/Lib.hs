{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
module Lib
    ( downloadSubredditToHtml
    , downloadSubredditToJson
    ) where

import qualified Network.HTTP.Conduit as C
import Network.HTTP.Client.Conduit
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.ByteString.Lazy as B
import Text.StringLike
import Control.Monad
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Prelude as P
import qualified System.IO as IO
import HTMLEntities.Decoder
import Control.Monad.Except
import Control.Monad.Reader



data Article = Article {
    aId :: T.Text
  , aUrl :: T.Text
  , aTitle :: T.Text
  , aAuthor :: T.Text
  , aText :: T.Text
  , aComments :: [Comment]
  } deriving Show

data Comment = Comment {
    cId :: T.Text
  , cAuthor :: T.Text
  , cText :: T.Text
  , cChildren :: [Comment]
  } deriving Show

newtype Listing = Listing {
    lListing :: [Article]
  } deriving Show

articleHtml :: Article -> H.Html
articleHtml article = do
  let articleTitle = aTitle article
  let articleId = aId article
  let articleText = aText article
  let articleComments = aComments article
  H.div $ do
    H.h3 $ makeAnchorLabel articleId articleTitle
    H.p  $ H.preEscapedToHtml articleText
    unless (P.null articleComments) $ H.ul $ mapM_ commentHtml articleComments
    H.p  $ makeAnchorLink articleId "back to top of story"
    H.p  $ makeAnchorLink tableOfContentsAnchor "back to table of contents"
  H.br

commentHtml :: Comment -> H.Html
commentHtml comment = do
  let author = H.preEscapedToHtml . cAuthor $ comment
  let text = H.preEscapedToHtml . cText $ comment
  let children = cChildren comment
  H.li $ do
    H.h6 author
    H.p text
    unless (P.null children) $ H.ul $ forM_ children $ \com ->
      commentHtml com

listingHtml :: Listing -> H.Html
listingHtml listing = do
  H.h2 "Listing:"
  H.div $ mapM_ articleHtml $ lListing listing

makeAnchorLabel :: T.Text -> T.Text -> H.Html
makeAnchorLabel anchor linkText = H.a H.! HA.name anchorLink $ anchorText
  where
    anchorLink = H.lazyTextValue anchor
    anchorText = H.preEscapedToHtml linkText

makeAnchorLink :: T.Text -> T.Text -> H.Html
makeAnchorLink anchorTo linkText = H.a H.! HA.href anchorLink $ anchorText
  where
    anchorLink = H.lazyTextValue $ T.cons '#' anchorTo
    anchorText = H.preEscapedToHtml linkText

tableOfContentsAnchor :: T.Text
tableOfContentsAnchor = "tableofcontents"

tableOfContentsHtml :: Listing -> H.Html
tableOfContentsHtml listing = do
  H.h3 $ makeAnchorLabel tableOfContentsAnchor "Table of contents:"
  H.ul $ forM_ (lListing listing) $ \article ->
    H.li $ makeAnchorLink (aId article) (aTitle article)

pageHtml :: Listing -> H.Html
pageHtml listing = H.docTypeHtml $ do
  H.head $ do
    H.title "Harmonious content"
    H.meta H.! HA.charset "UTF-8"
  H.body $ do
    H.h1 "askreddit"
    H.br
    H.div $ tableOfContentsHtml listing
    H.div $ listingHtml listing

renderListingAsHtml :: Listing -> B.ByteString
renderListingAsHtml listing = H.renderHtml $ pageHtml listing

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
