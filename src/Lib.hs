{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lib
    ( convertSubredditToHtml
    , downloadSubredditJsonToFile
    ) where

import Network.HTTP.Conduit
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
--import Data.Text as L
import Data.Text.Lazy as L
import Data.Text.Lazy.Encoding as L
import Data.Text.Lazy.IO as L
import Data.Text.Lazy.Builder as L
import qualified Data.ByteString.Lazy as B
import Text.StringLike
import Control.Monad
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Prelude as P
import qualified System.IO as IO
import qualified System.Directory as D
import HTMLEntities.Decoder


data Article = Article {
    aId :: L.Text
  , aUrl :: L.Text
  , aTitle :: L.Text
  , aAuthor :: L.Text
  , aText :: L.Text
  , aComments :: [Comment]
  } deriving Show

data Comment = Comment {
    cId :: L.Text
  , cAuthor :: L.Text
  , cText :: L.Text
  , cChildren :: [Comment]
  } deriving Show

newtype Listing = Listing {
    lListing :: [Article]
  } deriving Show

instance FromJSON Article where
  parseJSON = withObject "Article" $ \obj -> do
    articleData <- obj .: "data"
    title <- articleData .:? "title" .!= "err: noTitle"
    text <- articleData .:? "selftext_html" .!= "err: noText"
    id <- articleData .: "id"
    url <- articleData .: "url"
    let title' = L.toLazyText . htmlEncodedText $ title
    let text' = L.toLazyText . htmlEncodedText $ text
    return $ Article id url title' undefined text' []

instance FromJSON Listing where
  parseJSON = withObject "Listing" $ \obj -> do
    articles <- mapM parseJSON <=< (.: "children") <=< (.: "data") $ obj
    return $ Listing articles

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \obj -> do
    commentData <- obj .: "data"
    author <- commentData .:? "author" .!= "err: noAuthor"
    id <- commentData .: "id"
    text <- commentData .:? "body_html" .!= "err: noText"
    repliesField <- commentData .:? "replies" .!= ""
    children <- case repliesField of
      String _ -> return []
      Object repliesObj -> parseCommentsRecursive (Object repliesObj)
      other -> typeMismatch' "Object" other
    let text' = L.toLazyText . htmlEncodedText $ text
    return $ Comment id author text' children

typeMismatch' :: String -- ^ The name of the type you are trying to parse.
             -> Value  -- ^ The actual value encountered.
             -> Parser a
typeMismatch' expected actual =
    fail $ "when expecting a " ++ expected ++ ", encountered " ++ name
  where
    name = case actual of
             Object x -> "Object instead: " ++ show (HM.toList x)
             Array _  -> "Array instead:"
             String _ -> "String instead:"
             Number _ -> "Number instead:"
             Bool _   -> "Boolean instead:"
             Null     -> "Null instead:"

parseCommentsRecursive :: Value -> Parser [Comment]
parseCommentsRecursive = withObject "commentReplies" $ \obj ->
  let parseComments = mapM parseJSON <=< (.: "children") <=< (.: "data") in
  withObject "CommentRoot" parseComments (Object obj)

parseCommentsSection :: Value -> Parser [Comment]
parseCommentsSection = withArray "CommentListing" $ \arr ->
  let commentRoot = arr V.! 1 in
  parseCommentsRecursive commentRoot

populateArticleComments :: Article -> IO (Either String Article)
populateArticleComments article = do
  articleContents <- simpleHttp $ articleCommentsUrl (aId article)
  return $ do
    articleJson <- eitherDecode articleContents
    comments <- parseEither parseCommentsSection articleJson
    return $ article { aComments = comments }

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

makeAnchorLabel :: Text -> Text -> H.Html
makeAnchorLabel anchor linkText = H.a H.! HA.name anchorLink $ anchorText
  where
    anchorLink = H.lazyTextValue anchor
    anchorText = H.preEscapedToHtml linkText

makeAnchorLink :: Text -> Text -> H.Html
makeAnchorLink anchorTo linkText = H.a H.! HA.href anchorLink $ anchorText
  where
    anchorLink = H.lazyTextValue $ L.cons '#' anchorTo
    anchorText = H.preEscapedToHtml linkText

tableOfContentsAnchor :: Text
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

makePage :: Listing -> B.ByteString
makePage listing = H.renderHtml $ pageHtml listing

subredditUrl :: StringLike a => a -> String
subredditUrl subreddit = fullUrl
  where
    subreddit' = toString subreddit
    fullUrl = "http://www.reddit.com/r/" ++ subreddit' ++ "/hot.json"

articleCommentsUrl :: StringLike a => a -> String
articleCommentsUrl articleId = fullUrl
  where
    articleId' = toString articleId
    fullUrl = "http://www.reddit.com/comments/" ++ articleId' ++ ".json"

downloadSubredditJsonToFile :: String -> String -> IO ()
downloadSubredditJsonToFile subreddit filename = do
  subredditJson <- simpleHttp $ subredditUrl subreddit
  B.writeFile filename subredditJson

convertSubredditToHtml :: String -> String -> IO ()
convertSubredditToHtml subreddit filename = do
  subredditJson <- simpleHttp $ subredditUrl subreddit
  listingWithComments <- case eitherDecode subredditJson of
    Left err -> return $ Left err
    Right listingNoComments -> let listingNoComments' = lListing listingNoComments in
                               sequence <$> forM listingNoComments' populateArticleComments
  let listingWithComments' = Listing <$> listingWithComments
  writeListingToHtml filename listingWithComments'

writeListingToHtml :: String -> Either String Listing -> IO ()
writeListingToHtml filename listing = case listing of
  Right result -> B.writeFile filename $ makePage result
  Left error   -> P.putStrLn error

--convertSubredditToHtml :: String -> String -> IO ()
--convertSubredditToHtml subreddit filename = do
--  let subredditUrl = "http://www.reddit.com/r/" ++ subreddit ++ "/hot.json"
--  subredditJson <- simpleHttp subredditUrl
--  writeListingToHtml filename $ eitherDecode subredditJson
