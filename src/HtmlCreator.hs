{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
module HtmlCreator
    ( renderListingAsHtml
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
import Internal

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
