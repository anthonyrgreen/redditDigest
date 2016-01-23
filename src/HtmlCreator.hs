{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
module HtmlCreator
    ( renderListingAsHtml
    ) where

import qualified Data.ByteString.Lazy as B
import Clay
import Clay.Common
import Clay.List
import Clay.Geometry
import Clay.Size
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as T
import Control.Monad
import HTMLEntities.Decoder
import Types
import Prelude hiding (div)

mainColumnCenteredCSS :: Css
mainColumnCenteredCSS = div # byId "main-column" ? do
  marginLeft auto
  marginRight auto
  width $ pct 50

noBulletPointsCSS :: Css
noBulletPointsCSS = ul ? listStyleType none

articleCSS :: T.Text
articleCSS = render $ do
  noBulletPointsCSS
  mainColumnCenteredCSS

articleHtml :: Article -> H.Html
articleHtml article = do
  let articleTitle = aTitle article
  let articleId = aId article
  let articleText = aText article
  let articleComments = aComments article
  let articleAuthor = H.toHtml $ aAuthor article
  H.div $ do
    H.h3 $ makeAnchorLabel articleId articleTitle
    H.h6 articleAuthor
    H.p  $ H.preEscapedToHtml articleText
    unless (null articleComments) $ H.ul $ mapM_ commentHtml articleComments
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
    unless (null children) $ H.ul $ forM_ children $ \com ->
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

pageHtml :: H.ToMarkup a => Listing -> a -> H.Html
pageHtml listing title = H.docTypeHtml $ do
  H.head $ do
    H.title "Harmonious content"
    H.meta H.! HA.charset "UTF-8"
    H.style $ H.toHtml articleCSS
  H.body $
    H.div H.! HA.id "main-column" $ do
      H.h1 $ H.toHtml title
      H.br
      H.div $ tableOfContentsHtml listing
      H.div $ listingHtml listing

renderListingAsHtml :: H.ToMarkup a => Listing -> a ->  B.ByteString
renderListingAsHtml listing title = H.renderHtml $ pageHtml listing title
