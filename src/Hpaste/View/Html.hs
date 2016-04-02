{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | HTML-specific view functions.

module Hpaste.View.Html
  (aClass
  ,aClasses
  ,darkSection
  ,darkNoTitleSection
  ,lightSection
  ,lightNoTitleSection
  ,warnNoTitleSection
  ,errorNoTitleSection
  ,href
  ,clear
  ,showLanguage
  ,showChannel
  ,paginate
  ,preEscapedString)
  where

import           Hpaste.Types

import           Control.Arrow               ((&&&))
import           Control.Monad               (when)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid.Operator        ((++))
import           Data.Pagination
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as T
import           Network.URI.Params
import           Network.URI
import           Prelude                     hiding ((++))
import           Text.Blaze.Html5            as H hiding (map,nav)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Extra
import Snap.App.Types

-- | A class prefixed with nothing.
aClass :: AttributeValue -> Attribute
aClass name = A.class_ (name)

-- | A class prefixed with nothing.
aClasses :: [Text] -> Attribute
aClasses names = A.class_ $
  toValue $ T.intercalate " " $ names

-- | A warning section.
warnNoTitleSection :: Markup -> Markup
warnNoTitleSection inner =
  H.div ! aClasses ["section","section-warn"] $ do
    inner

-- | An error section.
errorNoTitleSection :: Markup -> Markup
errorNoTitleSection inner =
  H.div ! aClasses ["section","section-error"] $ do
    inner

-- | A dark section.
darkSection :: Text -> Markup -> Markup
darkSection title inner =
  H.div ! aClasses ["section","section-dark"] $ do
    h2 $ toMarkup title
    inner

-- | A dark section.
darkNoTitleSection :: Markup -> Markup
darkNoTitleSection inner =
  H.div ! aClasses ["section","section-dark"] $ do
    inner

-- | A light section.
lightSection :: Text -> Markup -> Markup
lightSection title inner =
  H.div ! aClasses ["section","section-light"] $ do
    h2 $ toMarkup title
    inner

-- | A light section with no title.
lightNoTitleSection :: Markup -> Markup
lightNoTitleSection inner =
  H.div ! aClasses ["section","section-light"] $ do
    inner

-- | An anchor link.
href :: (ToValue location,ToMarkup html) => location -> html -> Markup
href loc content = H.a ! A.href (toValue loc) $ toMarkup content

-- | A clear:both element.
clear :: Markup
clear = H.div ! aClass "clear" $ return ()

-- | Show a language.
showLanguage :: [Language] -> Maybe LanguageId -> Markup
showLanguage languages lid =
  toMarkup $ fromMaybe "-" (lid >>= (`lookup` langs))

    where langs = map (languageId &&& languageTitle) languages

-- | Show a channel.
showChannel :: Maybe PasteId -> [Channel] -> Maybe ChannelId -> Markup
showChannel pid channels lid = do
  toMarkup $ fromMaybe "-" chan
  case (pid,chan) of
    (Just pid,Just c) | c == "#haskell" -> do
      " "
      href ("http://ircbrowse.net/browse/haskell/?q=" ++ show pid) $
        ("Context in IRC logs" :: String)
    _ -> return ()

    where langs = map (channelId &&& channelName) channels
          chan = (lid >>= (`lookup` langs))

-- | Render results with pagination.
paginate :: URI -> Pagination -> Markup -> Markup
paginate uri pn inner = do
  nav uri pn True
  inner
  nav uri pn False

-- | Show a pagination navigation, with results count, if requested.
nav :: URI -> Pagination -> Bool -> Markup
nav uri pn@Pagination{..} showTotal = do
  H.div ! aClass "pagination" $ do
    H.div ! aClass "inner" $ do
      when (pnCurrentPage-1 > 0) $ navDirection uri pn (-1) "Previous"
      toMarkup (" " :: Text)
      when (pnTotal == pnPerPage) $ navDirection uri pn 1 "Next"
      when showTotal $ do
        br
        toMarkup $ results

    where results = unwords [show start ++ "—" ++ show end
                            ,"results of"
                            ,show pnTotal]
          start = 1 + (pnCurrentPage - 1) * pnTotal
          end = pnCurrentPage * pnTotal

-- | Link to change navigation page based on a direction.
navDirection :: URI -> Pagination -> Integer -> Text -> Markup
navDirection uri Pagination{..} change caption = do
  a ! hrefURI uri $ toMarkup caption

  where uri = updateUrlParam "page"
  	      		     (show (pnCurrentPage + change))
			     uri

-- -- | Migration function.
-- preEscapedString :: String -> Markup
-- preEscapedString = preEscapedToMarkup
