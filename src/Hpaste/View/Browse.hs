{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Browse page view.

module Hpaste.View.Browse
  (page)
  where

import           Hpaste.Types
import           Hpaste.View.Html
import           Hpaste.View.Layout
import           Hpaste.View.Paste  (pasteLink)

import           Control.Monad
import           Data.Maybe
import           Data.Monoid.Operator
import           Data.Pagination
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Time
import           Data.Time.Relative
import           Network.URI
import           Network.URI.Params
import           Prelude            hiding ((++))
import           Snap.App.Types
import           Text.Blaze.Extra
import           Text.Blaze.Html5   as H hiding (map)
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Pagination

-- | Render the browse page.
page :: UTCTime -> PN -> [Channel] -> [Language] -> [(Paste, Paste)] -> Maybe String -> Html
page now pn chans langs ps mauthor =
  layoutPage $ Page {
    pageTitle = "Browse pastes"
  , pageBody = browse now pn chans langs ps mauthor
  , pageName = "browse"
  }

-- | View the paginated pastes.
browse :: UTCTime -> PN -> [Channel] -> [Language] -> [(Paste, Paste)] -> Maybe String -> Html
browse now pn channels languages ps mauthor = do
  darkSection title $ do
    pagination pn
    table ! aClass "latest-pastes" $ do
      tr $ mapM_ (th . (toHtml :: String -> Html)) $
	 ["Title"] ++ ["Author"|isNothing mauthor] ++ ["When","Language","Channel"]
      pastes ps
    pagination pn { pnPn = (pnPn pn) { pnShowDesc = False } }

    where pastes = mapM_ $ \(original, latest) -> tr $ do
                     td $ pasteLink original (pasteTitle latest)
                     unless (isJust mauthor) $
                       td $ do
                         let authorLatest   = T.unpack (pasteAuthor latest)
                             authorOriginal = T.unpack (pasteAuthor original)
                         if authorLatest == authorOriginal
                            then makeAuthorLink authorOriginal
                            else do toMarkup authorLatest
                                    " (original by "
                                    makeAuthorLink authorOriginal
                                    ")"
                     td $ ago (pasteDate original) now
                     td $ showLanguage languages (pasteLanguage latest)
                     td $ showChannel Nothing channels (pasteChannel latest)
          makeAuthorLink author
            | True {-validNick author-} = a ! hrefURI (authorUri author)
                                            $ toHtml author
            | otherwise                 = toHtml author
          authorUri author = updateUrlParam "author" author
	  	    	   $ updateUrlParam "pastes_page"   "0"
			   $ pnURI pn
          title = LT.pack $ case mauthor of
	    Just author -> "Pastes by " ++ author
	    Nothing -> "Latest pastes"

epoch = formatTime defaultTimeLocale "%s"

ago t1 t2 = H.span !. "relative-time"
                   ! dataAttribute "epoch" (toValue (epoch t1))
                   ! A.title (toValue (show t1)) $
   toHtml (relative t1 t2 True)
