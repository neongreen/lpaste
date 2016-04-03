{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Annotate paste view.

module Hpaste.View.Annotate
  (page)
  where

import Hpaste.Types
import Hpaste.View.Html
import Hpaste.View.Layout

import Data.Monoid.Operator ((++))
import Prelude              hiding ((++))
import Text.Blaze.Html5     as H hiding (map)
import Data.Text
import Data.Text.Lazy (fromStrict)

-- | Render the create annotate paste page.
page :: Text -> Html -> Html
page pasteTitle form =
  layoutPage $ Page {
    pageTitle = "Annotate: " ++ pasteTitle
  , pageBody = lightSection ("Annotate: " ++ fromStrict pasteTitle) form
  , pageName = "annotate"
  }
