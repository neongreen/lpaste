{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Edit paste view.

module Hpaste.View.Edit
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

-- | Render the create edit paste page.
page :: Text -> Html -> Html
page pasteTitle form =
  layoutPage $ Page {
    pageTitle = "Edit: " ++ pasteTitle
  , pageBody = lightSection ("Edit: " ++ fromStrict pasteTitle) form
  , pageName = "edit"
  }
