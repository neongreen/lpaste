{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Browse page controller.

module Hpaste.Controller.Browse
  (handle)
  where

import Hpaste.Types
import Hpaste.Model.Channel  (getChannels)
import Hpaste.Model.Language (getLanguages)
import Hpaste.Model.Paste    (getPaginatedPastes,countPublicPastes,getLatestVersion)
import Hpaste.View.Browse    (page)

import Control.Monad.IO
import Data.Time
import Text.Blaze.Pagination
import Snap.App

-- | Browse all pastes.
handle :: HPCtrl ()
handle = do
  pn <- getPagination "pastes"
  author <- getStringMaybe "author"
  (pn',pastes) <- model $ getPaginatedPastes author (pnPn pn)
  latestVersions <- mapM (model . getLatestVersion) pastes
  chans <- model getChannels
  langs <- model getLanguages
  now <- io getCurrentTime
  output $ page now pn { pnPn = pn' } chans langs (zip pastes latestVersions) author
