{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw controller.

module Hpaste.Controller.Raw
  (handle)
  where

import Hpaste.Model.Paste   (getLatestVersionById)
import Hpaste.Types

import Control.Applicative
import Data.ByteString.UTF8 (toString)
import Data.Maybe
import Data.Text.Lazy       (fromStrict)
import Prelude              hiding ((++))
import Safe
import Snap.App

-- | Handle the paste page.
handle :: HPCtrl ()
handle = do
  pid <- (>>= readMay) . fmap (toString) <$> getParam "id"
  case pid of
    Nothing -> goHome
    Just (pid :: Integer) -> do
      mbPaste <- model $ getLatestVersionById (PasteId pid)
      case mbPaste of
        Nothing ->
          goHome
        Just paste -> do
          modifyResponse $ setContentType "text/plain; charset=UTF-8"
          outputText . fromStrict . pastePaste $ paste
