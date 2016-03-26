{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Create new paste controller.

module Hpaste.Controller.New
  (handle,NewStyle(..))
  where

import Hpaste.Types
import Hpaste.Controller.Paste (pasteForm,getPasteId)
import Hpaste.Model.Channel    (getChannels)
import Hpaste.Model.Language   (getLanguages)
import Hpaste.Model.Paste      (getPasteById, getLatestVersion)
import Hpaste.View.Annotate    as Annotate (page)
import Hpaste.View.Edit        as Edit (page)
import Hpaste.View.New         as New (page)

import Control.Applicative
import Data.Text.Encoding      (decodeUtf8)
import Snap.App

data NewStyle = NewPaste | AnnotatePaste | EditPaste
 deriving Eq

-- | Make a new paste.
handle :: NewStyle -> HPCtrl ()
handle style = do
  chans <- model $ getChannels
  langs <- model $ getLanguages
  defChan <- fmap decodeUtf8 <$> getParam "channel"
  pid <- if style == NewPaste then return Nothing else getPasteId
  case pid of
    Just pid -> do
      paste <- model $ getPasteById pid
      latest <- model $ traverse getLatestVersion paste
      let epaste | style == EditPaste = (,) <$> paste <*> latest
      	  	 | otherwise = Nothing
      let apaste | style == AnnotatePaste = (,) <$> paste <*> latest
                 | otherwise = Nothing
      form <- pasteForm chans langs defChan apaste epaste
      justOrGoHome paste $ \paste -> do
        case style of
          AnnotatePaste -> output $ Annotate.page paste form
	  EditPaste     -> output $ Edit.page paste form
	  _ -> goHome
    Nothing -> do
      form <- pasteForm chans langs defChan Nothing Nothing
      output $ New.page form
