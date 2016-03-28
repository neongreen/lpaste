{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Paste controller.

module Hpaste.Controller.Paste
  (handle
  ,pasteForm
  ,getPasteId
  ,getPasteIdKey
  ,withPasteKey)
  where

import Hpaste.Types
import Hpaste.Controller.Cache (cache,resetCache)
import Hpaste.Model.Channel    (getChannels)
import Hpaste.Model.Language   (getLanguages)
import Hpaste.Model.Paste
import Hpaste.Model.Spam
import Hpaste.Types.Cache      as Key
import Hpaste.View.Paste       (pasteFormlet,page)

import Control.Applicative
import Control.Monad           ((>=>))
import Control.Monad.IO
import Data.ByteString         (ByteString)
import Data.ByteString.UTF8    (toString)
import Data.Maybe
import Data.Monoid.Operator    ((++))
import Data.String             (fromString)
import Data.Text               (Text)
import Prelude                 hiding ((++))
import Safe
import Snap.App
import Text.Blaze.Html5        as H hiding (output)
import Text.Formlet

-- | Handle the paste page.
handle :: Bool -> HPCtrl ()
handle revision = do
  pid <- getPasteId
  justOrGoHome pid $ \(pid) -> do
      html <- cache (if revision then Key.Revision pid else Key.Paste pid) $ do
        getPrivate <- getParam "show_private"
        paste <- model $ if isJust getPrivate
	      	       	    then getPrivatePasteById (pid)
	      	       	    else getPasteById (pid)
        case paste of
          Nothing -> return Nothing
          Just paste -> do
            hints <- model $ getHints (pasteId paste)
            annotations <- model $ getAnnotations (pid)
            revisions <- model $ getRevisions (pid)
            ahints <- model $ mapM (getHints.pasteId) annotations
            rhints <- model $ mapM (getHints.pasteId) revisions
            chans <- model $ getChannels
            langs <- model $ getLanguages
            return $ Just $ page PastePage {
              ppChans       = chans
            , ppLangs       = langs
            , ppAnnotations = annotations
            , ppRevisions   = revisions
            , ppHints       = hints
            , ppPaste       = paste
            , ppAnnotationHints = ahints
            , ppRevisionsHints = rhints
	    , ppRevision = revision
            }
      justOrGoHome html outputText

-- | Control paste annotating / submission.
pasteForm :: [Channel] -> [Language] -> Maybe Text -> Maybe Paste -> Maybe Paste -> HPCtrl Html
pasteForm channels languages defChan annotatePaste editPaste = do
  params <- getParams
  submittedPrivate <- isJust <$> getParam "private"
  submittedPublic <- isJust <$> getParam "public"
  mbLatest <- model $ traverse getLatestVersion (annotatePaste <|> editPaste)
  let formlet = PasteFormlet {
          pfSubmitted = submittedPrivate || submittedPublic
        , pfErrors    = []
        , pfParams    = params
        , pfChannels  = channels
        , pfLanguages = languages
        , pfDefChan   = defChan
        , pfAnnotatePaste = annotatePaste
        , pfEditPaste = editPaste
	, pfContent = pastePaste <$> mbLatest
        }
      (getValue,_) = pasteFormlet formlet
      value = formletValue getValue params
      errors = either id (const []) value
      (_,html) = pasteFormlet formlet { pfErrors = errors }
      val = either (const Nothing) Just $ value
  case val of
    Nothing -> return ()
    Just PasteSubmit{pasteSubmitSpamTrap=Just{}} -> goHome
    Just paste -> do
      spamrating <- model $ spamRating paste
      if spamrating >= spamMaxLevel
      	 then goSpamBlocked
	 else do
	    resetCache Key.Home
	    maybe (return ()) (resetCache . Key.Paste) $ pasteSubmitId paste
	    pid <- model $ createPaste languages channels paste spamrating submittedPublic
	    maybe (return ()) redirectToPaste pid
  return html

-- | Go back to the home page with a spam indication.
goSpamBlocked :: HPCtrl ()
goSpamBlocked = redirect "/spam"

-- | Redirect to the paste's page.
redirectToPaste :: PasteId -> HPCtrl ()
redirectToPaste (PasteId pid) =
  redirect $ "/" ++ fromString (show pid)

-- | Get the paste id.
getPasteId :: HPCtrl (Maybe PasteId)
getPasteId = (fmap toString >=> (fmap PasteId . readMay)) <$> getParam "id"

-- | Get the paste id by a key.
getPasteIdKey :: ByteString -> HPCtrl (Maybe PasteId)
getPasteIdKey key = (fmap toString >=> (fmap PasteId . readMay)) <$> getParam key

-- | With the
withPasteKey :: ByteString -> (Paste -> HPCtrl a) -> HPCtrl ()
withPasteKey key with = do
  pid <- getPasteIdKey key
  justOrGoHome pid $ \(pid ) -> do
    paste <- model $ getPasteById pid
    justOrGoHome paste $ \paste -> do
      _ <- with paste
      return ()
