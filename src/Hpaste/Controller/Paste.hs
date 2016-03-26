{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.List               (nub)
import Data.String             (fromString)
import Data.Text               (Text)
import Data.Traversable        (for)
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
          Just pcOriginal -> model $ do
            chans <- getChannels
            langs <- getLanguages
            pcOriginalHints  <- getHints (pasteId pcOriginal)
            pcLatest         <- getLatestVersion pcOriginal
            pcLatestHints    <- getHints (pasteId pcLatest)
            pcRevisions      <- getRevisions (pasteId pcOriginal)
            pcRevisionsHints <- mapM (getHints.pasteId) pcRevisions
            annotations      <- getAnnotations (pasteId pcOriginal)
            pcAnnotations    <- for annotations $ \ann -> do
                pcOriginalHints  <- getHints (pasteId ann)
                pcLatest         <- getLatestVersion ann
                pcLatestHints    <- getHints (pasteId pcLatest)
                pcRevisions      <- getRevisions (pasteId ann)
                pcRevisionsHints <- mapM (getHints.pasteId) pcRevisions
                return PasteContext {
                    pcOriginal    = ann
                  , pcAnnotations = []
                  , .. }
            return $ Just $ page PastePage {
                ppChans    = chans
              , ppLangs    = langs
	      , ppRevision = revision
                -- Filling in all the pc* fields automatically
              , ppPaste    = PasteContext {..}
              }
      justOrGoHome html outputText

-- | Control paste annotating / submission.
--
-- We are passed the original and the latest version of the paste we're
-- editing/annotating. This is done for 2 reasons:
--
--   * Imagine that A has an annotation B1 which was later edited to B2. If
--   we're annotating B2, we want to attach the new annotation to A, not B1,
--   and so we would have to look up the “parent paste” of B2, but only B1
--   has a link to it.
--
--   * If we're editing an annotation, we want to redirect to the parent
--   paste, not the annotation. Again, we have to know what the parent paste
--   was, and for that we need the original paste.
pasteForm
  :: [Channel]
  -> [Language]
  -> Maybe Text
  -> Maybe (Paste, Paste)
  -> Maybe (Paste, Paste)
  -> HPCtrl Html
pasteForm channels languages defChan annotatePaste editPaste = do
  params <- getParams
  submittedPrivate <- isJust <$> getParam "private"
  submittedPublic <- isJust <$> getParam "public"
  let mbOriginal = fst <$> (annotatePaste <|> editPaste)
      mbLatest   = snd <$> (annotatePaste <|> editPaste)
  let parentPasteId = case mbOriginal of
                        Nothing -> Nothing
                        Just p  -> case pasteType p of
                          AnnotationOf x -> Just x
                          _ -> Nothing
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
	    mapM_ (resetCache . Key.Paste) $
              nub $ catMaybes [pasteSubmitId paste, parentPasteId]
	    pid <- model $ createPaste languages channels paste spamrating submittedPublic
	    maybe (return ()) redirectToPaste (parentPasteId <|> pid)
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
