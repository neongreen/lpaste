{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.List               (nub)
import Data.Maybe
import Data.Monoid.Operator    ((++))
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
        mbPaste <- model $ if isJust getPrivate
                              then getPrivatePasteById pid
                              else getPasteById pid
        for mbPaste $ \original -> model $ do
          originalHints <- getHints (pasteId original)
          latest <- getLatestVersion original
          latestHints <- getHints (pasteId latest)
          revisions <- getRevisions (pid)
          rhints <- mapM (getHints.pasteId) revisions
          chans <- getChannels
          langs <- getLanguages
          annotations <- getAnnotations (pid)
          annotations' <- for annotations $ \ann -> do
              pcOriginalHints  <- getHints (pasteId ann)
              pcLatest         <- getLatestVersion ann
              pcLatestHints    <- getHints (pasteId pcLatest)
              pcRevisions      <- getRevisions (pasteId ann)
              pcRevisionsHints <- mapM (getHints.pasteId) pcRevisions
              return PasteContext {
                pcOriginal    = ann,
                pcAnnotations = [],
                .. }
          let pasteContext = PasteContext {
                pcOriginal        = original,
                pcOriginalHints   = originalHints,
                pcLatest          = latest,
                pcLatestHints     = latestHints,
                pcRevisions       = revisions,
                pcRevisionHints   = rhints,
                pcAnnotations     = annotations' }
          return $ page PastePage {
            ppChans    = chans
          , ppLangs    = langs
          , ppPaste    = pasteContext
          , ppRevision = revision
          }
      justOrGoHome html outputText

-- | Control paste annotating / submission.
pasteForm :: [Channel] -> [Language] -> Maybe Text -> Maybe Paste -> Maybe Paste -> HPCtrl Html
pasteForm channels languages defChan annotatePaste editPaste = do
  params <- getParams
  submittedPrivate <- isJust <$> getParam "private"
  submittedPublic <- isJust <$> getParam "public"
  let mbOriginal = annotatePaste <|> editPaste
  mbLatest <- model $ traverse getLatestVersion mbOriginal
  -- If we're editing an annotation, we want to know the “parent paste”
  -- (e.g. if paste A has been annotated with B, and editPaste = B, then
  -- parentPasteId = A). This is because when when we're editing an
  -- annotation, after editing we want to redirect to the original paste, not
  -- the annotation.
  let parentPasteId = mbOriginal >>= \p ->
                        case pasteType p of
                          AnnotationOf x -> Just x
                          _ -> Nothing
  let formlet = PasteFormlet {
          pfSubmitted = submittedPrivate || submittedPublic
        , pfErrors    = []
        , pfParams    = params
        , pfChannels  = channels
        , pfLanguages = languages
        , pfDefChan   = defChan
        , pfAnnotatePaste = (,) <$> annotatePaste <*> mbLatest
        , pfEditPaste     = (,) <$> editPaste     <*> mbLatest
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
           pid <- model $ createPaste languages channels paste
                                      spamrating submittedPublic
           mapM_ redirectToPaste (parentPasteId <|> pid)
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
