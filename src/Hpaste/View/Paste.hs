{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Paste views.

module Hpaste.View.Paste
  (pasteFormlet
  ,page
  ,pasteLink
  ,pasteRawLink)
  where


import           Hpaste.Types
import           Hpaste.View.Highlight       (highlightPaste)
import           Hpaste.View.Hlint           (viewHints)
import           Hpaste.View.Html
import           Hpaste.View.Layout

import           Control.Applicative
import           Control.Arrow               ((&&&))
import           Control.Monad
import           Data.ByteString.UTF8        (toString)
import           Data.List                   (find,nub)
import qualified Data.Map                    as M
import           Data.Maybe
import Network.URI.Params
import Network.URI
import           Data.Monoid.Operator        ((++))
import           Data.Text                   (Text,pack)
import qualified Data.Text                   as T
import           Data.Text.Lazy              (fromStrict)
import           Data.Time.Show              (showDateTime)
import           Data.Traversable hiding (forM)

import           Prelude                     hiding ((++))
import           Safe                        (readMay)
import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5.Extra
import           Text.Blaze.Extra
import           Text.Formlet

-- | Render the page page.
page :: PastePage -> Markup
page PastePage{..} = do
  let paste = if not ppRevision
                then ppPaste
                else ppPaste { pcLatest         = pcOriginal ppPaste
                             , pcLatestHints    = pcOriginalHints ppPaste
                             , pcRevisions      = []
                             , pcRevisionsHints = [] }
  layoutPage $ Page {
    pageTitle = pasteTitle (pcLatest paste)
  , pageBody = do viewPaste ppChans ppLangs Nothing paste
                  viewAnnotations ppChans ppLangs paste
  , pageName = "paste"
  }

-- | A formlet for paste submission / annotateing.
pasteFormlet :: PasteFormlet -> (Formlet PasteSubmit,Markup)
pasteFormlet pf@PasteFormlet{..} =
  let form = postForm ! A.action (toValue action) $ do
        when pfSubmitted $
          when (not (null pfErrors)) $
            H.div ! aClass "errors" $
              mapM_ (p . toMarkup) pfErrors
        H.div !. "paste-buttons" $ do submitI "private" "Private" !. "private"
                                      " "
                                      submitI "public" "Public" !. "public"
        formletHtml (pasteSubmit pf) pfParams

  in (pasteSubmit pf,form)

  where action = case pfAnnotatePaste of
                   Just Paste{..} -> "/annotate/" ++ show (fromMaybe pasteId pasteParent)
                       where pasteParent = case pasteType of
                               AnnotationOf pid -> Just pid
                               _ -> Nothing
                   Nothing        ->
                     case pfEditPaste of
		       Just Paste{..} -> "/edit/" ++ show pasteId
		       Nothing -> "/new"


-- | Make a submit (captioned) button.
submitI :: Text -> Text -> Markup
submitI name caption =
  H.input ! A.type_ "submit"
          ! A.name (toValue name)
          ! A.value (toValue caption)


-- | The paste submitting formlet itself.
pasteSubmit :: PasteFormlet -> Formlet PasteSubmit
pasteSubmit pf@PasteFormlet{..} =
  PasteSubmit
    <$> pure (getPasteId pf)
    <*> pure (case pfAnnotatePaste of
    	       Just pid -> AnnotationOf (pasteId pid)
	       _ -> case pfEditPaste of
	         Just pid -> RevisionOf (pasteId pid)
		 _ -> NormalPaste)
    <*> defaulting "No title" (textPlaceholder "title" "Title" (annotateTitle <|> editTitle))
    <*> defaulting "Anonymous Coward" (textPlaceholder "author" "Author" Nothing)
    <*> parse (traverse lookupLang)
              (opt (dropPlace languages "language" (snd defChan)))
    <*> parse (traverse lookupChan)
              (opt (dropPlace channels "channel" (fst defChan)))
    <*> req (areaPlaceholder "paste" "Enter your code here" pfContent)
    <*> opt (wrap (H.div ! aClass "spam") (textInput "email" "Email" Nothing))

    where defaulting def = fmap swap where
    	    swap "" = def
	    swap x  = x
    	  channels = options channelName channelName pfChannels
          languages = options languageName languageTitle pfLanguages

          lookupLang slug = findOption ((==slug).languageName) pfLanguages languageId
          lookupChan slug = findOption ((==slug).channelName) pfChannels channelId

          defChan = maybe (fromMaybe "" (annotateChan <|> editChan)
	  	    	  ,fromMaybe "haskell" (annotateLanguage <|> editLanguage))
                          (channelName &&& makeChan . channelName)
                          (pfDefChan >>= findChan)
          findChan name = find ((==name).T.drop 1.channelName) pfChannels
	  makeChan "#haskell" = "haskell"
	  makeChan "#idris" = "idris"
	  makeChan "#agda" = "agda"
	  makeChan "#yesod" = "haskell"
	  makeChan "#emacs" = "elisp"
	  makeChan _ = ""

          annotateTitle = ((++ " (annotation)") . pasteTitle) <$> pfAnnotatePaste
          annotateLanguage = join (fmap pasteLanguage pfAnnotatePaste) >>= findLangById
          annotateChan = join (fmap pasteChannel pfAnnotatePaste) >>= findChanById

          editTitle = Nothing
          editLanguage = join (fmap pasteLanguage pfEditPaste) >>= findLangById
          editChan = join (fmap pasteChannel pfEditPaste) >>= findChanById

          findChanById id = channelName <$> find ((==id).channelId) pfChannels
          findLangById id = languageName <$> find ((==id).languageId) pfLanguages

-- | Make a text input formlet with a placeholder.
textPlaceholder :: Text -> Text -> Maybe Text -> Formlet Text
textPlaceholder name caption def =
  formlet name $ \value -> do
    input ! A.name (toValue name)
          ! A.value (toValue $ fromMaybe "" (value <|> def))
          ! A.placeholder (toValue caption)
          ! A.class_ "text"

-- | Make a textarea input with a placeholder.
areaPlaceholder :: Text -> Text -> Maybe Text -> Formlet Text
areaPlaceholder name caption def =
  formlet name $ \value -> do
    textarea ! A.placeholder (toValue caption) ! A.name (toValue name) $
      toHtml $ fromMaybe "" (value <|> def)

-- | Make a drop down input.
dropPlace :: [(Text,Text)] -> Text -> Text -> Formlet Text
dropPlace values name  def =
  formlet name $ \value -> do
    select ! A.name (toValue name) $
      forM_ values $ \(key,title) -> do
        let nonSelected = all ((/=value) . Just . fst) values
            defaulting = nonSelected && def == key
            selected
              | Just key == value = (! A.selected "selected")
              | defaulting        = (! A.selected "selected")
              | otherwise         = id
        selected $ option ! A.value (toValue key) $ toHtml title

-- | Get the id of the paste being edited/annotated.
getPasteId :: PasteFormlet -> Maybe PasteId
getPasteId PasteFormlet{..} =
  M.lookup "id" pfParams >>=
  readMay . concat . map toString >>=
  return . PasteId

-- | View the paste's annotations.
viewAnnotations :: [Channel] -> [Language] -> PasteContext -> Markup
viewAnnotations chans langs paste = do
  let annotations = pcAnnotations paste
      originalId  = pasteId (pcOriginal paste)
  let prevIds = originalId : map (pasteId.pcOriginal) annotations
  forM_ (zip prevIds annotations) $ \(prevId, ann) ->
    viewPaste chans langs (Just (prevId, originalId)) ann

-- | View the paste's details and content.
viewPaste
  :: [Channel]
  -> [Language]
  -> Maybe (PasteId, PasteId)   -- ^ (previous annotations's id, earliest id)
  -> PasteContext
  -> Markup
viewPaste chans langs mbAnnotationInfo paste = do
  pasteDetails chans langs mbAnnotationInfo paste
  pasteContent langs paste
  viewHints (pcLatestHints paste)

-- | List the details of the page in a dark section.
pasteDetails :: [Channel] -> [Language] -> Maybe (PasteId, PasteId) -> PasteContext -> Markup
pasteDetails chans langs mbAnnotationInfo paste =
  darkNoTitleSection $ do
    let latest   = pcLatest paste
        original = pcOriginal paste
    h2 $ a ! A.href (toValue ("#a" ++ show pid))
           ! A.id (toValue ("a" ++ show pid))
           ! A.name (toValue ("a" ++ show pid))
           $ toMarkup $ fromStrict (pasteTitle latest)
    pasteNav mbAnnotationInfo paste
    ul ! aClass "paste-specs" $ do
      detail "Paste" $ do
        revisionPidLink (pasteId latest)
	" "
        linkToParent latest
      detail "Author(s)" $ do
        let authors = map pasteAuthor (original : pcRevisions paste)
        htmlCommasAnd $ flip map (nub authors) $ \author ->
	  linkAuthor author
      detail "Language" $
        showLanguage langs (pasteLanguage latest)
      detail "Channel" $
        showChannel (Just latest) chans (pasteChannel latest)
      detail "Created" $
        showDateTime (pasteDate original)
      let revisions = pcRevisions paste
      unless (null revisions) $ detail "Revisions" $ do
        br
        ul !. "revisions" $ do
          zipWithM revisionDetails
            revisions
            (tail revisions ++ [pcOriginal paste])
          revisionDetails (pcOriginal paste) (pcOriginal paste)
    clear

    where pid = pasteId (pcOriginal paste)
          detail title content = do
            li $ do strong (title ++ ":"); toMarkup content

-- | Link to an author.
linkAuthor :: Text -> Markup
linkAuthor author = href ("/browse?author=" ++ author) $ toMarkup author

-- | Link to annotation/revision parents.
linkToParent :: Paste -> Markup
linkToParent paste = do
  case pasteType paste of
    NormalPaste -> return ()
    AnnotationOf pid -> do "(an annotation of "; pidLink pid; ")"
    RevisionOf pid -> do "(a revision of "; revisionPidLink pid; ")"

-- | List the details of a revision.
revisionDetails :: Paste -> Paste -> Markup
revisionDetails revision prev = li $ do
  toMarkup $ showDateTime (pasteDate revision)
  " "
  revisionPidLink (pasteId revision)
  unless (pasteId prev == pasteId revision) $ do
    " "
    href ("/diff/" ++ show (pasteId prev) ++ "/" ++ show (pasteId revision)) $
      ("(diff)" :: Markup)
  ": "
  toMarkup (pasteTitle revision)
  " ("
  linkAuthor (pasteAuthor revision)
  ")"

-- | Individual paste navigation.
pasteNav :: Maybe (PasteId, PasteId) -> PasteContext -> Markup
pasteNav mbAnnotationInfo paste =
  H.div ! aClass "paste-nav" $ do
    diffLink
    href ("/edit/" ++ pack (show pid) ++ "") ("Edit" :: Text)
    " - "
    href ("/annotate/" ++ pack (show pid) ++ "") ("Annotate" :: Text)
    " - "
    href ("/report/" ++ pack (show pid) ++ "") ("Report/Delete" :: Text)
    " - "
    pasteRawLink (pcOriginal paste) $ ("Raw" :: Text)

    " - "
    a ! hrefURI' (updateUrlParams [("title",T.unpack (pasteTitle (pcLatest paste)))
                                 ,("paste","http://lpaste.net/raw/" ++ show pid)]
                                 (fromJust (parseURI "https://fpcomplete.com/ide"))) $
      "Clone in IDE"

    where pid = pasteId (pcOriginal paste)
          diffLink = case mbAnnotationInfo of
            Nothing -> return ()
            Just (prevAnnId, parentPasteId) -> do
              href ("/diff/" ++ show parentPasteId ++ "/" ++ show pid)
                   ("Diff original" :: Text)
              when (prevAnnId /= parentPasteId) $ do
                " / "
                href ("/diff/" ++ show prevAnnId ++ "/" ++ show pid)
                     ("prev" :: Text)
              " - "

hrefURI' :: URI -> Attribute
hrefURI' uri = A.href (toValue (show uri)) where

-- | Show the paste content with highlighting.
pasteContent :: [Language] -> PasteContext -> Markup
pasteContent langs paste =
  lightNoTitleSection $ highlightPaste langs (pcLatest paste)

-- | The href link to a paste.
pasteLink :: ToMarkup html => Paste -> html -> Markup
pasteLink Paste{..} inner = href ("/" ++ show pasteId) inner

-- | The href link to a paste (by its pid).
pidLink :: PasteId -> Markup
pidLink pid = href ("/" ++ show pid) $ toMarkup $ "#" ++ show pid

-- | The href link to a specific revision of a paste.
revisionLink :: ToMarkup html => Paste -> html -> Markup
revisionLink Paste{..} inner = href ("/revision/" ++ show pasteId) inner

-- | The href link to a specific revision of a paste (by its pid).
revisionPidLink :: PasteId -> Markup
revisionPidLink pid = href ("/revision/" ++ show pid) $ toMarkup $ "#" ++ show pid

-- | The href link to a paste, raw content.
pasteRawLink :: ToMarkup html => Paste -> html -> Markup
pasteRawLink Paste{..} inner = href ("/raw/" ++ show pasteId) inner
