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
import           Data.Foldable               (for_)
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
                else ppPaste { pcLatest        = pcOriginal ppPaste
                             , pcLatestHints   = pcOriginalHints ppPaste
                             , pcRevisions     = []
                             , pcRevisionHints = [] }
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
                   Just (Paste{..}, _) -> "/annotate/" ++ show (fromMaybe pasteId pasteParent)
                       where pasteParent = case pasteType of
                               AnnotationOf pid -> Just pid
                               _ -> Nothing
                   Nothing        ->
                     case pfEditPaste of
		       Just (Paste{..}, _) -> "/edit/" ++ show pasteId
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
    	       Just (p, _) -> AnnotationOf (pasteId p)
	       _ -> case pfEditPaste of
	         Just (p, _) -> RevisionOf (pasteId p)
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

          annotateTitle = ((++ " (annotation)") . pasteTitle . snd) <$> pfAnnotatePaste
          annotateLanguage = pfAnnotatePaste >>= pasteLanguage.snd >>= findLangById
          annotateChan = pfAnnotatePaste >>= pasteChannel.snd >>= findChanById

          editTitle = pasteTitle.snd <$> pfEditPaste
          editLanguage = pfEditPaste >>= pasteLanguage.snd >>= findLangById
          editChan = pfEditPaste >>= pasteChannel.snd >>= findChanById

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

-- | Get the paste id.
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

-- | View a paste's details and content.
viewPaste
  :: [Channel]
  -> [Language]
  -> Maybe (PasteId, PasteId)
  -> PasteContext
  -> Markup
viewPaste chans langs annotationInfo paste = do
  pasteDetails chans langs annotationInfo paste
  pasteContent langs (pcLatest paste)
  viewHints (pcLatestHints paste)

-- | List the details of the page in a dark section.
pasteDetails
  :: [Channel]
  -> [Language]
  -> Maybe (PasteId, PasteId)
  -> PasteContext
  -> Markup
pasteDetails chans langs annotationInfo paste =
  darkNoTitleSection $ do
    let original  = pcOriginal paste
        latest    = pcLatest paste
        revisions = pcRevisions paste
    h2 $ a ! A.href (toValue ("#a" ++ show pid))
           ! A.id (toValue ("a" ++ show pid))
           ! A.name (toValue ("a" ++ show pid))
           $ toMarkup $ fromStrict (pasteTitle latest)
    pasteNav annotationInfo paste
    ul ! aClass "paste-specs" $ do
      detail "Paste" $ do
        pasteLink original $ "#" ++ show pid
	" "
        linkToParent original
      let authors = nub (map pasteAuthor (original:revisions))
      detail (if length authors > 1 then "Authors" else "Author") $
        htmlCommasAnd $ map linkAuthor authors
      detail "Language" $ showLanguage langs (pasteLanguage latest)
      detail "Channel" $ showChannel (Just pid) chans (pasteChannel latest)
      detail "Created" $ showDateTime (pasteDate original)
      unless (length revisions <= 1) $ detail "Revisions" $ do
        br
        ul !. "revisions" $
          -- E.g. if the revisions go like [r2,r1,orig], it would call
          -- revisionDetails for (r1 r2), (orig r1), and (orig orig)
          zipWithM_ revisionDetails
            (tail revisions ++ [pcOriginal paste])
            revisions
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
    RevisionOf pid -> do "(a revision of "; pidLink pid; ")"

-- | List the details of a revision.
revisionDetails
  :: Paste      -- ^ The previous version of the paste
  -> Paste      -- ^ The revised version
  -> Markup
revisionDetails paste revision = li $ do
  toMarkup $ showDateTime (pasteDate revision)
  " "
  revisionLink revision $ do "#"; toMarkup (show (pasteId revision))
  unless (pasteId paste == pasteId revision) $ do
    " "
    href ("/diff/" ++ show (pasteId paste) ++ "/" ++ show (pasteId revision)) $
      ("(diff)" :: Markup)
  ": "
  toMarkup (pasteTitle revision)
  " ("
  linkAuthor (pasteAuthor revision)
  ")"

-- | Individual paste navigation.
pasteNav
  :: Maybe (PasteId, PasteId)   -- ^ (previous annotations's id, parent's id)
  -> PasteContext
  -> Markup
pasteNav annotationInfo paste =
  H.div ! aClass "paste-nav" $ do
    diffLink
    href ("/edit/" ++ pack (show pid) ++ "") ("Edit" :: Text)
    " - "
    href ("/annotate/" ++ pack (show pid) ++ "") ("Annotate" :: Text)
    " - "
    href ("/report/" ++ pack (show pid) ++ "") ("Report/Delete" :: Text)
    " - "
    pasteRawLink (pcOriginal paste) ("Raw" :: Text)

    " - "
    a ! hrefURI' (updateUrlParams [("title",T.unpack (pasteTitle (pcLatest paste)))
                                 ,("paste","http://lpaste.net/raw/" ++ show pid)]
                                 (fromJust (parseURI "https://fpcomplete.com/ide"))) $
      "Clone in IDE"

    where pid = pasteId (pcOriginal paste)
          diffLink =
            case annotationInfo of
              Nothing -> return ()
              Just (prevId, parentId) -> do
                href ("/diff/" ++ show parentId ++ "/" ++ show pid)
                     ("Diff original" :: Text)
                when (prevId /= parentId) $ do
                  " / "
                  href ("/diff/" ++ show prevId ++ "/" ++ show pid)
                       ("prev" :: Text)
                " - "

hrefURI' :: URI -> Attribute
hrefURI' uri = A.href (toValue (show uri)) where

-- | Show the paste content with highlighting.
pasteContent :: [Language] -> Paste -> Markup
pasteContent langs paste = lightNoTitleSection $ highlightPaste langs paste

-- | The href link to a paste.
pasteLink :: ToMarkup html => Paste -> html -> Markup
pasteLink Paste{..} inner = href ("/" ++ show pasteId) inner

-- | The href link to a paste pid.
pidLink :: PasteId -> Markup
pidLink pid = href ("/" ++ show pid) $ toMarkup $ "#" ++ show pid

-- | The href link to a paste.
revisionLink :: ToMarkup html => Paste -> html -> Markup
revisionLink Paste{..} inner = href ("/revision/" ++ show pasteId) inner

-- | The href link to a paste, raw content.
pasteRawLink :: ToMarkup html => Paste -> html -> Markup
pasteRawLink Paste{..} inner = href ("/raw/" ++ show pasteId) inner
