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
page PastePage{ppPaste=PasteContext{..},..} =
  layoutPage $ Page {
    pageTitle = case pcRevisions of
                  (rev:_) -> pasteTitle rev
                  _ -> pasteTitle pcPaste
  , pageBody = do viewPaste (if ppRevision then [] else pcRevisions)
    	       	  	    []
			    ppChans
			    ppLangs
			    (pcPaste, case pcRevisionHints of (hints:_) -> hints; _ -> pcHints)
                  viewAnnotations (pcPaste : pcAnnotations)
                                  ppChans
                                  ppLangs
                                  (zip pcAnnotations pcAnnotationHints)
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

-- | Get the paste id.
getPasteId :: PasteFormlet -> Maybe PasteId
getPasteId PasteFormlet{..} =
  M.lookup "id" pfParams >>=
  readMay . concat . map toString >>=
  return . PasteId

-- | View the paste's annotations.
viewAnnotations :: [Paste] -> [Channel] -> [Language] -> [(Paste,[Hint])] -> Markup
viewAnnotations pastes chans langs annotations = do
  mapM_ (viewPaste [] pastes chans langs) annotations

-- | View a paste's details and content.
viewPaste :: [Paste] -> [Paste] -> [Channel] -> [Language] -> (Paste,[Hint]) -> Markup
viewPaste revisions annotations chans langs (paste@Paste{..},hints) = do
  pasteDetails revisions annotations chans langs paste
  pasteContent revisions langs paste
  viewHints hints

-- | List the details of the page in a dark section.
pasteDetails :: [Paste] -> [Paste] -> [Channel] -> [Language] -> Paste -> Markup
pasteDetails revisions annotations chans langs paste =
  darkNoTitleSection $ do
    let title = case revisions of
                  (rev:_) -> pasteTitle rev
                  _ -> pasteTitle paste
    h2 $ a ! A.href (toValue ("#a" ++ show (pasteId paste)))
           ! A.id (toValue ("a" ++ show (pasteId paste)))
           ! A.name (toValue ("a" ++ show (pasteId paste)))
           $ toMarkup $ fromStrict title
    pasteNav annotations paste
    ul ! aClass "paste-specs" $ do
      detail "Paste" $ do
        pasteLink paste $ "#" ++ show (pasteId paste)
	" "
        linkToParent paste
      detail "Author(s)" $ do
        let authors | null revisions = map pasteAuthor [paste]
	    	    | otherwise      = map pasteAuthor revisions
        htmlCommasAnd $ flip map (nub authors) $ \author ->
	  linkAuthor author
      detail "Language" $ showLanguage langs (pasteLanguage paste)
      detail "Channel" $ showChannel (Just paste) chans (pasteChannel paste)
      detail "Created" $ showDateTime (pasteDate paste)
      unless (length revisions < 2) $ detail "Revisions" $ do
        br
        ul !. "revisions" $ listRevisions paste revisions
    clear

    where detail title content = do
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

-- | List the revisions of a paste.
listRevisions :: Paste -> [Paste] -> Markup
listRevisions _ [] = return ()
listRevisions p [x] = revisionDetails p x
listRevisions p (x:y:xs) = do
  revisionDetails y x
  listRevisions p (y:xs)

-- | List the details of a revision.
revisionDetails :: Paste -> Paste -> Markup
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
pasteNav :: [Paste] -> Paste -> Markup
pasteNav pastes paste =
  H.div ! aClass "paste-nav" $ do
    diffLink
    href ("/edit/" ++ pack (show pid) ++ "") ("Edit" :: Text)
    " - "
    href ("/annotate/" ++ pack (show pid) ++ "") ("Annotate" :: Text)
    " - "
    href ("/report/" ++ pack (show pid) ++ "") ("Report/Delete" :: Text)
    " - "
    pasteRawLink paste $ ("Raw" :: Text)

    " - "
    a ! hrefURI' (updateUrlParams [("title",T.unpack (pasteTitle paste))
                                 ,("paste","http://lpaste.net/raw/" ++ show (pasteId paste))]
                                 (fromJust (parseURI "https://fpcomplete.com/ide"))) $
      "Clone in IDE"

    where pid = pasteId paste
          pairs = zip (drop 1 pastes) pastes
          parent = fmap snd $ find ((==pid).pasteId.fst) $ pairs
          diffLink = do
            case listToMaybe pastes of
              Nothing -> return ()
              Just Paste{pasteId=parentId} -> do
                href ("/diff/" ++ show parentId ++ "/" ++ show pid)
                     ("Diff original" :: Text)
            case parent of
              Nothing -> return ()
              Just Paste{pasteId=prevId} -> do
	        when (pasteType paste /= AnnotationOf prevId) $ do
                  " / "
                  href ("/diff/" ++ show prevId ++ "/" ++ show pid)
                       ("prev" :: Text)
            case listToMaybe pastes of
              Nothing -> return ()
              Just{} -> " - "


hrefURI' :: URI -> Attribute
hrefURI' uri = A.href (toValue (show uri)) where

-- | Show the paste content with highlighting.
pasteContent :: [Paste] -> [Language] -> Paste -> Markup
pasteContent revisions langs paste =
  case revisions of
    (rev:_) -> lightNoTitleSection $ highlightPaste langs rev
    _ -> lightNoTitleSection $ highlightPaste langs paste

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
