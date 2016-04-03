{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | Paste model.

module Hpaste.Model.Paste
  (getLatestPastes
  ,getPasteById
  ,getPrivatePasteById
  ,createOrUpdate
  ,deletePaste
  ,createPaste
  ,getAnnotations
  ,getRevisions
  ,getLatestVersionById
  ,getLatestVersion
  ,getPaginatedPastes
  ,countPublicPastes
  ,generateHints
  ,getHints
  ,validNick)
  where

import Hpaste.Types
import Hpaste.Model.Announcer
import Hpaste.Model.Spam

import Data.Pagination
import Control.Applicative    ((<$>),(<|>))
import Control.Exception as E
import Control.Monad
import Control.Monad.Env
import Control.Monad.IO
import Data.Char
import Data.List              (find,intercalate)
import Data.Maybe
import Data.Monoid.Operator   ((++))
import Data.Text              (Text,unpack,pack)
import qualified Data.Text              as T
import Data.Text.IO           as T (writeFile)
import Data.Text.Lazy         (fromStrict)
import Language.Haskell.HLint
import Prelude                hiding ((++))
import Snap.App
import System.Directory
import System.FilePath

deletePaste :: Integer -> HPModel ()
deletePaste pid = void (exec ["DELETE FROM paste WHERE id = ?"] (Only pid))

-- | Count public pastes.
countPublicPastes :: Maybe String -> HPModel Integer
countPublicPastes mauthor = do
  rows <- single ["SELECT COUNT(*)"
                 ,"FROM public_toplevel_paste"
		 ,"WHERE (? IS NULL) OR (author = ?) AND spamrating < ?"]
		 (mauthor,mauthor,spamMinLevel)
  return $ fromMaybe 0 rows

-- | Get the latest pastes.
getLatestPastes :: Maybe ChannelId -> HPModel [Paste]
getLatestPastes channel =
  query ["SELECT ",pasteFields
	,"FROM public_toplevel_paste"
	,"WHERE spamrating < ?"
        ,"AND channel = ? or ? is null"
	,"ORDER BY created DESC"
	,"LIMIT 20"]
       (spamMinLevel,channel,channel)

-- | Get some paginated pastes.
getPaginatedPastes :: Maybe String -> Pagination -> HPModel (Pagination,[Paste])
getPaginatedPastes mauthor pn@Pagination{..} = do
  total <- countPublicPastes mauthor
  rows <- query ["SELECT",pasteFields
		,"FROM public_toplevel_paste"
		,"WHERE (? IS NULL) OR (author = ?) AND spamrating < ?"
		,"ORDER BY created DESC"
		,"OFFSET " ++ show (max 0 (pnCurrentPage - 1) * pnPerPage)
		,"LIMIT " ++ show pnPerPage]
		(mauthor,mauthor,spamMinLevel)
  return (pn { pnTotal = total },rows)

-- | Get a paste by its id.
getPasteById :: PasteId -> HPModel (Maybe Paste)
getPasteById pid =
  listToMaybe <$> query ["SELECT ",pasteFields
                        ,"FROM public_paste"
                        ,"WHERE id = ?"]
                        (Only pid)

-- | Get a private paste by its id, regardless of any status.
getPrivatePasteById :: PasteId -> HPModel (Maybe Paste)
getPrivatePasteById pid =
  listToMaybe <$> query ["SELECT",pasteFields
                        ,"FROM private_paste"
                        ,"WHERE id = ?"]
                        (Only pid)

-- | Get annotations of a paste.
getAnnotations :: PasteId -> HPModel [Paste]
getAnnotations pid =
  query ["SELECT",pasteFields
        ,"FROM public_paste"
        ,"WHERE annotation_of = ?"
        ,"ORDER BY created ASC"]
        (Only pid)

-- | Get revisions of a paste.
getRevisions :: PasteId -> HPModel [Paste]
getRevisions pid = do
  query ["SELECT",pasteFields
        ,"FROM public_paste"
        ,"WHERE revision_of = ? or id = ?"
        ,"ORDER BY created DESC"]
        (pid,pid)

-- | Get latest version of a paste by its id.
getLatestVersionById :: PasteId -> HPModel (Maybe Paste)
getLatestVersionById pid = traverse getLatestVersion =<< getPasteById pid

-- | Get latest version of a paste.
getLatestVersion :: Paste -> HPModel Paste
getLatestVersion paste = do
  revs <- getRevisions (pasteId paste)
  return $ case revs of
    (rev:_) -> rev
    _ -> paste

-- | Create a paste, or update an existing one.
createOrUpdate :: [Language] -> [Channel] -> PasteSubmit -> Integer -> Bool -> HPModel (Maybe PasteId)
createOrUpdate langs chans paste@PasteSubmit{..} spamrating public = do
  case pasteSubmitId of
    Nothing  -> createPaste langs chans paste spamrating public
    Just pid -> do updatePaste pid paste
                   return $ Just pid

-- | Create a new paste (possibly annotating an existing one).
createPaste :: [Language] -> [Channel] -> PasteSubmit -> Integer -> Bool -> HPModel (Maybe PasteId)
createPaste langs chans ps@PasteSubmit{..} spamrating public = do
  -- We need the title of the latest version of the paste for the
  -- announcement (the announcement has the form “<previous version's title>
  -- revised to <new version's title>”.
  prevTitle <- case ann_pid <|> rev_pid of
    Nothing  -> return Nothing
    Just pid -> fmap pasteTitle <$> getLatestVersionById pid
  pid <- generatePasteId public
  res <- single ["INSERT INTO paste"
                ,"(id,title,author,content,channel,language,annotation_of,revision_of,spamrating,public)"
                ,"VALUES"
                ,"(?,?,?,?,?,?,?,?,?,?)"
                ,"returning id"]
                (pid,pasteSubmitTitle,pasteSubmitAuthor,pasteSubmitPaste
                ,pasteSubmitChannel,pasteSubmitLanguage,ann_pid,rev_pid,spamrating,public)
  when (lang == Just "haskell") $ just res $ createHints ps
  just (pasteSubmitChannel >>= lookupChan) $ \chan ->
    just res $ \pid -> do
      when (spamrating < spamMinLevel) $
        announcePaste pasteSubmitType (channelName chan) ps prevTitle pid
  return (pasteSubmitId <|> res)

  where lookupChan cid = find ((==cid).channelId) chans
        lookupLang lid = find ((==lid).languageId) langs
        lang = pasteSubmitLanguage >>= (fmap languageName . lookupLang)
        just j m = maybe (return ()) m j
        ann_pid = case pasteSubmitType of AnnotationOf pid -> Just pid; _ -> Nothing
        rev_pid = case pasteSubmitType of RevisionOf pid -> Just pid; _ -> Nothing

-- | Generate a fresh unique paste id.
generatePasteId :: Bool -> HPModel PasteId
generatePasteId public = do
  result <- if public
               then single ["SELECT NEXTVAL('paste_id_seq')"] ()
               else single ["SELECT (RANDOM()*9223372036854775807) :: BIGINT"] ()
  case result of
    Just pid@(PasteId i) -> do
      result <- single ["SELECT TRUE FROM paste WHERE id = ?"] (Only pid)
      case result :: Maybe PasteId of
        Just pid -> generatePasteId public
        _        -> return pid

-- | Create the hints for a paste.
createHints :: PasteSubmit -> PasteId -> HPModel ()
createHints ps pid = do
  hints <- generateHintsForPaste ps pid
  forM_ hints $ \hint ->
    exec ["INSERT INTO hint"
         ,"(paste,type,content)"
         ,"VALUES"
         ,"(?,?,?)"]
         (pid
         ,suggestionSeverity hint
         ,show hint)

-- | Announce the paste.
announcePaste :: PasteType -> Text -> PasteSubmit -> Maybe Text -> PasteId -> HPModel ()
announcePaste ptype channel PasteSubmit{..} prevTitle pid = do
  conf <- env modelStateConfig
  unless (seemsLikeSpam pasteSubmitTitle || seemsLikeSpam pasteSubmitAuthor) $ do
    announcer <- env modelStateAnns
    io $ announce announcer pasteSubmitAuthor channel $ do
      nick ++ " " ++ verb ++ " “" ++ pasteSubmitTitle ++ "” at " ++ link conf
  where nick | validNick (unpack pasteSubmitAuthor) = pasteSubmitAuthor
             | otherwise = "“" ++ pasteSubmitAuthor ++ "”"
        link Config{..} = "http://" ++ pack configDomain ++ "/" ++ pid'
        pid' = case ptype of
	         NormalPaste -> showPid pid
                 AnnotationOf apid -> showPid apid ++ "#a" ++ showPid pid
                 RevisionOf apid -> showPid apid
        verb = case ptype of
          NormalPaste -> "pasted"
          AnnotationOf _ -> case prevTitle of
	    Just s  -> "annotated “" ++ s ++ "” with"
            Nothing -> "annotated a paste with"
          RevisionOf _ -> case prevTitle of
	    Just s  -> "revised “" ++ s ++ "”:"
            Nothing -> "revised a paste:"
        showPid (PasteId p) = pack $ show $ (p :: Integer)
        seemsLikeSpam = T.isInfixOf "http://"

-- | Is a nickname valid? Digit/letter or one of these: -_/\\;()[]{}?`'
validNick :: String -> Bool
validNick s = first && all ok s && length s > 0 where
  ok c = isDigit c || isLetter c || elem c ("-_/\\;()[]{}?`'" :: String)
  first = all (\c -> isDigit c || isLetter c) $ take 1 s

-- | Get hints for a Haskell paste from hlint.
generateHintsForPaste :: PasteSubmit -> PasteId -> HPModel [Suggestion]
generateHintsForPaste PasteSubmit{..} (PasteId pid) = io $
  E.catch (generateHints (show pid) pasteSubmitPaste)
          (\SomeException{} -> return [])

-- | Get hints for a Haskell paste from hlint.
generateHints :: FilePath -> Text -> IO [Suggestion]
generateHints pid contents = io $ do
  tmpdir <- getTemporaryDirectory
  let tmp = tmpdir </> pid ++ ".hs"
  exists <- doesFileExist tmp
  unless exists $ T.writeFile tmp $ contents
  !hints <- hlint [tmp,"--quiet","--ignore=Parse error"]
  removeFile tmp
  return hints

getHints :: PasteId -> HPModel [Hint]
getHints pid =
  query ["SELECT type,content"
        ,"FROM hint"
        ,"WHERE paste = ?"]
        (Only pid)

-- | Update an existing paste.
updatePaste :: PasteId -> PasteSubmit -> HPModel ()
updatePaste pid PasteSubmit{..} = do
  _ <- exec (["UPDATE paste"
             ,"SET"]
             ++
             [intercalate ", " (map set (words fields))]
             ++
             ["WHERE id = ?"])
            (pasteSubmitTitle
            ,pasteSubmitAuthor
            ,pasteSubmitPaste
            ,pasteSubmitLanguage
            ,pasteSubmitChannel
            ,pid)
  return ()

    where fields = "title author content language channel"
          set key = unwords [key,"=","?"]

pasteFields = "id,title,content,author,created,views,language,channel,annotation_of,revision_of"
