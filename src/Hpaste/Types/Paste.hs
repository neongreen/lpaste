{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The paste type.

module Hpaste.Types.Paste
       (PasteContext(..)
       ,Paste(..)
       ,PasteType(..)
       ,PasteSubmit(..)
       ,PasteFormlet(..)
       ,ExprFormlet(..)
       ,PastePage(..)
       ,StepsPage(..)
       ,Hint(..)
       ,ReportFormlet(..)
       ,ReportSubmit(..))
       where

import Hpaste.Types.Newtypes
import Hpaste.Types.Language
import Hpaste.Types.Channel
import Control.Applicative
import Blaze.ByteString.Builder                (toByteString)
import Blaze.ByteString.Builder.Char.Utf8      as Utf8 (fromString)
import Data.Text                               (Text,pack)
import Data.Time                               (UTCTime,zonedTimeToUTC)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Language.Haskell.HLint                  (Severity)
import Snap.Core
import Text.Blaze                              (ToMarkup(..),toMarkup)
import Text.Blaze.Html5                        (Markup)

-- | A paste together with its revisions, annotations, and hints
data PasteContext = PasteContext {
   -- The original version of the paste (note that we usually refer to the
   -- paste by the id of the earliest version, not of the latest version)
   pcOriginal        :: Paste
  ,pcOriginalHints   :: [Hint]
   -- The latest revision of the paste
  ,pcLatest          :: Paste
  ,pcLatestHints     :: [Hint]
   -- The revisions go from latest to earliest
  ,pcRevisions       :: [Paste]
  ,pcRevisionHints   :: [[Hint]]
   -- The annotations go from earliest to latest
  ,pcAnnotations     :: [PasteContext]
}

-- | A paste.
data Paste = Paste {
   pasteId       :: PasteId
  ,pasteTitle    :: Text
  ,pasteDate     :: UTCTime
  ,pasteAuthor   :: Text
  ,pasteLanguage :: Maybe LanguageId
  ,pasteChannel  :: Maybe ChannelId
  ,pastePaste    :: Text
  ,pasteViews    :: Integer
  ,pasteType     :: PasteType
} deriving Show

instance ToMarkup Paste where
  toMarkup paste@Paste{..} = toMarkup $ pack $ show paste

instance FromRow Paste where
  fromRow = do
    (pid,title,content,author,date,views,language,channel,annotation_of,revision_of) <- fromRow
    return $ Paste
      { pasteTitle = title
      , pasteAuthor = author
      , pasteLanguage = language
      , pasteChannel = channel
      , pastePaste = content
      , pasteDate = zonedTimeToUTC date
      , pasteId = pid
      , pasteViews = views
      , pasteType = case annotation_of of
	Just pid' -> AnnotationOf pid'
	_ -> case revision_of of
	  Just pid' -> RevisionOf pid'
	  _ -> NormalPaste
      }

-- | The type of a paste.
data PasteType
  = NormalPaste
  | AnnotationOf PasteId
  | RevisionOf PasteId
  deriving (Eq,Show)

-- | A paste submission or annotate.
data PasteSubmit = PasteSubmit {
   pasteSubmitId       :: Maybe PasteId
  ,pasteSubmitType     :: PasteType
  ,pasteSubmitTitle    :: Text
  ,pasteSubmitAuthor   :: Text
  ,pasteSubmitLanguage :: Maybe LanguageId
  ,pasteSubmitChannel  :: Maybe ChannelId
  ,pasteSubmitPaste    :: Text
  ,pasteSubmitSpamTrap :: Maybe Text
} deriving Show

data PasteFormlet = PasteFormlet {
   pfSubmitted :: Bool
 , pfErrors    :: [Text]
 , pfParams    :: Params
 , pfLanguages :: [Language]
 , pfChannels  :: [Channel]
 , pfDefChan   :: Maybe Text
 , pfAnnotatePaste :: Maybe (Paste, Paste)   -- original, latest
 , pfEditPaste :: Maybe (Paste, Paste)
 , pfContent :: Maybe Text
}

data ExprFormlet = ExprFormlet {
   efSubmitted :: Bool
 , efParams    :: Params
}

data PastePage = PastePage {
    ppPaste           :: PasteContext
  , ppChans           :: [Channel]
  , ppLangs           :: [Language]
  , ppRevision        :: Bool
}

data StepsPage = StepsPage {
    spPaste           :: Paste
  , spChans           :: [Channel]
  , spLangs           :: [Language]
  , spHints           :: [Hint]
  , spSteps           :: [Text]
  , spAnnotations     :: [Paste]
  , spAnnotationHints :: [[Hint]]
  , spForm :: Markup
}

instance ToField Severity where
  toField = toField . show

--  render = Escape . toByteString . Utf8.fromString . show
--  {-# INLINE render #-}

instance FromField Severity where
  fromField x y = fmap read (fromField x y)
  {-# INLINE fromField #-}

-- | A hlint (or like) suggestion.
data Hint = Hint {
   hintType    :: Severity
 , hintContent :: String
}

instance FromRow Hint where
  fromRow = Hint <$> field <*> field

-- instance QueryResults Hint where
--   convertResults field values = Hint {
--       hintType = severity
--     , hintContent = content
--     }
--     where (severity,content) = convertResults field values

data ReportFormlet = ReportFormlet {
   rfSubmitted :: Bool
 , rfParams    :: Params
}

data ReportSubmit = ReportSubmit {
   rsPaste :: PasteId
  ,rsComments :: String
}
