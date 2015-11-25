{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Code highlighting.

module Hpaste.View.Highlight
 (highlightPaste
 ,highlightHaskell)
  where

import           Hpaste.Types
import           Hpaste.View.Html

import Data.Monoid
import           Control.Monad
import           Data.List                     (find)
import           Data.Monoid.Operator
import           Data.Text                     (Text,unpack,pack)
import qualified Data.Text                     as T
import           Language.Haskell.HsColour.CSS (hscolour)
import           Prelude                       hiding ((++))
import           Text.Blaze.Html5              as H hiding (map)
import qualified Text.Blaze.Html5.Attributes   as A

-- | Syntax highlight the paste.
highlightPaste :: [Language] -> Paste -> Html
highlightPaste langs Paste{..} =
  H.table ! aClass "code" $ do
    td ! aClass "line-nums" $ do
      pre $
        forM_ [1..length (T.lines pastePaste)] $ \i -> do
          let name = "line" ++ pack (show i)
          href ("#" ++ name) (toHtml i) ! A.id (toValue name) ! A.name (toValue name)
          "\n"
    td $
      case lang of
        Just (Language{languageName})
         | languageName == "literatehaskell" ->
           birdStyle pastePaste
         | elem languageName ["haskell","agda","idris","elm"] ->
          preEscapedString $ hscolour False 1 (unpack pastePaste)
        Just (Language{..}) ->
          pre $ code ! A.class_ (toValue $ "language-" ++ lang) $
            toHtml pastePaste
	    where lang | languageName == "elisp" = "lisp"
	    	       | otherwise = languageName
        _ ->
          pre $ toHtml pastePaste

  where lang = find ((==pasteLanguage) . Just . languageId) langs

highlightHaskell :: Text -> Html
highlightHaskell paste =
  H.table ! aClass "code" $
    td $ preEscapedString $ hscolour False 1 (unpack paste)

birdStyle :: Text -> Html
birdStyle = collect mempty (Right []) . map T.unpack . T.lines where
  collect doc acc (('>':(dropSpace -> hsline)):xs) =
    case acc of
      Right hslines -> collect doc (Right (hslines ++ hsline ++ "\n")) xs
      Left text -> collect (doc <> plaintext text) (Right (hsline ++ "\n")) xs
  collect doc acc (textline:xs) =
    case acc of
      Right hslines -> collect (doc <> highlight hslines) (Left (textline ++ "\n")) xs
      Left text -> collect doc (Left (text ++ textline ++ "\n")) xs
  collect doc acc [] =
    case acc of
      Right hslines -> doc <> highlight hslines
      Left text -> doc <> plaintext text
  highlight = preEscapedString . beaks . hscolour False 1
  plaintext = pre . toHtml
  dropSpace (' ':xs) = xs
  dropSpace xs = xs
  beaks x = "<pre class='bird-code'>" ++ unlines (map beakize (lines x)) ++ "</pre>" where
    beakize ('<':'p':'r':'e':'>':code) = "<pre><span class='beak'>&gt; </span>" ++ code
    beakize "</pre>" = "</pre>"
    beakize code = "<span class='beak'>&gt; </span>" ++ code
