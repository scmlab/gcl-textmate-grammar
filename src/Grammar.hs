{-# LANGUAGE OverloadedStrings #-}

module Grammar where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Map (Map, foldrWithKey)
import qualified Data.Map as Map
import Data.Text (Text, pack)

-- | Top level structure
data Grammar = Grammar
  { grammarScopeName :: Text,
    grammarFileTypes :: [Text],
    grammarFoldingStartMarker :: Maybe Text,
    grammarFoldingStopMarker :: Maybe Text,
    grammarFirstLineMatch :: Maybe Text,
    grammarPatterns :: [Reference],
    grammarRepository :: Repository
  }

-- | Repository
type Repository = Map Text Rule

-- | Rule
data Rule = Rule
  { -- | Unique ID as key in the repository
    ruleID :: Text,
    -- | "name"
    ruleName :: Maybe Text,
    -- | "contentName"
    ruleContentName :: Maybe Text,
    -- | "match" and "captures"
    ruleMatch :: Maybe Capture,
    -- | "begin" and "beginCaptures"
    ruleBegin :: Maybe Capture,
    -- | "end" and "endCaptures"
    ruleEnd :: Maybe Capture,
    -- | "include"
    ruleInclude :: [Reference]
  }



-- | A capturing group
data Capture = Capture String (Map Int Text)

-- | Reference in "include"
data Reference
  = -- | Generates "$self"
    Self
  | -- | e.g. Place "variable" to generate "#variable"
    Ref Text
  | -- | e.g. Place "hs" to generate "source.hs"
    Lang Text

--------------------------------------------------------------------------------

-- | Helper functions
match :: Text -> String -> Text -> Rule
match i regex name =
  Rule
    { ruleID = i,
      ruleBegin = Nothing,
      ruleEnd = Nothing,
      ruleMatch = Just (Capture ("\\b(" <> regex <> ")\\b") Map.empty),
      ruleName = Just name,
      ruleInclude = [],
      ruleContentName = Nothing
    }

include :: Text -> Reference -> Rule
include i ref =
  Rule
    { ruleID = i,
      ruleName = Nothing,
      ruleMatch = Nothing,
      ruleBegin = Nothing,
      ruleEnd = Nothing,
      ruleContentName = Nothing,
      ruleInclude = [ref]
    }

ref :: Rule -> Reference 
ref = Ref . ruleID

capture :: String -> Text -> Maybe Capture
capture regex name = Just $ Capture ("(" <> regex <> ")") $ Map.fromList [(1, name)]

captureWord :: String -> Text -> Maybe Capture
captureWord regex = capture ("\\b" <> regex <> "\\b")

--------------------------------------------------------------------------------

instance ToJSON Grammar where
  toJSON grammar =
    object $
      [ "scopeName" .= grammarScopeName grammar,
        "fileTypes" .= grammarFileTypes grammar,
        "repository" .= grammarRepository grammar
      ]
        <> ["patterns" .= map (\ref -> object ["include" .= ref]) (grammarPatterns grammar)]
        <> "foldingStartMarker" .? grammarFoldingStartMarker grammar
        <> "foldingStopMarker" .? grammarFoldingStopMarker grammar
        <> "firstLineMatch" .? grammarFirstLineMatch grammar

instance ToJSON Rule where
  toJSON rule =
    object $
      match
        <> begin
        <> end
        <> "name" .? ruleName rule
        <> "contentName" .? ruleContentName rule
        <> include
    where
      match = case ruleMatch rule of
        Nothing -> []
        Just (Capture regex groups) ->
          [ "match" .= regex,
            "captures" .= foldCaptureGroups groups
          ]

      begin = case ruleBegin rule of
        Nothing -> []
        Just (Capture regex groups) ->
          [ "begin" .= regex,
            "beginCaptures" .= foldCaptureGroups groups
          ]

      end = case ruleEnd rule of
        Nothing -> []
        Just (Capture regex groups) ->
          [ "end" .= regex,
            "endCaptures" .= foldCaptureGroups groups
          ]

      include = ["patterns" .= map (\ref -> object ["include" .= ref]) (ruleInclude rule)]

infixr 8 .?

(.?) :: (KeyValue a, ToJSON v) => Text -> Maybe v -> [a]
key .? field = maybe [] (\x -> [key .= x]) field

foldCaptureGroups :: Map Int Text -> Value
foldCaptureGroups = object . foldrWithKey (\key value acc -> acc <> [pack (show key) .= object ["name" .= value]]) []

instance ToJSON Reference where
  toJSON Self = "$self"
  toJSON (Ref s) = String $ "#" <> s
  toJSON (Lang s) = String $ "source." <> s
