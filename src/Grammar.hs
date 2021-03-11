{-# LANGUAGE OverloadedStrings #-}

module Grammar where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Map ( foldrWithKey, Map )
import Data.Text ( pack, Text )

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
  { 
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

include :: Reference -> Rule
include ref =
  Rule
    { ruleName = Nothing,
      ruleMatch = Nothing,
      ruleBegin = Nothing,
      ruleEnd = Nothing,
      ruleContentName = Nothing,
      ruleInclude = [ref]
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

instance ToJSON Grammar where
  toJSON grammar =
    object $
      [ "scopeName" .= grammarScopeName grammar,
        "fileTypes" .= grammarFileTypes grammar,
        -- "patterns" .= grammarPatterns grammar,
        "repository" .= grammarRepository grammar
      ]
        -- <> ["patterns" .= grammarPatterns grammar]
        -- <> ["patterns" .= map (\ref -> ref) (grammarPatterns grammar)]
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