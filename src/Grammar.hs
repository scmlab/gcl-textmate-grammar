module Grammar where

import Data.Map

-- | Top level structure
data Grammar = Grammar
  { scopeName :: String,
    fileTypes :: [String],
    foldingStartMarker :: Maybe String,
    foldingStopMarker :: Maybe String,
    patterns :: [String],
    firstLineMatch :: Maybe String,
    repository :: Repository
  }

-- | Repository
type Repository = Map String Rule

-- | Rule
data Rule = Rule
  { ruleName :: String,
    ruleMatch :: Maybe Capture,
    ruleBegin :: Maybe Capture,
    ruleEnd :: Maybe Capture,
    ruleContentName :: Maybe String,
    ruleInclude :: Maybe Reference
  }

-- | A capturing group
data Capture = Capture String (Map Int String)

-- | Reference in "include"
data Reference
  = -- | Generates "$self"
    Self
  | -- | e.g. Place "variable" to generate "#variable"
    Ref String
  | -- | e.g. Place "hs" to generate "source.hs"
    Lang String
