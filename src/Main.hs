{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Grammar

main :: IO ()
main = BS.writeFile "output.tmLanguage.json" (encode grammar)

grammar :: Grammar
grammar =
  Grammar
    { grammarScopeName = "source.gcl",
      grammarFileTypes = ["gcl"],
      grammarFoldingStartMarker = Nothing,
      grammarFoldingStopMarker = Nothing,
      grammarFirstLineMatch = Nothing,
      grammarPatterns = references,
      grammarRepository = repository
    }

references :: [Reference]
references =
  [ Ref "skip",
    Ref "abort"
  ]

repository :: Repository
repository =
  Map.fromList
    [ ("skip", skip),
      ("abort", abort)
    ]
  where 
    skip :: Rule
    skip =
      Rule
        { ruleBegin = Nothing,
          ruleEnd = Nothing,
          ruleMatch = Just (Capture "skip" Map.empty),
          ruleName = Just "keyword.control.skip",
          ruleInclude = [],
          ruleContentName = Nothing
        }

    abort :: Rule
    abort =
      Rule
        { ruleBegin = Nothing,
          ruleEnd = Nothing,
          ruleMatch = Just (Capture "abort" Map.empty),
          ruleName = Just "keyword.control.abort",
          ruleInclude = [],
          ruleContentName = Nothing
        }


-- declaration :: Rule
-- declaration =
--   Rule
--     { ruleBegin = Just $ Capture "^var" $ Map.fromList [(0, "storage.modifier")],
--       ruleEnd = Just $ Capture "(?=^\\S)" Map.empty,
--       ruleMatch = Nothing,
--       ruleName = Just "meta.declaration",
--       ruleInclude = [Ref "declaration-var"],
--       ruleContentName = Nothing
--     }

declarationVar :: Rule
declarationVar =
  Rule
    { ruleBegin = Just $ Capture "^var" $ Map.fromList [(0, "storage.modifier")],
      ruleEnd = Just $ Capture "(?=^\\S)" Map.empty,
      ruleMatch = Nothing,
      ruleName = Just "meta.declaration",
      ruleInclude = [Ref "built-in-type", Ref "declaration-var-names"],
      ruleContentName = Nothing
    }