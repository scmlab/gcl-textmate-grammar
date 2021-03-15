{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (toList)
import qualified Data.Map as Map
import Grammar

main :: IO ()
main = BS.writeFile "output.tmLanguage.json" (encode grammar)

--------------------------------------------------------------------------------

grammar :: Grammar
grammar =
  Grammar
    { grammarScopeName = "source.gcl",
      grammarFileTypes = ["gcl"],
      grammarFoldingStartMarker = Nothing,
      grammarFoldingStopMarker = Nothing,
      grammarFirstLineMatch = Nothing,
      grammarPatterns = patterns,
      grammarRepository = repository
    }

patterns :: [Reference]
patterns =
  map
    ref
    [ skip,
      abort,
      loop,
      conditional,
      assignment
    ]

repository :: Repository
repository =
  Map.fromList $
    map
      (\rule -> (ruleID rule, rule))
      [ skip,
        abort,
        loop,
        conditional,
        assignment,
        guardedCommands,
        guardedCommandBar,
        betweenConditionalCons,
        others2
      ]

--------------------------------------------------------------------------------

-- | Statements
skip :: Rule
skip = match "skip" "\\bskip\\b" "keyword.control.skip"

abort :: Rule
abort = match "abort" "\\babort\\b" "keyword.control.abort"

loop :: Rule
loop =
  Rule
    { ruleID = "loop",
      ruleBegin = Just $ Capture "\\b(do)\\b" $ Map.fromList [(1, "keyword.control.loop")],
      ruleEnd = Just $ Capture "\\b(od)\\b" $ Map.fromList [(1, "keyword.control.loop")],
      ruleMatch = Nothing,
      ruleName = Just "meta.statement.loop",
      ruleInclude = [ref skip, ref abort, ref conditional, ref assignment],
      ruleContentName = Nothing
    }

conditional :: Rule
conditional =
  Rule
    { ruleID = "conditional",
      ruleBegin = Just $ Capture "(if)" $ Map.fromList [(1, "keyword.control.conditional")],
      ruleEnd = Just $ Capture "(fi)" $ Map.fromList [(1, "keyword.control.conditional")],
      ruleMatch = Nothing,
      ruleName = Just "meta.statement.conditional",
      ruleInclude = [ref betweenConditionalCons],
      -- ruleInclude = [Ref "guarded-commands"],
      -- ruleInclude = [Ref "skip", Ref "abort", Ref "conditional", Ref "assignment"],
      ruleContentName = Nothing
    }

assignment :: Rule
assignment =
  Rule
    { ruleID = "assignment",
      ruleBegin = Nothing,
      ruleEnd = Nothing,
      ruleMatch = Just $ Capture "aaaaa" $ Map.fromList [(1, "keyword.control.assigment")],
      ruleName = Just "invalid",
      ruleInclude = [],
      ruleContentName = Just "invalid"
    }

guardedCommands :: Rule
guardedCommands =
  Rule
    { ruleID = "guarded-commands",
      ruleBegin = Nothing,
      ruleEnd = Nothing,
      ruleMatch = Nothing,
      ruleName = Just "invalid",
      ruleInclude = [ref guardedCommandBar],
      ruleContentName = Just "invalid"
    }

guardedCommandBar :: Rule
guardedCommandBar = match "guarded-command-bar" "\\|" "punctuation.separator.bar"

-- | in between if ... fi
betweenConditionalCons :: Rule
betweenConditionalCons =
  Rule
    { ruleID = "others",
      ruleBegin = Just $ Capture "(?<=if)" Map.empty,
      ruleEnd = Just $ Capture "\\|s|(?=fi)" Map.empty,
      ruleMatch = Nothing,
      ruleName = Just "invalid2",
      ruleInclude = [ref guardedCommands],
      ruleContentName = Just "invalid2"
    }

others2 :: Rule
others2 =
  Rule
    { ruleID = "others2",
      ruleBegin = Nothing,
      ruleEnd = Nothing,
      ruleMatch = Just $ Capture "->" Map.empty,
      ruleName = Just "invalid",
      ruleInclude = [],
      ruleContentName = Just "invalid"
    }

-- assignment :: Rule
-- assignment =
--   Rule
--     { ruleBegin = Nothing,
--       ruleEnd = Nothing,
--       ruleMatch = Just $ Capture "(:=)" $ Map.fromList [(1, "keyword.control.assigment")],
--       ruleName = Just "meta.statement.assignment",
--       ruleInclude = [],
--       ruleContentName = Just "meta.statement.assignment.values"
--     }

-- assignment :: Rule
-- assignment =
--   Rule
--     { ruleBegin =
--         Just $
--           Capture "(:=)" $
--             Map.fromList
--               [ (1, "keyword.control.assignment") ],
--       ruleEnd = Just $ Capture "(?=^\\S)" Map.empty,
--       ruleMatch = Nothing,
--       ruleName = Just "meta.statement.assignment",
--       ruleInclude = [],
--       ruleContentName = Just "invalid.meta.statement.assignment.values"
--     }

-- assignmentNames :: Rule
-- assignmentNames =
--   Rule
--     { ruleBegin = Nothing,
--       ruleEnd = Nothing,
--       ruleMatch = Just $ Capture ".+" Map.empty,
--       ruleName = Just "invalid.meta.assignment.names2",
--       ruleInclude = [],
--       ruleContentName = Nothing
--     }

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

-- declarationVar :: Rule
-- declarationVar =
--   Rule
--     { ruleBegin = Just $ Capture "^var" $ Map.fromList [(0, "storage.modifier")],
--       ruleEnd = Just $ Capture "(?=^\\S)" Map.empty,
--       ruleMatch = Nothing,
--       ruleName = Just "meta.declaration",
--       ruleInclude = [Ref "built-in-type", Ref "declaration-var-names"],
--       ruleContentName = Nothing
--     }