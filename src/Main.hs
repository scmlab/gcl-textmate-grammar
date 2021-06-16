{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (toList)
import qualified Data.Map as Map
import Grammar

main :: IO ()
main = BS.writeFile "output.tmGrammar.json" (encode grammar)

--------------------------------------------------------------------------------

grammar :: Grammar
grammar =
  Grammar
    { grammarScopeName = "source.gcl",
      grammarFileTypes = ["gcl"],
      grammarFoldingStartMarker = Nothing,
      grammarFoldingStopMarker = Nothing,
      grammarFirstLineMatch = Nothing,
      grammarPatterns = topLevelPatterns,
      grammarRepository = repository
    }

-- | Only allowed in the top-most level
topLevelPatterns :: [Reference]
topLevelPatterns =
  map
    ref
    [var, con, let']
    <> nestedPatterns

-- | Can appear in anywhere
nestedPatterns :: [Reference]
nestedPatterns =
  map
    ref
    [ skip,
      abort,
      spec,
      assertion,
      loop,
      conditional,
      assignment,
      comment
    ]

repository :: Repository
repository =
  Map.fromList $
    map
      (\rule -> (ruleID rule, rule))
      [ 
        -- statements 
        skip,
        abort,
        spec,
        assertion,
        loop,
        conditional,
        assignment,
        guardedCommand,
        -- comment
        comment,
        -- declarations 
        con,
        var,
        let',

        -- types  
        int,
        bool
      ]

--------------------------------------------------------------------------------

-- | Declarations

con :: Rule
con = Rule
    { ruleID = "con",
      ruleBegin = captureWord "con" "keyword.control.con",
      ruleEnd = Just $ Capture "\\n" Map.empty,
      ruleMatch = Nothing,
      ruleName = Just "meta.declaration",
      ruleInclude = types,
      ruleContentName = Nothing
    }

var :: Rule
var = Rule
    { ruleID = "var",
      ruleBegin = captureWord "var" "keyword.control.var",
      ruleEnd = Just $ Capture "\\n" Map.empty,
      ruleMatch = Nothing,
      ruleName = Just "meta.declaration",
      ruleInclude = types,
      ruleContentName = Nothing
    }


let' :: Rule
let' = match "let" "let" "keyword.control.let"

--------------------------------------------------------------------------------

-- | Comment
comment :: Rule
comment = 
  Rule
    { ruleID = "comment",
      ruleBegin = Nothing,
      ruleEnd = Nothing,
      ruleMatch =
        Just $
          Capture "(\\-\\-.*$)" $
            Map.fromList
              [ (1, "comment.line.double-dash")
              ],
      ruleName = Just "meta.comment.line",
      ruleInclude = [],
      ruleContentName = Nothing
    }

--------------------------------------------------------------------------------

-- | Statements
skip :: Rule
skip = match "skip" "skip" "keyword.control.skip"

abort :: Rule
abort = match "abort" "abort" "keyword.control.abort"

spec :: Rule
spec =
  Rule
    { ruleID = "spec",
      ruleBegin = capture "\\{\\!" "punctuation.definition.quote.begin.markdown.gcl.spec.open",
      ruleEnd = capture "\\!\\}" "punctuation.definition.quote.begin.markdown.gcl.spec.close",
      ruleMatch = Nothing,
      ruleName = Just "meta.statement.spec",
      ruleInclude = [],
      ruleContentName = Nothing
    }

assertion :: Rule
assertion =
  Rule
    { ruleID = "assertion",
      ruleBegin = capture "\\{" "support.other.parenthesis.regexp.gcl.assertion.open",
      ruleEnd = capture "\\}" "support.other.parenthesis.regexp.gcl.assertion.close",
      ruleMatch = Nothing,
      ruleName = Just "meta.statement.assertion",
      ruleInclude = [],
      ruleContentName = Nothing
    }

loop :: Rule
loop =
  Rule
    { ruleID = "loop",
      ruleBegin = captureWord "do" "keyword.control.loop",
      ruleEnd = captureWord "od" "keyword.control.loop",
      ruleMatch = Nothing,
      ruleName = Just "meta.statement.loop",
      ruleInclude = ref guardedCommand : nestedPatterns,
      ruleContentName = Nothing
    }

conditional :: Rule
conditional =
  Rule
    { ruleID = "conditional",
      ruleBegin = captureWord "if" "keyword.control.conditional",
      ruleEnd = captureWord "fi" "keyword.control.conditional",
      ruleMatch = Nothing,
      ruleName = Just "meta.statement.conditional",
      ruleInclude = ref guardedCommand : nestedPatterns,
      ruleContentName = Nothing
    }

assignment :: Rule
assignment =
  Rule
    { ruleID = "assignment",
      ruleBegin = Nothing,
      ruleEnd = Nothing,
      ruleMatch =
        Just $
          Capture "(\\:\\=)" $
            Map.fromList
              [ (1, "keyword.control.assignment")
              ],
      ruleName = Just "meta.statement.assignment",
      ruleInclude = [],
      ruleContentName = Nothing
    }

guardedCommand :: Rule
guardedCommand =
  Rule
    { ruleID = "guarded-command",
      ruleBegin = Nothing,
      ruleEnd = Nothing,
      ruleMatch =
        Just $
          Capture "(\\-\\>)|(\\|)" $
            Map.fromList
              [ (1, "punctuation.section.embedded.arrow"),
                (2, "punctuation.section.embedded.bar")
              ],
      ruleName = Just "meta.statement.guardedCommands",
      ruleInclude = [],
      ruleContentName = Nothing
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

--------------------------------------------------------------------------------

-- | Types

-- | Appears in declarations
types :: [Reference]
types =
  map
    ref
    [ int,
      bool
    ]

int :: Rule
int = match "int" "Int" "storage.type.int"

bool :: Rule
bool = match "bool" "Bool" "storage.type.bool"
