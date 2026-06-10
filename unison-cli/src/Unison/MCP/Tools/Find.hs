{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @find@ MCP tool: composable Boolean query over codebase definitions.
--
-- Consumes the Pattern DSL substrate at 'Unison.MCP.Domain.Pattern' and walks
-- the current branch's terms + types directly via 'Branch.deepTerms' and
-- 'Branch.deepTypes', applying 'matchPattern' to each. Returns structured
-- JSON — no @notifyUser@ text path, so the response is agent-ready and free
-- of TUI chrome.
module Unison.MCP.Tools.Find
  ( findTool,
    classifyTermNameByConvention,
    kindText,
  )
where

import Control.Monad.Except (throwError)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch qualified as Branch
import Unison.MCP.Cli (cliToMCP)
import Unison.MCP.Domain.Pattern qualified as Pattern
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ShortHash qualified as ShortHash
import Unison.Syntax.Name qualified as Name
import Unison.Util.Relation qualified as R

-- ----------------------------------------------------------------------------
-- Response shape
-- ----------------------------------------------------------------------------

data FindResult = FindResult
  { matches :: [Match],
    totalCount :: Int
  }

instance Aeson.ToJSON FindResult where
  toJSON r =
    Aeson.object
      [ "matches" Aeson..= r.matches,
        "totalCount" Aeson..= r.totalCount
      ]

data Match = Match
  { name :: Text,
    kind :: Text,
    hash :: Text
  }

instance Aeson.ToJSON Match where
  toJSON m =
    Aeson.object
      [ "name" Aeson..= m.name,
        "kind" Aeson..= m.kind,
        "hash" Aeson..= m.hash
      ]

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

findTool :: Tool MCP
findTool =
  Tool
    { toolName = toToolName FindTool,
      toolDescription =
        "Composable Boolean query over codebase definitions. \
        \Supports kind:/name:/project:/owner: predicates, AND/OR/NOT \
        \composition, parens, ANYTHING/NOTHING identities, and glob \
        \patterns (* and ?). Returns structured JSON \
        \{matches: [{name, kind, hash}], totalCount}.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Find",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @FindToolArguments,
      toolHandler = \(FindToolArguments {projectContext, query}) -> handleToolError $ do
        case Pattern.parsePattern query of
          Left err -> throwError $ "Pattern parse error: " <> Text.pack (show err)
          Right pat -> do
            let noop _ = pure ()
            (mb, _output) <- cliToMCP projectContext noop Cli.getCurrentBranch0
            case mb of
              Nothing -> throwError "No current branch"
              Just b -> do
                let projectText = into @Text projectContext.projectName
                let ms = enumerateMatches pat projectText b
                let result = FindResult {matches = ms, totalCount = length ms}
                pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode result
    }

-- ----------------------------------------------------------------------------
-- Enumeration (pure given a Branch0)
-- ----------------------------------------------------------------------------

-- | Walk every term and type in @b@, classify its kind, build the 'Operand',
-- and keep those that match the pattern. Terms emit their referent's short
-- hash; types emit the type reference's short hash; constructors are flagged
-- as kind \"ctor\" with the constructor's referent hash.
enumerateMatches :: Pattern.Pattern -> Text -> Branch.Branch0 m -> [Match]
enumerateMatches pat projectText b =
  let termMatches =
        [ Match
            { name = Name.toText n,
              kind = kindText opKind,
              hash = ShortHash.toText (Referent.toShortHash ref)
            }
        | (ref, n) <- R.toList (Branch.deepTerms b),
          let opKind = classifyReferent ref n,
          let op =
                Pattern.Operand
                  { Pattern.opName = Name.toText n,
                    Pattern.opKind = opKind,
                    Pattern.opProject = Just projectText,
                    Pattern.opOwner = Nothing
                  },
          Pattern.matchPattern pat op
        ]
      typeMatches =
        [ Match
            { name = Name.toText n,
              kind = "type",
              hash = ShortHash.toText (Reference.toShortHash ref)
            }
        | (ref, n) <- R.toList (Branch.deepTypes b),
          let op =
                Pattern.Operand
                  { Pattern.opName = Name.toText n,
                    Pattern.opKind = Pattern.OpType,
                    Pattern.opProject = Just projectText,
                    Pattern.opOwner = Nothing
                  },
          Pattern.matchPattern pat op
        ]
   in termMatches <> typeMatches

-- | Classify a 'Referent' + 'Name' into an 'OpKind'.
--
-- * 'Referent.Con' → 'OpCtor' (constructor reference)
-- * 'Referent.Ref' with name suffix @.doc@ → 'OpDoc'
-- * 'Referent.Ref' with @.tests.@ infix or @.tests@ suffix → 'OpTest'
-- * otherwise 'OpTerm'
--
-- Doc/Test classification is name-convention-driven (UCM doesn't carry a
-- separate kind tag for them).
classifyReferent :: Referent -> Name -> Pattern.OpKind
classifyReferent ref n = case ref of
  Referent.Con {} -> Pattern.OpCtor
  Referent.Ref _ -> classifyTermNameByConvention (Name.toText n)

-- | Pure name-convention classifier — separated for unit testing.
classifyTermNameByConvention :: Text -> Pattern.OpKind
classifyTermNameByConvention t
  | ".doc" `Text.isSuffixOf` t = Pattern.OpDoc
  | ".tests." `Text.isInfixOf` t || ".tests" `Text.isSuffixOf` t = Pattern.OpTest
  | otherwise = Pattern.OpTerm

-- | Wire format for the @kind@ JSON field.
kindText :: Pattern.OpKind -> Text
kindText = \case
  Pattern.OpTerm -> "term"
  Pattern.OpType -> "type"
  Pattern.OpCtor -> "ctor"
  Pattern.OpDoc -> "doc"
  Pattern.OpTest -> "test"
  Pattern.OpAbility -> "ability"
