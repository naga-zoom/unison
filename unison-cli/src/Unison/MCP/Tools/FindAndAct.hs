{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @find-and-act@ MCP tool: compose Pattern DSL queries with bulk
-- actions. One call to find matches and apply an action to all of them.
--
-- Defaults to /dryRun/=true for safety — the response always lists the
-- matches, and only mutates the codebase when @dryRun@ is set false.
--
-- v1 supports two actions:
--
-- * @{type: \"delete\"}@ — delete each matched definition.
-- * @{type: \"move-to\", destNamespace: \"foo.bar\"}@ — move each match
--   under the destination namespace, preserving the final name segment.
--
-- Rename-by-template is intentionally out of scope for v1: applying a
-- substitution rule across a heterogeneous match set produces brittle
-- name collisions; agents that want batch rename should compose
-- 'find-and-act' (delete) with 'update-definitions' (recreate under
-- new names), or use 'rename-definition' / 'move-definition' which now
-- accept bulk lists.
module Unison.MCP.Tools.FindAndAct
  ( findAndActTool,
  )
where

import Control.Monad.Except (throwError)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Control.Monad.Except (ExceptT)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Path qualified as Path
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.MCP.Cli (CliOutput (..), cliToMCP, handleInputMCP)
import Unison.MCP.Domain.Pattern qualified as Pattern
import Unison.MCP.Tools.Find (kindText)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Name (Name)
import Unison.Name qualified as Name (makeAbsolute)
import Unison.Prelude
import Unison.Referent qualified as Referent
import Unison.Syntax.Name qualified as Name
import Unison.Util.Relation qualified as R

findAndActTool :: Tool MCP
findAndActTool =
  Tool
    { toolName = toToolName FindAndActTool,
      toolDescription =
        "Run a Pattern-DSL query, then apply an action to every match. \
        \Defaults to dryRun=true (returns matches, no mutation). \
        \Supported actions: \
        \{type: \"delete\"} — delete each match; \
        \{type: \"move-to\", destNamespace: \"foo.bar\"} — move each \
        \match under destNamespace, preserving the final segment. Pair \
        \with the Pattern DSL's kind:/name:/glob/AND/OR/NOT primitives.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Find and Act",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy @FindAndActToolArguments,
      toolHandler = \(FindAndActToolArguments {projectContext, query, action, dryRun}) -> handleToolError $ do
        case Pattern.parsePattern query of
          Left err -> throwError $ "Pattern parse error: " <> Text.pack (show err)
          Right pat -> do
            let noop _ = pure ()
            (mb, _) <- cliToMCP projectContext noop Cli.getCurrentBranch0
            case mb of
              Nothing -> throwError "No current branch"
              Just b -> do
                let matches = enumerateMatchNames pat (into @Text projectContext.projectName) b
                let isDry = fromMaybe True dryRun
                results <-
                  if isDry
                    then pure []
                    else for matches (applyAction projectContext action)
                let report =
                      Aeson.object
                        [ "query" Aeson..= query,
                          "action" Aeson..= encodeAction action,
                          "dryRun" Aeson..= isDry,
                          "matchCount" Aeson..= length matches,
                          "matches" Aeson..= map encodeMatch matches,
                          "results" Aeson..= results
                        ]
                pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode report
    }

-- ----------------------------------------------------------------------------
-- Match enumeration (mirrors Find, but keeps Name + kind structurally)
-- ----------------------------------------------------------------------------

data Hit = Hit
  { hitName :: Name,
    hitKind :: Pattern.OpKind
  }

encodeMatch :: Hit -> Aeson.Value
encodeMatch h =
  Aeson.object
    [ "name" Aeson..= Name.toText h.hitName,
      "kind" Aeson..= kindText h.hitKind
    ]

enumerateMatchNames :: Pattern.Pattern -> Text -> Branch.Branch0 m -> [Hit]
enumerateMatchNames pat projectText b =
  let termHits =
        [ Hit n opKind
        | (ref, n) <- R.toList (Branch.deepTerms b),
          let opKind = case ref of
                Referent.Con {} -> Pattern.OpCtor
                Referent.Ref _ -> classifyByConvention (Name.toText n),
          Pattern.matchPattern pat (Pattern.Operand (Name.toText n) opKind (Just projectText) Nothing)
        ]
      typeHits =
        [ Hit n Pattern.OpType
        | (_, n) <- R.toList (Branch.deepTypes b),
          Pattern.matchPattern pat (Pattern.Operand (Name.toText n) Pattern.OpType (Just projectText) Nothing)
        ]
   in termHits <> typeHits

classifyByConvention :: Text -> Pattern.OpKind
classifyByConvention t
  | ".doc" `Text.isSuffixOf` t = Pattern.OpDoc
  | ".tests." `Text.isInfixOf` t || ".tests" `Text.isSuffixOf` t = Pattern.OpTest
  | otherwise = Pattern.OpTerm

-- ----------------------------------------------------------------------------
-- Action application
-- ----------------------------------------------------------------------------

encodeAction :: FindAction -> Aeson.Value
encodeAction = \case
  FindActionDelete -> Aeson.object ["type" Aeson..= ("delete" :: Text)]
  FindActionMoveTo dst ->
    Aeson.object
      [ "type" Aeson..= ("move-to" :: Text),
        "destNamespace" Aeson..= Name.toText dst
      ]

type EMCP = ExceptT Text MCP

applyAction :: ProjectContext -> FindAction -> Hit -> EMCP Aeson.Value
applyAction ctx act hit = do
  out <- case act of
    FindActionDelete ->
      handleInputMCP
        ctx
        [Right $ Input.DeleteI True Input.DeleteTarget'TermOrType [HQ'.NameOnly hit.hitName]]
    FindActionMoveTo dst -> do
      let src = Path.fromName' (Name.makeAbsolute hit.hitName)
      let destPath = Path.fromName' (Name.makeAbsolute dst)
      handleInputMCP ctx [Right $ Input.MoveAllI src destPath]
  pure $
    Aeson.object
      [ "name" Aeson..= Name.toText hit.hitName,
        "kind" Aeson..= kindText hit.hitKind,
        "ok" Aeson..= null out.errorMessages,
        "errors" Aeson..= out.errorMessages
      ]
