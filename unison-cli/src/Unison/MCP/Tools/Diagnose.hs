{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @diagnose@ MCP tool: scan a branch for structural issues with type
-- declarations.
--
-- Wraps 'Unison.MCP.Domain.Structural.scanBranch'. Returns a discriminated
-- list of issues: each one is either an @orphan_type@ (declared
-- constructors exceed named constructors) or a @misplaced_ctor@ (a named
-- constructor's path is not under any of the type's namespaces).
--
-- Read-only — the analysis does not modify the codebase. The companion
-- @sanity-fix@ tool (planned) loops @diagnose@ against actual fixes.
module Unison.MCP.Tools.Diagnose
  ( diagnoseTool,
    issueToJson,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch qualified as Branch
import Unison.MCP.Cache qualified as Cache
import Unison.MCP.Cli (cliToMCP)
import Unison.MCP.Domain.Structural (StructuralIssue (..), scanBranch)
import Unison.MCP.Types
import Unison.MCP.Wire qualified as Wire
import Unison.MCP.Wrapper
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.Name qualified as Name
import UnliftIO qualified

-- ----------------------------------------------------------------------------
-- Wire format
-- ----------------------------------------------------------------------------

data DiagnoseResponse = DiagnoseResponse
  { issues :: [Aeson.Value],
    totalCount :: Int
  }

instance Aeson.ToJSON DiagnoseResponse where
  toJSON r =
    Aeson.object
      [ "issues" Aeson..= r.issues,
        "totalCount" Aeson..= r.totalCount
      ]

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

diagnoseTool :: Tool MCP
diagnoseTool =
  Tool
    { toolName = toToolName DiagnoseTool,
      toolDescription =
        "Scan the current branch for structural issues with type declarations: \
        \orphan types (declared constructors exceed named constructors) and \
        \misplaced constructors (named ctors whose path is not under any of \
        \the type's namespaces). Read-only. Returns structured JSON \
        \{issues: [{kind: orphan_type | misplaced_ctor, ...}], totalCount}.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Diagnose",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @DiagnoseToolArguments,
      toolHandler = \(DiagnoseToolArguments {projectContext}) -> handleToolError $ do
        codebase <- asks (.codebase)
        let noop _ = pure ()
        (mFullBranch, _output) <- cliToMCP projectContext noop Cli.getCurrentBranch
        case mFullBranch of
          Nothing -> throwError "No current branch"
          Just fullBranch -> do
            let b = Branch.head fullBranch
            -- Merkle-keyed cache: same branch hash → reuse scan result.
            let cacheKey = Cache.branchCacheKey fullBranch "diagnose"
            Cache.getOrComputeEMCP cacheKey
              ( do
                  structuralIssues <- UnliftIO.liftIO $ scanBranch codebase b
                  let payload =
                        DiagnoseResponse
                          { issues = map issueToJson structuralIssues,
                            totalCount = length structuralIssues
                          }
                  pure (Aeson.toJSON payload)
              )
              >>= \payload ->
                pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode payload
    }

-- ----------------------------------------------------------------------------
-- JSON projection of a StructuralIssue (pure; unit-testable)
-- ----------------------------------------------------------------------------

-- | Render one 'StructuralIssue' as a JSON object with a @kind@ discriminator.
issueToJson :: StructuralIssue -> Aeson.Value
issueToJson = \case
  OrphanType {typeRef, typeNames, declaredCtors, namedCtors} ->
    Aeson.object
      [ "kind" Aeson..= ("orphan_type" :: Text),
        "typeRef" Aeson..= refToText typeRef,
        "typeNames" Aeson..= map Name.toText (Set.toList typeNames),
        "declaredCtors" Aeson..= declaredCtors,
        "namedCtors" Aeson..= namedCtors
      ]
  MisplacedConstructor {ctorName, ctorRef, typeRef, typeNames} ->
    Aeson.object
      [ "kind" Aeson..= ("misplaced_ctor" :: Text),
        "ctorName" Aeson..= Name.toText ctorName,
        "ctorRef" Aeson..= referentText ctorRef,
        "typeRef" Aeson..= refToText typeRef,
        "typeNames" Aeson..= map Name.toText (Set.toList typeNames)
      ]
  where
    refToText :: Reference.TypeReference -> Text
    refToText = Wire.shortHashText . Reference.toShortHash

    referentText :: Referent.Referent -> Text
    referentText = Wire.shortHashText . Referent.toShortHash
