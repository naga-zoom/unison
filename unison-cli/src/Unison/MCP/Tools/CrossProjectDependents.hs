{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @cross-project-dependents@ MCP tool: find usages of a definition
-- across multiple local projects in one call.
--
-- The built-in @list-definition-dependents@ only walks the current
-- branch. When an agent is about to rename or remove a library type, it
-- needs to know whether downstream projects reference it. This tool
-- loops the dependent search across a set of local projects' @main@
-- branches and aggregates the per-project hit report.
--
-- /Scope:/ local projects only — the tool reads the local UCM sqlite
-- codebase. It does not query Unison Share or any remote codebase.
module Unison.MCP.Tools.CrossProjectDependents
  ( crossProjectDependentsTool,
  )
where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.HashQualified qualified as HQ
import Unison.MCP.Cli (CliOutput (..), handleInputMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Prelude
import Unison.Syntax.Name qualified as Name
import UnliftIO qualified

crossProjectDependentsTool :: Tool MCP
crossProjectDependentsTool =
  Tool
    { toolName = toToolName CrossProjectDependentsTool,
      toolDescription =
        "Find usages of a definition across multiple local projects in \
        \one call. By default scans all local projects' main branches; \
        \pass `projects` to restrict to a subset (e.g. \
        \[\"loom\",\"compose\"]). Local-codebase only — no Share / \
        \remote scanning. Use before renaming or removing a definition \
        \that may be referenced by downstream consumers.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Cross-Project Dependents",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @CrossProjectDependentsToolArguments,
      toolHandler = \(CrossProjectDependentsToolArguments {definitionName, projects, branchName}) -> handleToolError $ do
        codebase <- asks (.codebase)
        targetProjects <- case projects of
          Just ps -> pure ps
          Nothing -> do
            all_ <- UnliftIO.liftIO $ Codebase.runTransaction codebase Q.loadAllProjects
            pure [into @Text (name p) | p <- all_]
        let theBranchName = fromMaybe "main" branchName
        results <- for targetProjects $ \projName -> do
          let pctx =
                ProjectContext
                  { projectName = UnsafeProjectName projName,
                    branchName = UnsafeProjectBranchName theBranchName
                  }
          eOut <- lift . UnliftIO.tryAny $ do
            r <- runExceptT $ handleInputMCP pctx [Right $ Input.ListDependentsI (HQ.NameOnly definitionName)]
            case r of
              Left e -> UnliftIO.throwIO (userError (Text.unpack e))
              Right out -> pure out
          case eOut of
            Left ex -> pure (mkErr projName (Text.pack (show ex)))
            Right out -> pure (mkOk projName out)
        let aggregated =
              Aeson.object
                [ "definitionName" Aeson..= Name.toText definitionName,
                  "branchName" Aeson..= theBranchName,
                  "projectsScanned" Aeson..= length targetProjects,
                  "results" Aeson..= results
                ]
        pure $ textToolResult (Text.decodeUtf8 . BL.toStrict $ Aeson.encode aggregated)
    }

mkOk :: Text -> CliOutput -> Aeson.Value
mkOk projName out =
  Aeson.object
    [ "project" Aeson..= projName,
      "ok" Aeson..= null out.errorMessages,
      "output" Aeson..= Text.intercalate "\n" out.outputMessages,
      "errors" Aeson..= out.errorMessages
    ]

mkErr :: Text -> Text -> Aeson.Value
mkErr projName err =
  Aeson.object
    [ "project" Aeson..= projName,
      "ok" Aeson..= False,
      "output" Aeson..= ("" :: Text),
      "errors" Aeson..= [err]
    ]
