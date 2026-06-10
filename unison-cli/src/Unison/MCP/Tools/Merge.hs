{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @merge@ MCP tool: merge a source branch into the current (target)
-- branch.
--
-- Wraps 'Unison.Codebase.Editor.HandleInput.Merge2.handleMerge'. The
-- target branch is taken from @projectContext@ (set as current by
-- 'cliToMCP' before the handler runs). The source branch is required;
-- the source project is optional (defaults to the current project).
--
-- UCM's merge does a fast-forward when possible; otherwise it performs a
-- three-way merge and reports any conflicts. The agent reads the
-- response's outputMessages / errorMessages for the merge result.
module Unison.MCP.Tools.Merge
  ( mergeTool,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text
import Unison.Codebase.Editor.HandleInput.Merge2 (handleMerge)
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName (..), ProjectName (..))
import Unison.MCP.Cli (cliToMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

mergeTool :: Tool MCP
mergeTool =
  Tool
    { toolName = toToolName MergeTool,
      toolDescription =
        "Merge a source branch into the target branch (from projectContext). \
        \Source 'branch' is required; source 'project' is optional and \
        \defaults to the target's project. UCM does fast-forward when \
        \possible; otherwise a three-way merge with conflict reporting.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Merge",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy @MergeToolArguments,
      toolHandler = \(MergeToolArguments {projectContext, source}) -> handleToolError $ do
        let pab =
              ProjectAndBranch
                (UnsafeProjectName <$> source.project)
                (UnsafeProjectBranchName source.branch)
        (_r, output) <- cliToMCP projectContext (const $ pure ()) (handleMerge pab)
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
    }
