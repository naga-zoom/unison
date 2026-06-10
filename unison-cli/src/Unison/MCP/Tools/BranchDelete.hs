{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @branch-delete@ MCP tool: delete a project branch.
--
-- Wraps 'DeleteBranch.handleDeleteBranch' via 'cliToMCP'. Refuses to
-- delete protected branches (per 'Unison.MCP.Domain.BranchProtection')
-- unless the agent passes @force: true@. The convention names protected:
-- @main@, @releases\/*@, and temp branches @update-*@\/@merge-*@\/@upgrade-*@.
module Unison.MCP.Tools.BranchDelete
  ( branchDeleteTool,
  )
where

import Control.Monad.Except (throwError)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text
import Unison.Codebase.Editor.HandleInput.DeleteBranch (handleDeleteBranch)
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName (..), ProjectName (..))
import Unison.MCP.Cli (cliToMCP)
import Unison.MCP.Domain.BranchProtection (formatProtectionReason, isProtected)
import Unison.MCP.Types
import Unison.MCP.Wrapper

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

branchDeleteTool :: Tool MCP
branchDeleteTool =
  Tool
    { toolName = toToolName BranchDeleteTool,
      toolDescription =
        "Delete a project branch. The branch to delete is supplied as \
        \'target' {project?, branch}. Refuses to delete protected branches \
        \(main, releases/*, update-*/merge-*/upgrade-*) unless force=true. \
        \For the temp-branch family specifically, prefer the dedicated \
        \'reap-temp-branches' tool which has integration-status guards.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Delete Branch",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @BranchDeleteToolArguments,
      toolHandler = \(BranchDeleteToolArguments {projectContext, target, force}) -> handleToolError $ do
        let doForce = fromMaybe False force
        case (isProtected target.branch, doForce) of
          (Just reason, False) ->
            throwError $ formatProtectionReason target.branch reason
          _ -> do
            let pab =
                  ProjectAndBranch
                    (UnsafeProjectName <$> target.project)
                    (UnsafeProjectBranchName target.branch)
            (_r, output) <- cliToMCP projectContext (const $ pure ()) (handleDeleteBranch pab)
            pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
    }

fromMaybe :: a -> Maybe a -> a
fromMaybe d = \case
  Nothing -> d
  Just x -> x
