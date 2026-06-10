{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @project-rename@ MCP tool: rename the project supplied via
-- @projectContext@ to a new name.
--
-- Wraps 'Unison.Codebase.Editor.HandleInput.ProjectRename.handleProjectRename'
-- via 'cliToMCP'. The current project is determined by @projectContext@;
-- the tool renames it to @newName@. UCM refuses the rename when another
-- project already has that name.
module Unison.MCP.Tools.ProjectRename
  ( projectRenameTool,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text
import Unison.Codebase.Editor.HandleInput.ProjectRename (handleProjectRename)
import Unison.Core.Project (ProjectName (..))
import Unison.MCP.Cli (cliToMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

projectRenameTool :: Tool MCP
projectRenameTool =
  Tool
    { toolName = toToolName ProjectRenameTool,
      toolDescription =
        "Rename the project supplied via projectContext to 'newName'. \
        \UCM refuses when another project already has that name; the \
        \error is returned in the response. No-op when newName equals \
        \the existing project name. \
        \\
        \WARNING: This renames ONLY the local project. Branch \
        \remote-tracking URLs (which embed the original project name) are \
        \NOT updated. A subsequent 'push' without an explicit target will \
        \still write to the ORIGINAL remote project on Share. To publish \
        \the renamed project under a new Share project, either (a) call \
        \'push' with an explicit target {project, branch}, or (b) verify \
        \with 'list-project-branches' that the remote URL is what you \
        \expect before pushing.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Rename Project",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @ProjectRenameToolArguments,
      toolHandler = \(ProjectRenameToolArguments {projectContext, newName}) -> handleToolError $ do
        (_r, output) <-
          cliToMCP projectContext (const $ pure ()) $
            handleProjectRename (UnsafeProjectName newName)
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
    }
