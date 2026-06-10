{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @project-create@ MCP tool: create a new local project.
--
-- Wraps 'Unison.Codebase.Editor.HandleInput.ProjectCreate.projectCreate'
-- via 'cliToMCP'. The @projectContext@ supplied by the agent is only used
-- to bootstrap the 'Cli' environment; the new project is independent of
-- it.
--
-- Two knobs:
--
-- * @projectName@ (optional) — desired name. Auto-generated if absent.
-- * @downloadBase@ (optional, default true) — fetch and install
--   @\@unison\/base@ into the new project.
module Unison.MCP.Tools.ProjectCreate
  ( projectCreateTool,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text
import Unison.Codebase.Editor.HandleInput.ProjectCreate qualified as ProjectCreate
import Unison.Core.Project (ProjectName (..))
import Unison.MCP.Cli (cliToMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

projectCreateTool :: Tool MCP
projectCreateTool =
  Tool
    { toolName = toToolName ProjectCreateTool,
      toolDescription =
        "Create a new local project. Optional 'projectName' (auto-generated \
        \if omitted); optional 'downloadBase' (default true) — fetches and \
        \installs @unison/base into the new project. The projectContext arg \
        \is used only to bootstrap the Cli environment; the new project is \
        \independent of it.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Create Project",
            readOnlyHint = Just False,
            destructiveHint = Just False,
            idempotentHint = Just False,
            openWorldHint = Just True
          },
      toolArgType = Proxy @ProjectCreateToolArguments,
      toolHandler = \(ProjectCreateToolArguments {projectContext, projectName, downloadBase}) -> handleToolError $ do
        let dl = fromMaybe True downloadBase
        let name = UnsafeProjectName <$> projectName
        (_r, output) <-
          cliToMCP projectContext (const $ pure ()) $
            ProjectCreate.projectCreate dl name
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
    }

fromMaybe :: a -> Maybe a -> a
fromMaybe d = \case
  Nothing -> d
  Just x -> x
