{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Session-state tools: let an agent set a default 'ProjectContext'
-- once per session instead of re-passing it on every tool call.
--
-- See @docs/mcp-spec-extensions/session-state-and-cached-resources.md@
-- for the motivating analysis. The implementation lives at three
-- locations:
--
-- 1. 'Unison.MCP.Types.Env.sessionContext' — the TVar that holds it.
-- 2. 'Unison.MCP.sessionContextPreprocessor' — the dispatch-time
--    JSON-args injector that fills in missing @projectContext@.
-- 3. This module — the three tools that read \/ write \/ clear the
--    state.
--
-- Staleness contract:
--
-- * Setting a context that doesn't exist is allowed (we don't
--   validate). The first downstream tool call that needs it fails
--   with a clear "project not found" / "branch not found" error and
--   the agent re-sets it.
-- * Project rename and branch-delete tools clear the session if the
--   pinned context becomes unreachable (mitigation, not strict
--   guarantee — best-effort).
module Unison.MCP.Tools.Session
  ( setSessionContextTool,
    getSessionContextTool,
    clearSessionContextTool,
  )
where

import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text
import Unison.MCP.Types
import Unison.MCP.Wrapper
import UnliftIO.STM (atomically, readTVarIO, writeTVar)

setSessionContextTool :: Tool MCP
setSessionContextTool =
  Tool
    { toolName = toToolName SetSessionContextTool,
      toolDescription =
        "Set the session's default projectContext. Subsequent tool \
        \calls that omit `projectContext` will use this value. Saves \
        \~60 bytes per call across the surface (~80% of tools take \
        \projectContext). Call once near the start of a session, then \
        \omit projectContext from later calls.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Set Session Context",
            readOnlyHint = Just False,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @ProjectContextArgument,
      toolHandler = \(ProjectContextArgument ctx) -> handleToolError $ do
        env <- asks id
        atomically $ writeTVar env.sessionContext (Just ctx)
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode (Aeson.object ["ok" Aeson..= True, "context" Aeson..= ctx])
    }

getSessionContextTool :: Tool MCP
getSessionContextTool =
  Tool
    { toolName = toToolName GetSessionContextTool,
      toolDescription =
        "Return the session's current default projectContext, or null \
        \if unset.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Get Session Context",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @(),
      toolHandler = \() -> handleToolError $ do
        env <- asks id
        m <- readTVarIO env.sessionContext
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode (Aeson.object ["context" Aeson..= m])
    }

clearSessionContextTool :: Tool MCP
clearSessionContextTool =
  Tool
    { toolName = toToolName ClearSessionContextTool,
      toolDescription =
        "Clear the session's default projectContext. After this, tool \
        \calls that omit projectContext will fail until either a new \
        \context is set or projectContext is included in each call.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Clear Session Context",
            readOnlyHint = Just False,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @(),
      toolHandler = \() -> handleToolError $ do
        env <- asks id
        atomically $ writeTVar env.sessionContext Nothing
        pure $ textToolResult "{\"ok\":true}"
    }
