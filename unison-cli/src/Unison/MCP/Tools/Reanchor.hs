{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @reanchor@ MCP tool: render source for definitions with hash
-- references replaced by names — the agent's typical workflow after a
-- 'detect-stale' report flags definitions whose dependencies are no
-- longer named.
--
-- Substitutions are plain 'Data.Text.replace'. Pass each mapping as
-- @{ "hash": "#abc123", "name": "fully.qualified.Name" }@ — the @hash@
-- field should include the leading @#@ so it doesn't accidentally
-- match anything else.
module Unison.MCP.Tools.Reanchor
  ( reanchorTool,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Unison.Codebase.Editor.Input qualified as Input
import Unison.HashQualified qualified as HQ
import Unison.MCP.Cli (CliOutput (..), handleInputMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Prelude

reanchorTool :: Tool MCP
reanchorTool =
  Tool
    { toolName = toToolName ReanchorTool,
      toolDescription =
        "Render the source of one or more definitions, replacing hash \
        \references with names. Each mapping is `{hash, name}` — the \
        \hash should include the leading `#`. Pair with detect-stale, \
        \which surfaces the hashes that need re-anchoring.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Reanchor",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @ReanchorToolArguments,
      toolHandler = \(ReanchorToolArguments {projectContext, names, mappings}) -> handleToolError $ do
        case NEL.nonEmpty names of
          Nothing ->
            pure $ errorToolResult "No names provided to reanchor"
          Just nonEmptyNames -> do
            let names' = HQ.NameOnly <$> nonEmptyNames
            output <- handleInputMCP projectContext [Right $ Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal names']
            let rewritten = applyMappings mappings output
            pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode rewritten
    }

applyMappings :: [HashRename] -> CliOutput -> CliOutput
applyMappings ms out =
  out
    { outputMessages = map (substituteAll ms) out.outputMessages,
      sourceCodeUpdates = map (substituteAll ms) out.sourceCodeUpdates
    }

substituteAll :: [HashRename] -> Text -> Text
substituteAll ms t = foldl' apply t ms
  where
    apply acc m
      | Text.null m.hash = acc
      | otherwise = Text.replace m.hash m.name acc
