{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @pipeline@ MCP tool: run a sequence of other MCP tool calls
-- inside one server-side dispatch.
--
-- A typical agent workflow is N round-trips: tool-A → response → tool-B
-- → response → tool-C. Each round-trip is JSON encode + transport +
-- decode at both ends. For setup-style chains (create project →
-- install lib → create branch → add def → run tests), the per-call
-- overhead dominates the actual work.
--
-- The pipeline tool flattens these into a single request:
--
-- @
-- { "steps": [
--     { "tool": "project-create",      "arguments": { ... } },
--     { "tool": "lib-install",         "arguments": { ... } },
--     { "tool": "create-branch",       "arguments": { ... } },
--     { "tool": "update-definitions",  "arguments": { ... } },
--     { "tool": "run-tests",           "arguments": { ... } }
--   ],
--   "stopOnFirstError": true
-- }
-- @
--
-- Returns @{ steps: [{ index, tool, ok, content }], stopped: Bool }@.
-- Failures are surfaced per-step; the @stopped@ flag indicates whether
-- @stopOnFirstError@ short-circuited the chain.
--
-- The pipeline tool is /not/ recursive — calling @pipeline@ from a
-- pipeline step is treated as "tool not found" (the registry passed to
-- the handler excludes the pipeline itself to make this a static
-- guarantee).
module Unison.MCP.Tools.Pipeline
  ( pipelineTool,
  )
where

import Control.Monad.Except (ExceptT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.MCP.Types qualified as MCP
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Prelude

-- | The pipeline tool takes a snapshot of the registry as its first
-- argument so that the handler can dispatch into it.
pipelineTool :: Map Text (Tool MCP) -> Tool MCP
pipelineTool registry =
  Tool
    { toolName = toToolName PipelineTool,
      toolDescription =
        "Run a sequence of MCP tool calls in one server-side dispatch. \
        \Cuts per-call round-trip overhead for setup chains (project- \
        \create + lib-install + create-branch + update + run-tests). \
        \steps: [{tool: <name>, arguments: <args>}], stopOnFirstError: \
        \bool (default true). Returns {steps: [{index, tool, ok, \
        \content}], stopped: bool}. Cannot recurse into itself.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Pipeline",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy @PipelineToolArguments,
      toolHandler = \(PipelineToolArguments {steps, stopOnFirstError}) -> handleToolError $ do
        let stopOnErr = fromMaybe True stopOnFirstError
        (reversedResults, stopped) <- foldM (runStep stopOnErr) ([], False) (zip [0 ..] steps)
        let result =
              Aeson.object
                [ "steps" Aeson..= reverse reversedResults,
                  "stopped" Aeson..= stopped
                ]
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode result
    }
  where
    runStep stopOnErr (acc, stopped) (i, PipelineStep {tool, arguments})
      | stopped = pure (acc, stopped)
      | otherwise = do
          stepResult <- dispatchStep i tool arguments
          let stepOk = stepResult.ok
          pure (stepResult : acc, stopOnErr && not stepOk)

    dispatchStep :: Int -> Text -> Aeson.Value -> ExceptT Text MCP StepResult
    dispatchStep i name args =
      case Map.lookup name registry of
        Nothing ->
          pure $ StepResult i name False (errorContent ("Tool not found in pipeline registry: " <> name))
        Just (Tool {toolHandler}) ->
          case Aeson.fromJSON args of
            Aeson.Error e ->
              pure $ StepResult i name False (errorContent ("arg parse: " <> Text.pack e))
            Aeson.Success a -> do
              r <- lift (toolHandler a)
              pure $ StepResult i name (not r.callToolIsError) (resultContent r)

    errorContent msg =
      Aeson.object ["error" Aeson..= msg]

    resultContent r =
      case r.callToolContent of
        (MCP.ToolContent _ (Just txt) : _) ->
          case Aeson.eitherDecodeStrict (Text.encodeUtf8 txt) of
            Right (v :: Aeson.Value) -> v
            Left _ -> Aeson.toJSON txt
        _ -> Aeson.Null

data StepResult = StepResult
  { index :: Int,
    tool :: Text,
    ok :: Bool,
    content :: Aeson.Value
  }

instance Aeson.ToJSON StepResult where
  toJSON s =
    Aeson.object
      [ "index" Aeson..= s.index,
        "tool" Aeson..= s.tool,
        "ok" Aeson..= s.ok,
        "content" Aeson..= s.content
      ]
