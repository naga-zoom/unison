{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wrapper to provide safer interface in constructing an MCP server.
module Unison.MCP.Wrapper
  ( Tool (..),
    Prompt (..),
    HasInputSchema (..),
    mkServer,
    ArgsPreprocessor,
    CallToolResult (..),
    PromptArgument (..),
    StaticResources,
    Server,
    MCP.ServerCapabilities (..),
    MCP.ToolAnnotations (..),
    MCP.Implementation (..),
    MCP.ResourcesCapability (..),
    MCP.ToolsCapability (..),
    MCP.PromptsCapability (..),
    MCP.PromptContentType (..),
    errorToolResult,
    textToolResult,
    jsonToolResult,
    handleToolError,
  )
where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Data (Proxy)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Network.MCP.Server
import Network.MCP.Types (CallToolResult (CallToolResult))
import Network.MCP.Types qualified as MCP
import Unison.Prelude
import UnliftIO qualified
import UnliftIO.Environment (lookupEnv)

type StaticResources = Map Text (MCP.Resource, MCP.ResourceContent)

class HasInputSchema arg where
  toInputSchema :: Proxy arg -> Aeson.Value

instance HasInputSchema () where
  toInputSchema _ =
    Aeson.object
      [ ("type", Aeson.String "object"),
        ("properties", Aeson.object []),
        ("required", Aeson.Array mempty)
      ]

data Tool m
  = forall arg.
  (FromJSON arg, HasInputSchema arg) =>
  Tool
  { toolName :: Text,
    toolDescription :: Text,
    toolAnnotations :: MCP.ToolAnnotations,
    toolArgType :: Proxy arg,
    toolHandler :: arg -> m MCP.CallToolResult
  }

data Prompt m = Prompt
  { promptName :: Text,
    promptDescription :: Text,
    promptArgs :: Map Text PromptArgument,
    promptHandler :: Map Text Text -> m MCP.GetPromptResult
  }

data PromptArgument = PromptArgument
  { promptArgumentDescription :: Text,
    -- | Whether the argument is required
    promptArgumentRequired :: Bool
  }

-- | Hook fired on every @tools/call@ before the tool's FromJSON runs.
-- The first argument is the tool's name and the second is the raw
-- JSON args object; the result replaces the args. Use 'pure . id' for
-- a no-op preprocessor. See ucm-mcp's
-- @sessionContextPreprocessor@ for an example that injects session
-- defaults.
type ArgsPreprocessor m = Text -> Aeson.Value -> m Aeson.Value

mkServer :: (MonadUnliftIO m) => MCP.ServerInfo -> Text -> StaticResources -> [Tool m] -> [Prompt m] -> ArgsPreprocessor m -> m Server
mkServer serverInfo serverDescription staticResources tools prompts argsPreprocess = do
  let serverCapabilities =
        MCP.ServerCapabilities
          { resourcesCapability = Just $ MCP.ResourcesCapability (not $ Map.null staticResources),
            toolsCapability = Just $ MCP.ToolsCapability (not $ null tools),
            promptsCapability = Just $ MCP.PromptsCapability (not $ null prompts)
          }
  server <- liftIO $ createServer serverInfo serverCapabilities serverDescription

  doResources server staticResources
  doTools server tools argsPreprocess
  doPrompts server prompts

  pure server

doResources :: (MonadUnliftIO m) => Server -> StaticResources -> m ()
doResources server staticResources = do
  liftIO $ registerResources server (fst <$> Map.elems staticResources)

  -- Register resource read handler
  liftIO $ registerResourceReadHandler server $ \(MCP.ReadResourceRequest {resourceReadUri}) -> do
    case Map.lookup resourceReadUri staticResources of
      Just (_, content) ->
        pure . MCP.ReadResourceResult $ [content]
      _ -> pure $ MCP.ReadResourceResult []

-- | Default timeout for MCP tool calls in seconds
defaultMcpTimeoutSeconds :: Int
defaultMcpTimeoutSeconds = 60

-- | Get the MCP tool timeout in microseconds from UNISON_MCP_TIMEOUT env var.
-- The env var is specified in seconds for user convenience.
-- Defaults to 60 seconds if not set or invalid.
getMcpTimeoutMicroseconds :: (MonadIO m) => m Int
getMcpTimeoutMicroseconds = liftIO $ do
  lookupEnv "UNISON_MCP_TIMEOUT" <&> \case
    Just str -> maybe (defaultMcpTimeoutSeconds * 1_000_000) (* 1_000_000) (readMaybe str)
    Nothing -> defaultMcpTimeoutSeconds * 1_000_000

doTools :: (MonadUnliftIO m) => Server -> [Tool m] -> ArgsPreprocessor m -> m ()
doTools server tools argsPreprocess = do
  runInIO <- askRunInIO
  timeoutMicros <- getMcpTimeoutMicroseconds
  let timeoutSeconds = timeoutMicros `div` 1_000_000
  let toolMap = Map.fromList (tools <&> (\tool -> (toolName tool, tool)))
  let mcpTools =
        tools <&> \(Tool {toolName, toolDescription, toolAnnotations, toolArgType}) ->
          MCP.Tool
            { MCP.toolName,
              MCP.toolDescription = Just toolDescription,
              MCP.toolInputSchema = toInputSchema toolArgType,
              MCP.toolAnnotations = Just toolAnnotations
            }
  liftIO $ registerTools server mcpTools
  liftIO $ registerToolCallHandler server \(MCP.CallToolRequest {callToolName, callToolArguments}) -> runInIO $ do
    case Map.lookup callToolName toolMap of
      Just Tool {toolHandler} -> do
        processedArgs <- argsPreprocess callToolName callToolArguments
        case Aeson.fromJSON processedArgs of
          Aeson.Success arg ->
            UnliftIO.timeout timeoutMicros (toolHandler arg) >>= \case
              Nothing -> pure $ errorToolResult $ "Tool '" <> callToolName <> "' timed out after " <> Text.pack (show timeoutSeconds) <> " seconds."
              Just result -> pure result
          Aeson.Error err -> pure $ errorToolResult $ "Failed to parse arguments for tool '" <> callToolName <> "': " <> Text.pack err
      Nothing -> pure $ errorToolResult $ "Tool '" <> callToolName <> "' not found."

errorToolResult :: Text -> MCP.CallToolResult
errorToolResult errMsg =
  MCP.CallToolResult
    { MCP.callToolContent = [MCP.ToolContent MCP.TextualContent $ Just errMsg],
      MCP.callToolIsError = True
    }

doPrompts :: (MonadUnliftIO m) => Server -> [Prompt m] -> m ()
doPrompts server prompts = do
  let mcpPrompts =
        prompts <&> \(Prompt {promptName, promptDescription, promptArgs}) ->
          MCP.Prompt
            { MCP.promptName,
              MCP.promptDescription = Just promptDescription,
              MCP.promptArguments =
                promptArgs
                  & Map.toList
                  <&> \(argName, PromptArgument {promptArgumentDescription, promptArgumentRequired}) ->
                    MCP.PromptArgument
                      { MCP.promptArgumentName = argName,
                        MCP.promptArgumentDescription = Just promptArgumentDescription,
                        MCP.promptArgumentRequired = promptArgumentRequired
                      }
            }
  let promptsMap = Map.fromList $ prompts <&> (\p -> (promptName p, p))
  liftIO $ registerPrompts server mcpPrompts
  runInIO <- askRunInIO
  liftIO $ registerPromptHandler server $ \(MCP.GetPromptRequest {getPromptName, getPromptArguments}) -> runInIO do
    case Map.lookup getPromptName promptsMap of
      Nothing -> error $ "Prompt '" <> Text.unpack getPromptName <> "' not found."
      Just (Prompt {promptHandler}) -> do
        promptHandler getPromptArguments

textToolResult :: Text -> MCP.CallToolResult
textToolResult msg =
  MCP.CallToolResult
    { MCP.callToolContent = [MCP.ToolContent MCP.TextualContent $ Just msg],
      MCP.callToolIsError = False
    }

jsonToolResult :: (Aeson.ToJSON a) => a -> MCP.CallToolResult
jsonToolResult msg = textToolResult $ Text.pack $ BL.unpack $ Aeson.encode msg

-- | Run an MCP tool body that may short-circuit with a typed error
-- ('ExceptT Text'), converting a 'Left' into 'errorToolResult'. The monad
-- is left generic so per-file tool modules can use this regardless of which
-- specific MCP monad they live in.
handleToolError :: (Monad m) => ExceptT Text m MCP.CallToolResult -> m MCP.CallToolResult
handleToolError action = do
  result <- runExceptT action
  case result of
    Left err -> pure $ errorToolResult err
    Right res -> pure res
