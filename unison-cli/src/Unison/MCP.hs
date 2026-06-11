module Unison.MCP (runOnStdIO, initServer) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as KeyMap
import Network.MCP.Server qualified as MCP
import Network.MCP.Server.StdIO qualified as MCP
import Network.MCP.Types
import Text.RawString.QQ (r)
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Codebase (Codebase)
import Unison.MCP.Prompts (prompts)
import Unison.MCP.StaticResources (staticResources)
import Unison.MCP.Tools (tools)
import Unison.MCP.Types
import Unison.MCP.Wrapper qualified as MCPWrapper
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Runtime (Runtime)
import Unison.Symbol (Symbol)
import UnliftIO.STM (newTVarIO, readTVarIO)

serverDescription :: Text
serverDescription =
  [r|
        This server provides tools for interacting with Unison Code locally, such as typechecking or reading
        documentation, as well as tools for searching Unison Share, which is a platform for sharing Unison projects and
        libraries.

        It also provides some mechanisms for editing and updating local Unison projects, such as installing libraries
        from Unison Share.

        Before doing any work in unison please read the file://unison-guide resource for information on how to write
        Unison.
    |]

initServer ::
  Codebase IO Symbol Ann ->
  Runtime Symbol ->
  Runtime Symbol ->
  Maybe FilePath ->
  Text ->
  AuthN.AuthenticatedHttpClient ->
  IO MCP.Server
initServer codebase runtime sbRuntime workDir ucmVersion authenticatedHTTPClient = do
  sessionContext <- newTVarIO Nothing
  let env =
        Env
          { codebase,
            runtime,
            sbRuntime,
            ucmVersion,
            workDir,
            authenticatedHTTPClient,
            sessionContext
          }
  -- Create server
  let serverInfo = Implementation "unison-mcp" "0.0.1"

  runMCP env $ MCPWrapper.mkServer serverInfo serverDescription staticResources tools prompts (sessionContextPreprocessor env)

-- | Args preprocessor that injects @projectContext@ from session state
-- when the tool call omits it. Operates entirely on the JSON args before
-- the tool's FromJSON deserializer runs, so per-tool changes aren't
-- required.
--
-- /Staleness/: the session-state TVar is only mutated by deliberate
-- 'set-session-context' calls and by tools that change the user's
-- working scope ('project-rename', 'branch-delete' on the pinned
-- branch). A wrong value self-heals on the next call — the tool that
-- can't find the project simply errors.
sessionContextPreprocessor :: Env -> Text -> Aeson.Value -> MCP Aeson.Value
sessionContextPreprocessor env _toolName args = do
  mctx <- readTVarIO env.sessionContext
  pure $ case (args, mctx) of
    (Aeson.Object obj, Just ctx)
      | not (KeyMap.member (AesonKey.fromText "projectContext") obj) ->
          Aeson.Object (KeyMap.insert (AesonKey.fromText "projectContext") (Aeson.toJSON ctx) obj)
    _ -> args

-- | Run the MCP server until we hit EOF.
runOnStdIO ::
  Codebase IO Symbol Ann ->
  Runtime Symbol ->
  Runtime Symbol ->
  FilePath ->
  Text ->
  AuthN.AuthenticatedHttpClient ->
  IO ()
runOnStdIO codebase runtime sbRuntime workDir ucmVersion authenticatedHTTPClient = do
  server <- initServer codebase runtime sbRuntime (Just workDir) ucmVersion authenticatedHTTPClient
  -- Start the server with StdIO transport
  MCP.runServerWithSTDIO server
