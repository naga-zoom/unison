{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @pull@ MCP tool: fetch a remote project branch into the local
-- branch.
--
-- Wraps 'Unison.Codebase.Editor.HandleInput.Pull.handlePull'. The local
-- branch is set from @projectContext@ (made current via 'cliToMCP' before
-- the handler runs). The remote source is required — pulling without a
-- specified source isn't a coherent agent operation (unlike push, where
-- the branch's remote-tracking can be used as default).
--
-- Default 'pullMode' is @with-history@ — preserves the full causal chain.
-- Pass @without-history@ to fetch only the current head.
module Unison.MCP.Tools.Pull
  ( pullTool,
    parsePullMode,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text
import Data.These (These (..))
import Unison.Codebase.Editor.HandleInput.Pull (handlePull)
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.RemoteRepo qualified as RemoteRepo
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.MCP.Cli (cliToMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Prelude
import Unison.Project (ProjectBranchNameOrLatestRelease (..))

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

pullTool :: Tool MCP
pullTool =
  Tool
    { toolName = toToolName PullTool,
      toolDescription =
        "Pull a remote project branch from Unison Share into the local \
        \branch (from projectContext). Source {project, branch} is \
        \required. Use branch 'latest-release' to fetch the latest \
        \released version. Default pullMode is 'with-history' (preserves \
        \causal chain); 'without-history' fetches only the head.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Pull from Share",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just True
          },
      toolArgType = Proxy @PullToolArguments,
      toolHandler = \(PullToolArguments {projectContext, source, pullMode}) -> handleToolError $ do
        let mode = maybe Input.PullWithHistory parsePullMode pullMode
        let sourceTarget = makePullSourceTarget source
        (_r, output) <- cliToMCP projectContext (const $ pure ()) (handlePull sourceTarget mode)
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
    }

-- ----------------------------------------------------------------------------
-- Wire-format parsers
-- ----------------------------------------------------------------------------

-- | Parse the @pullMode@ string. Unknown values fall back to
-- 'PullWithHistory' (default).
parsePullMode :: Text -> Input.PullMode
parsePullMode = \case
  "with-history" -> Input.PullWithHistory
  "without-history" -> Input.PullWithoutHistory
  _ -> Input.PullWithHistory

-- ----------------------------------------------------------------------------
-- Source assembly
-- ----------------------------------------------------------------------------

-- | Build a 'PullSourceTarget1' from the agent's request. The local
-- branch is the current branch (set by 'cliToMCP' from @projectContext@),
-- so no explicit target is needed.
makePullSourceTarget :: PullSource -> Input.PullSourceTarget
makePullSourceTarget src =
  let remote = RemoteRepo.ReadShare'ProjectBranch (sourceThese src)
   in Input.PullSourceTarget1 remote

sourceThese :: PullSource -> These ProjectName ProjectBranchNameOrLatestRelease
sourceThese src = case (src.project, src.branch) of
  (Just p, Just b) -> These (UnsafeProjectName p) (parseBranchOrLatest b)
  (Just p, Nothing) -> This (UnsafeProjectName p)
  (Nothing, Just b) -> That (parseBranchOrLatest b)
  (Nothing, Nothing) -> error "PullSource must have at least project or branch"

parseBranchOrLatest :: Text -> ProjectBranchNameOrLatestRelease
parseBranchOrLatest = \case
  "latest-release" -> ProjectBranchNameOrLatestRelease'LatestRelease
  other -> ProjectBranchNameOrLatestRelease'Name (UnsafeProjectBranchName other)
