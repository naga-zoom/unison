{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @push@ MCP tool: publish a project branch to Unison Share.
--
-- Wraps 'Unison.Codebase.Editor.HandleInput.Push.handlePushRemoteBranch'.
-- The source branch is taken from @projectContext@ (set as current via
-- 'cliToMCP' before the handler runs). The target is optional — when
-- absent, the branch's existing remote-tracking is used.
--
-- Default 'pushBehavior' is 'RequireNonEmpty' (safest — refuses to push
-- into an empty remote namespace, preventing accidental writes to a fresh
-- branch). Agents that need a different behavior pass an explicit value.
module Unison.MCP.Tools.Push
  ( pushTool,
    parsePushBehavior,
  )
where

import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text
import Data.These (These (..))
import Unison.Codebase.Editor.HandleInput.Push (handlePushRemoteBranch)
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.PushBehavior (PushBehavior (..))
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.MCP.Cli (cliToMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Prelude

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

pushTool :: Tool MCP
pushTool =
  Tool
    { toolName = toToolName PushTool,
      toolDescription =
        "Push the source branch (from projectContext) to Unison Share. \
        \Target project/branch is optional — when omitted, the branch's \
        \remote-tracking is used. Default push behavior is RequireNonEmpty \
        \(refuses to push to an empty remote namespace). Set pushBehavior \
        \to 'force' to overwrite the remote head, or 'require-empty' to \
        \insist the remote namespace be empty.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Push to Share",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just True
          },
      toolArgType = Proxy @PushToolArguments,
      toolHandler = \(PushToolArguments {projectContext, target, pushBehavior}) -> handleToolError $ do
        _ <- asks (.codebase)
        let beh = maybe RequireNonEmpty parsePushBehavior pushBehavior
        let sourceTarget = makeSourceTarget projectContext target
        let pushInput = Input.PushRemoteBranchInput {sourceTarget, pushBehavior = beh}
        (_r, output) <- cliToMCP projectContext (const $ pure ()) (handlePushRemoteBranch pushInput)
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
    }

-- ----------------------------------------------------------------------------
-- PushBehavior wire format
-- ----------------------------------------------------------------------------

-- | Parse the wire-format @pushBehavior@ string. Unknown values fall back
-- to 'RequireNonEmpty' (the safest default).
parsePushBehavior :: Text -> PushBehavior
parsePushBehavior = \case
  "force" -> ForcePush
  "require-empty" -> RequireEmpty
  "require-non-empty" -> RequireNonEmpty
  _ -> RequireNonEmpty

-- ----------------------------------------------------------------------------
-- SourceTarget assembly
-- ----------------------------------------------------------------------------

-- | Build a 'PushSourceTarget' from the agent's request.
--
-- * No target: 'PushSourceTarget0' — both source and target inferred from
--   the current branch + its remote-tracking. projectContext has already
--   set the current branch via 'cliToMCP'.
-- * Target provided: 'PushSourceTarget2' with the source from
--   projectContext made explicit and the target as supplied.
makeSourceTarget :: ProjectContext -> Maybe PushTarget -> Input.PushSourceTarget
makeSourceTarget _src Nothing = Input.PushSourceTarget0
makeSourceTarget src (Just t) =
  let srcThese = These src.projectName src.branchName
      tgtThese = makeTargetThese t
   in Input.PushSourceTarget2 (Input.ProjySource srcThese) tgtThese

makeTargetThese :: PushTarget -> These ProjectName ProjectBranchName
makeTargetThese t = case (t.project, t.branch) of
  (Just p, Just b) -> These (UnsafeProjectName p) (UnsafeProjectBranchName b)
  (Just p, Nothing) -> This (UnsafeProjectName p)
  (Nothing, Just b) -> That (UnsafeProjectBranchName b)
  (Nothing, Nothing) -> error "PushTarget must have at least one of project or branch"
