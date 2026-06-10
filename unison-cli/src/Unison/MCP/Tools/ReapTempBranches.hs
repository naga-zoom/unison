{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @reap-temp-branches@ MCP tool: identify temporary update / merge /
-- upgrade branches in a project that have already been integrated into a
-- target branch (i.e., the temp branch's head is an ancestor of the
-- target's head) — and optionally delete them.
--
-- Defaults to /dry-run/: returns the report without deleting. Pass
-- @apply: true@ to actually delete the safe candidates.
--
-- Pass @force: true@ to bypass the ancestor check entirely and delete every
-- temp branch matching the conventional name patterns. Combine with
-- @apply: true@ to actually delete them. Use carefully.
module Unison.MCP.Tools.ReapTempBranches
  ( reapTempBranchesTool,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text
import U.Codebase.Sqlite.DbId (CausalHashId, ProjectBranchId, ProjectId)
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase qualified as Codebase
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName (..))
import Unison.MCP.Tools.CompleteUpdate (isTempBranchName)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Prelude
import Unison.Sqlite (Transaction)
import UnliftIO qualified

-- ----------------------------------------------------------------------------
-- Wire format
-- ----------------------------------------------------------------------------

data ReapResponse = ReapResponse
  { candidates :: [Candidate],
    totalCount :: Int,
    deletedCount :: Int
  }

instance Aeson.ToJSON ReapResponse where
  toJSON r =
    Aeson.object
      [ "candidates" Aeson..= r.candidates,
        "totalCount" Aeson..= r.totalCount,
        "deletedCount" Aeson..= r.deletedCount
      ]

data Candidate = Candidate
  { tempBranch :: Text,
    isAncestor :: Bool,
    deleted :: Bool,
    reason :: Maybe Text
  }

instance Aeson.ToJSON Candidate where
  toJSON c =
    Aeson.object
      [ "tempBranch" Aeson..= c.tempBranch,
        "isAncestor" Aeson..= c.isAncestor,
        "deleted" Aeson..= c.deleted,
        "reason" Aeson..= c.reason
      ]

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

reapTempBranchesTool :: Tool MCP
reapTempBranchesTool =
  Tool
    { toolName = toToolName ReapTempBranchesTool,
      toolDescription =
        "Identify temporary update/merge/upgrade branches in the project \
        \that have already been integrated into the target branch (from \
        \projectContext, typically 'main'). Default behaviour is dry-run: \
        \emits the report without deleting. Pass apply=true to actually \
        \delete safe candidates (those whose head is an ancestor of the \
        \target's head). Pass force=true to bypass the ancestor check \
        \entirely — only use when you accept losing the branch's \
        \unintegrated work.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Reap Temp Branches",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @ReapTempBranchesToolArguments,
      toolHandler = \(ReapTempBranchesToolArguments {projectContext, apply, force}) -> handleToolError $ do
        codebase <- asks (.codebase)
        let doApply = fromMaybe False apply
        let doForce = fromMaybe False force
        result <- UnliftIO.liftIO $ Codebase.runTransaction codebase $ reap projectContext doApply doForce
        case result of
          Left err -> throwError err
          Right (cs, deletedCount) ->
            pure $
              textToolResult $
                Text.decodeUtf8 . BL.toStrict $
                  Aeson.encode
                    ReapResponse
                      { candidates = cs,
                        totalCount = length cs,
                        deletedCount = deletedCount
                      }
    }

-- ----------------------------------------------------------------------------
-- Implementation in a single transaction
-- ----------------------------------------------------------------------------

reap :: ProjectContext -> Bool -> Bool -> Transaction (Either Text ([Candidate], Int))
reap projectContext doApply doForce = do
  let pName = projectContext.projectName
  let bName = projectContext.branchName
  mPB <- ProjectUtils.getProjectAndBranchByNames (ProjectAndBranch pName bName)
  case mPB of
    Nothing -> pure $ Left $ "Target project/branch not found: " <> into @Text pName <> "/" <> into @Text bName
    Just (ProjectAndBranch project targetBranch) -> do
      targetHeadId <- Q.expectProjectBranchHead project.projectId targetBranch.branchId
      allBranches <- Q.loadAllProjectBranchesBeginningWith project.projectId Nothing
      let temps =
            [ (bid, bn)
            | (bid, bn) <- allBranches,
              bid /= targetBranch.branchId,
              isTempBranchName (into @Text bn)
            ]
      (cs, deletedCount) <- foldM (analyzeAndMaybeDelete project.projectId targetHeadId doApply doForce) ([], 0) temps
      pure $ Right (reverse cs, deletedCount)

analyzeAndMaybeDelete ::
  ProjectId ->
  CausalHashId ->
  Bool ->
  Bool ->
  ([Candidate], Int) ->
  (ProjectBranchId, ProjectBranchName) ->
  Transaction ([Candidate], Int)
analyzeAndMaybeDelete projectId targetHeadId doApply doForce (acc, deletedCount) (bid, bn) = do
  candidateHeadId <- Q.expectProjectBranchHead projectId bid
  ancestor <- Q.before candidateHeadId targetHeadId
  let safeToDelete = ancestor || doForce
  if doApply && safeToDelete
    then do
      Q.deleteProjectBranch projectId bid
      let c =
            Candidate
              { tempBranch = into @Text bn,
                isAncestor = ancestor,
                deleted = True,
                reason = Just (if ancestor then "head is ancestor of target; reaped" else "force=true; reaped")
              }
      pure (c : acc, deletedCount + 1)
    else do
      let c =
            Candidate
              { tempBranch = into @Text bn,
                isAncestor = ancestor,
                deleted = False,
                reason = case (doApply, safeToDelete) of
                  (True, False) -> Just "head is not an ancestor of target; skipped (set force=true to delete anyway)"
                  (False, _) -> Just "dry-run; pass apply=true to delete"
                  _ -> Nothing
              }
      pure (c : acc, deletedCount)
