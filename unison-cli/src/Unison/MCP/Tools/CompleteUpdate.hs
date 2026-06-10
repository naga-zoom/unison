{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @complete-update@ MCP tool: enumerate temporary update/merge/upgrade
-- branches in a project and report which are fast-forward-eligible into the
-- target branch.
--
-- v1 is /read-only/: it identifies candidates and reports whether each can
-- be FF-merged into the target. The agent then chooses what to do — invoke
-- the existing merge tool, leave them, or wait for the v2 apply path that
-- includes transactional rollback.
--
-- Returns structured JSON:
-- @{candidates: [{tempBranch, candidateHead, ffEligible, reason}],
--   totalCount}@.
module Unison.MCP.Tools.CompleteUpdate
  ( completeUpdateTool,
    isTempBranchName,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import U.Codebase.Sqlite.DbId (CausalHashId, ProjectBranchId, ProjectId)
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase qualified as Codebase
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Prelude
import Unison.Sqlite (Transaction)
import UnliftIO qualified

-- ----------------------------------------------------------------------------
-- Wire format
-- ----------------------------------------------------------------------------

data CompleteUpdateResponse = CompleteUpdateResponse
  { candidates :: [Candidate],
    totalCount :: Int
  }

instance Aeson.ToJSON CompleteUpdateResponse where
  toJSON r =
    Aeson.object
      [ "candidates" Aeson..= r.candidates,
        "totalCount" Aeson..= r.totalCount
      ]

data Candidate = Candidate
  { tempBranch :: Text,
    ffEligible :: Bool,
    reason :: Maybe Text
  }

instance Aeson.ToJSON Candidate where
  toJSON c =
    Aeson.object
      [ "tempBranch" Aeson..= c.tempBranch,
        "ffEligible" Aeson..= c.ffEligible,
        "reason" Aeson..= c.reason
      ]

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

completeUpdateTool :: Tool MCP
completeUpdateTool =
  Tool
    { toolName = toToolName CompleteUpdateTool,
      toolDescription =
        "Enumerate temporary update/merge/upgrade branches in the project \
        \and report which can be fast-forward-merged into the target branch \
        \(passed as projectContext.branchName, typically 'main'). \
        \v1 is read-only — the agent decides what to do with the candidates. \
        \Returns structured JSON \
        \{candidates: [{tempBranch, ffEligible, reason}], totalCount}.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Complete Update (enumerate FF candidates)",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @CompleteUpdateToolArguments,
      toolHandler = \(CompleteUpdateToolArguments {projectContext}) -> handleToolError $ do
        codebase <- asks (.codebase)
        result <- UnliftIO.liftIO $ Codebase.runTransaction codebase $ enumerate projectContext
        case result of
          Left err -> throwError err
          Right cs ->
            pure $
              textToolResult $
                Text.decodeUtf8 . BL.toStrict $
                  Aeson.encode
                    CompleteUpdateResponse
                      { candidates = cs,
                        totalCount = length cs
                      }
    }

-- ----------------------------------------------------------------------------
-- Enumeration in a single transaction
-- ----------------------------------------------------------------------------

enumerate :: ProjectContext -> Transaction (Either Text [Candidate])
enumerate projectContext = do
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
              bid /= targetBranch.branchId, -- never report the target
              isTempBranchName (into @Text bn)
            ]
      cs <- traverse (analyzeCandidate project.projectId targetHeadId) temps
      pure $ Right cs

analyzeCandidate ::
  ProjectId ->
  CausalHashId ->
  (ProjectBranchId, ProjectBranchName) ->
  Transaction Candidate
analyzeCandidate projectId targetHeadId (bid, bn) = do
  candidateHeadId <- Q.expectProjectBranchHead projectId bid
  beforeResult <- Q.before targetHeadId candidateHeadId
  pure $
    if beforeResult
      then
        Candidate
          { tempBranch = into @Text bn,
            ffEligible = True,
            reason = Nothing
          }
      else
        Candidate
          { tempBranch = into @Text bn,
            ffEligible = False,
            reason = Just "target head is not an ancestor of the candidate head; merge would not be a fast-forward"
          }

-- ----------------------------------------------------------------------------
-- Pure helper — branch-name pattern recognizer (unit-testable)
-- ----------------------------------------------------------------------------

-- | Recognize conventional UCM \"temporary\" branches created during update,
-- merge, or upgrade workflows. Examples:
--
-- * @update-main@, @update-main-2@, @update-flow-rename-3@
-- * @merge-foo-into-main@
-- * @upgrade-base-1-0-0-to-base-2-0-0@
--
-- Does not match production branches (@main@, @releases/*@, plain feature
-- branches).
isTempBranchName :: Text -> Bool
isTempBranchName n =
  any
    (`Text.isPrefixOf` n)
    [ "update-",
      "merge-",
      "upgrade-"
    ]
