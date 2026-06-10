{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Unison.MCP.Domain.Transaction'.
--
-- The runner needs an 'MCP' context, but we don't need a real MCP env
-- for the orchestration semantics — we test the algorithm against
-- in-memory effects (IORef writes) by lifting the test actions into
-- 'MCP' via 'liftIO'.
module Unison.Test.MCP.Transaction where

import Control.Exception (ErrorCall (..), throwIO)
import Data.IORef
import Data.Text (Text)
import EasyTest
import Unison.MCP.Domain.Transaction
  ( InverseFailure (..),
    PlanOutcome (..),
    Step (..),
    StepFailure (..),
    runPlan,
  )
import Unison.MCP.Types (MCP, runMCP)
import qualified Unison.MCP.Types as Types
import UnliftIO (liftIO)

test :: Test ()
test =
  scope "mcp.transaction" . tests $
    [ scope "empty plan commits" emptyPlanCommits,
      scope "all steps succeed → committed in order" allSucceed,
      scope "step failure rolls back completed inverses in reverse" failureRollsBack,
      scope "first step failure → no inverses run" firstStepFailsNoInverses,
      scope "inverse failure recorded but rollback continues" inverseFailureRecorded
    ]

-- ----------------------------------------------------------------------------
-- Test driver — runs MCP without a real Env via lifted IORef effects
-- ----------------------------------------------------------------------------

runInTest :: MCP a -> IO a
runInTest m = runMCP fakeEnv m
  where
    -- We never use the env in these tests (steps only do IORef stuff),
    -- so bottoming out is fine. The runtime never reaches these thunks.
    fakeEnv =
      Types.Env
        { Types.codebase = error "fakeEnv.codebase: tests never touch the codebase",
          Types.runtime = error "fakeEnv.runtime: tests never touch the runtime",
          Types.sbRuntime = error "fakeEnv.sbRuntime: tests never touch sbRuntime",
          Types.ucmVersion = "test",
          Types.workDir = Nothing,
          Types.authenticatedHTTPClient = error "fakeEnv.http: tests never touch HTTP"
        }

mkLog :: IO (IORef [Text], Text -> MCP ())
mkLog = do
  ref <- newIORef []
  let append msg = liftIO $ modifyIORef' ref (msg :)
  pure (ref, append)

readLog :: IORef [Text] -> IO [Text]
readLog ref = reverse <$> readIORef ref

throwStep :: Text -> MCP ()
throwStep msg = liftIO $ throwIO (ErrorCall (show msg))

-- ----------------------------------------------------------------------------
-- Cases
-- ----------------------------------------------------------------------------

emptyPlanCommits :: Test ()
emptyPlanCommits = do
  outcome <- io $ runInTest (runPlan [])
  expectEqual outcome PlanCommitted

allSucceed :: Test ()
allSucceed = do
  outcome <- io $ do
    (ref, append) <- mkLog
    let p =
          [ Step "step-1" (append "run-1") (append "inv-1"),
            Step "step-2" (append "run-2") (append "inv-2"),
            Step "step-3" (append "run-3") (append "inv-3")
          ]
    outcome <- runInTest (runPlan p)
    finalLog <- readLog ref
    pure (outcome, finalLog)
  case outcome of
    (PlanCommitted, log_) ->
      expectEqual log_ ["run-1", "run-2", "run-3"]
    other -> crash ("expected PlanCommitted; got " ++ show other)

failureRollsBack :: Test ()
failureRollsBack = do
  result <- io $ do
    (ref, append) <- mkLog
    let p =
          [ Step "step-1" (append "run-1") (append "inv-1"),
            Step "step-2" (append "run-2") (append "inv-2"),
            Step "step-3" (append "run-3" >> throwStep "boom") (append "inv-3"),
            Step "step-4" (append "run-4") (append "inv-4")
          ]
    outcome <- runInTest (runPlan p)
    finalLog <- readLog ref
    pure (outcome, finalLog)
  case result of
    (PlanRolledBack failure rolled, log_) -> do
      expectEqual failure.stepName "step-3"
      expectEqual rolled ["step-2", "step-1"]
      -- run-3 partially executed before the throw, then inverses in reverse,
      -- and step-4 never ran.
      expectEqual log_ ["run-1", "run-2", "run-3", "inv-2", "inv-1"]
    other -> crash ("expected PlanRolledBack on step-3; got " ++ show other)

firstStepFailsNoInverses :: Test ()
firstStepFailsNoInverses = do
  result <- io $ do
    (ref, append) <- mkLog
    let p =
          [ Step "step-1" (throwStep "early-fail") (append "inv-1"),
            Step "step-2" (append "run-2") (append "inv-2")
          ]
    outcome <- runInTest (runPlan p)
    finalLog <- readLog ref
    pure (outcome, finalLog)
  case result of
    (PlanRolledBack failure rolled, log_) -> do
      expectEqual failure.stepName "step-1"
      expectEqual rolled []
      expectEqual log_ []
    other -> crash ("expected PlanRolledBack on step-1 with no inverses; got " ++ show other)

inverseFailureRecorded :: Test ()
inverseFailureRecorded = do
  result <- io $ do
    (ref, append) <- mkLog
    let p =
          [ -- step-1 succeeds; its inverse will fail later.
            Step "step-1" (append "run-1") (throwStep "inv-1-fails"),
            Step "step-2" (append "run-2") (append "inv-2"),
            Step "step-3" (append "run-3" >> throwStep "boom") (append "inv-3")
          ]
    outcome <- runInTest (runPlan p)
    finalLog <- readLog ref
    pure (outcome, finalLog)
  case result of
    (PlanRollbackFailed failure rolled failedInverses, _log_) -> do
      expectEqual failure.stepName "step-3"
      -- step-2's inverse succeeds; step-1's inverse fails.
      expectEqual rolled ["step-2"]
      case failedInverses of
        [InverseFailure {inverseStepName = "step-1"}] -> ok
        other -> crash ("expected one InverseFailure for step-1; got " ++ show other)
    other -> crash ("expected PlanRollbackFailed; got " ++ show other)
