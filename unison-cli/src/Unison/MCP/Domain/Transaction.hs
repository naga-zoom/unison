{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Multi-step orchestration with compensating-action rollback, at the
-- MCP layer.
--
-- A 'Plan' is a sequence of 'Step's. Each step has a forward action and
-- an inverse (a compensating action that undoes the forward effect).
-- 'runPlan' executes the forwards in order; on failure of any step, it
-- runs the inverses of all preceding /completed/ steps in reverse,
-- best-effort.
--
-- Steps live in the 'MCP' monad rather than 'Cli' deliberately: 'MCP' is
-- 'MonadUnliftIO', so 'UnliftIO.try' can catch synchronous IO exceptions
-- thrown by a step. A step that needs Cli access invokes 'cliToMCP'
-- inside its body and returns to MCP between forward steps. The
-- MCP-boundary is also the natural rollback granularity: each step is a
-- discrete user-observable unit.
--
-- /Scope and limitations:/
--
-- * Compensation-based, not ACID. Steps that cross process boundaries
--   (push to Share, install a remote lib) can be /compensated/ — the
--   inverse is whatever the agent considers a best-effort undo — not
--   literally rolled back.
-- * IO exceptions thrown by a step are caught and trigger rollback.
--   Failures signaled by 'Cli.returnEarly' inside a step's
--   'cliToMCP'-wrapped Cli body propagate through 'cliToMCP' to MCP as a
--   non-empty 'CliOutput.errorMessages' — the step's @run@ should
--   inspect that and either succeed or throw to signal failure for
--   rollback purposes.
-- * Inverses are best-effort: an inverse that itself fails is recorded
--   in 'PlanRollbackFailed' but does not interrupt rollback of earlier
--   steps. The runner always tries to undo as much as it can.
module Unison.MCP.Domain.Transaction
  ( -- * Building plans
    Step (..),
    Plan,
    plan,

    -- * Running
    runPlan,
    PlanOutcome (..),
    StepFailure (..),
    InverseFailure (..),
  )
where

import Control.Exception (displayException)
import Data.Text qualified as Text
import Unison.MCP.Types (MCP)
import Unison.Prelude
import UnliftIO qualified

-- ----------------------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------------------

-- | One step in a 'Plan'.
data Step = Step
  { -- | Short identifier; used in 'PlanOutcome' for logging.
    name :: Text,
    -- | Forward action. May throw IO exceptions to signal failure;
    -- the runner catches them and triggers rollback.
    run :: MCP (),
    -- | Compensating action. Runs only if a later step fails after this
    -- one completed. Should be safe to re-run (best-effort idempotent).
    inverse :: MCP ()
  }

-- | A plan is a sequence of steps executed in order.
type Plan = [Step]

-- | Constructor alias for readability.
plan :: [Step] -> Plan
plan = id

-- | What happened when a plan ran.
data PlanOutcome
  = -- | All steps succeeded.
    PlanCommitted
  | -- | A step failed; rollback completed cleanly.
    PlanRolledBack StepFailure [Text]
  | -- | A step failed; one or more inverses also failed during rollback.
    PlanRollbackFailed StepFailure [Text] [InverseFailure]
  deriving (Eq, Show)

data StepFailure = StepFailure
  { stepName :: Text,
    reason :: Text
  }
  deriving (Eq, Show)

data InverseFailure = InverseFailure
  { inverseStepName :: Text,
    inverseReason :: Text
  }
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Runner
-- ----------------------------------------------------------------------------

-- | Execute a plan.
--
-- Forwards run in order. If a forward throws a synchronous IO exception,
-- the runner aborts forward progress and runs the inverses of every
-- preceding completed step in reverse. Inverses are best-effort: each
-- one's failure is recorded but does not stop the rollback of earlier
-- steps.
runPlan :: Plan -> MCP PlanOutcome
runPlan = go []
  where
    go ::
      -- | Accumulator of (stepName, inverse) for steps that COMPLETED,
      -- most recent first (cheap to extend; reverse on use).
      [(Text, MCP ())] ->
      -- | Remaining forward steps.
      [Step] ->
      MCP PlanOutcome
    go _ [] = pure PlanCommitted
    go completed (s : rest) = do
      result <- UnliftIO.try @_ @SomeException s.run
      case result of
        Right () ->
          go ((s.name, s.inverse) : completed) rest
        Left err ->
          let failure =
                StepFailure
                  { stepName = s.name,
                    reason = Text.pack (displayException err)
                  }
           in rollback failure completed

    rollback :: StepFailure -> [(Text, MCP ())] -> MCP PlanOutcome
    rollback failure inverses = do
      (rolled, failed) <- foldM runInverse ([], []) inverses
      case failed of
        [] -> pure (PlanRolledBack failure (reverse rolled))
        _ -> pure (PlanRollbackFailed failure (reverse rolled) (reverse failed))

    runInverse :: ([Text], [InverseFailure]) -> (Text, MCP ()) -> MCP ([Text], [InverseFailure])
    runInverse (rolled, failed) (n, inv) = do
      result <- UnliftIO.try @_ @SomeException inv
      case result of
        Right () -> pure (n : rolled, failed)
        Left err ->
          pure
            ( rolled,
              InverseFailure {inverseStepName = n, inverseReason = Text.pack (displayException err)} : failed
            )
