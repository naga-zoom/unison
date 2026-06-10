{-# LANGUAGE OverloadedStrings #-}

-- | Convention-based recognition of /protected/ branch names.
--
-- This is the lightweight version of the "WritableBranch" idea from the
-- early gap-analysis plan. Rather than introducing a wrapper type that
-- every mutator signature would need to thread, we expose a pure classifier
-- that destructive tools call before invoking the underlying handler.
-- Convention-based, not type-level — the cost of forgetting the check is a
-- bug, not a compile error, but each destructive tool can add a single-line
-- guard and a one-line lint test asserts that they all do.
--
-- A branch is /protected/ when its name is conventionally significant to
-- the project's lifecycle: @main@, anything under @releases/@, and the
-- temporary @update-@\/@merge-@\/@upgrade-@ branches that automated update
-- flows create (they shouldn't be deleted by manual one-off calls; the
-- @reap-temp-branches@ tool is the dedicated path).
module Unison.MCP.Domain.BranchProtection
  ( ProtectedReason (..),
    isProtected,
    formatProtectionReason,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

-- | Why a branch name is protected.
data ProtectedReason
  = IsMain
  | IsReleaseFamily -- @releases/...@
  | IsTempBranch -- @update-...@, @merge-...@, @upgrade-...@
  deriving (Eq, Show)

-- | Classify a branch name. 'Nothing' = freely deletable; 'Just' = some
-- conventional protection applies.
isProtected :: Text -> Maybe ProtectedReason
isProtected n
  | n == "main" = Just IsMain
  | "releases/" `Text.isPrefixOf` n = Just IsReleaseFamily
  | any (`Text.isPrefixOf` n) ["update-", "merge-", "upgrade-"] = Just IsTempBranch
  | otherwise = Nothing

-- | Render a 'ProtectedReason' as a user-readable message. Used by tool
-- handlers when surfacing the refusal.
formatProtectionReason :: Text -> ProtectedReason -> Text
formatProtectionReason name reason =
  "branch '"
    <> name
    <> "' is protected ("
    <> case reason of
      IsMain -> "main branch is never auto-deletable"
      IsReleaseFamily -> "release-family branches must be deleted explicitly via a release-management path"
      IsTempBranch -> "temp branches should be deleted via the reap-temp-branches tool, not branch-delete"
    <> "). Pass force=true to override."
