{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Unison.MCP.Domain.BranchProtection'.
--
-- The classifier is pure and convention-based; these assertions pin the
-- conventional set so a future contributor who edits the predicate sees a
-- broken test if they change classification of any well-known name.
module Unison.Test.MCP.BranchProtection where

import EasyTest
import Unison.MCP.Domain.BranchProtection
  ( ProtectedReason (..),
    isProtected,
  )

test :: Test ()
test =
  scope "mcp.branch-protection" . tests $
    [ scope "main is protected" $
        expectEqual (isProtected "main") (Just IsMain),
      scope "releases/drafts/5.0.0 is protected" $
        expectEqual (isProtected "releases/drafts/5.0.0") (Just IsReleaseFamily),
      scope "releases/3.0.0 is protected" $
        expectEqual (isProtected "releases/3.0.0") (Just IsReleaseFamily),
      scope "update-main is a temp branch" $
        expectEqual (isProtected "update-main") (Just IsTempBranch),
      scope "merge-foo-into-main is a temp branch" $
        expectEqual (isProtected "merge-foo-into-main") (Just IsTempBranch),
      scope "upgrade-base-1-0-0 is a temp branch" $
        expectEqual (isProtected "upgrade-base-1-0-0") (Just IsTempBranch),
      scope "feature/foo is NOT protected" $
        expectEqual (isProtected "feature/foo") Nothing,
      scope "wip-bug-123 is NOT protected" $
        expectEqual (isProtected "wip-bug-123") Nothing,
      scope "release (singular) is NOT protected" $
        -- Note: only 'releases/*' is protected; bare 'release' is a free name.
        expectEqual (isProtected "release") Nothing,
      scope "naked 'update' is NOT protected" $
        expectEqual (isProtected "update") Nothing,
      scope "empty string is NOT protected" $
        expectEqual (isProtected "") Nothing
    ]
