{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure helpers in 'Unison.MCP.Tools.CompleteUpdate'.
--
-- The enumeration + FF eligibility check touches Sqlite; exercised
-- end-to-end via integration smoke. These unit tests cover the pure
-- branch-name pattern recognizer.
module Unison.Test.MCP.CompleteUpdate where

import EasyTest
import Unison.MCP.Tools.CompleteUpdate (isTempBranchName)

test :: Test ()
test =
  scope "mcp.complete-update" . tests $
    [ scope "isTempBranchName" tempBranchCases
    ]

tempBranchCases :: Test ()
tempBranchCases =
  tests
    [ scope "update-main matches" $
        expect (isTempBranchName "update-main"),
      scope "update-main-2 matches" $
        expect (isTempBranchName "update-main-2"),
      scope "update-flow-rename matches" $
        expect (isTempBranchName "update-flow-rename"),
      scope "merge-foo-into-main matches" $
        expect (isTempBranchName "merge-foo-into-main"),
      scope "upgrade-base-1-0-0 matches" $
        expect (isTempBranchName "upgrade-base-1-0-0"),
      scope "main does not match" $
        expect (not (isTempBranchName "main")),
      scope "releases/drafts/5.0.0 does not match" $
        expect (not (isTempBranchName "releases/drafts/5.0.0")),
      scope "feature-foo does not match" $
        expect (not (isTempBranchName "feature-foo")),
      scope "empty string does not match" $
        expect (not (isTempBranchName "")),
      scope "naked 'update' (no hyphen) does not match" $
        expect (not (isTempBranchName "update"))
    ]
