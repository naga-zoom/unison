{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the compact-path JSON form of 'ProjectContext'.
--
-- The 'FromJSON' instance accepts two shapes:
--
--   1. Object: @{"projectName": "foo", "branchName": "main"}@
--   2. Compact string: @"foo:main"@
--
-- These tests pin both forms and the failure modes for malformed input.
module Unison.Test.MCP.CompactPath where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import EasyTest
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.MCP.Types (ProjectContext (..))

decode :: String -> Either String ProjectContext
decode = Aeson.eitherDecode . BL.pack

test :: Test ()
test =
  scope "mcp.compact-path" . tests $
    [ scope "object form decodes" $
        case decode "{\"projectName\":\"temper\",\"branchName\":\"main\"}" of
          Right pc -> do
            expectEqual (pc.projectName) (UnsafeProjectName "temper")
            expectEqual (pc.branchName) (UnsafeProjectBranchName "main")
          Left e -> crash $ "expected ok; got: " ++ e,
      scope "compact form decodes" $
        case decode "\"temper:main\"" of
          Right pc -> do
            expectEqual (pc.projectName) (UnsafeProjectName "temper")
            expectEqual (pc.branchName) (UnsafeProjectBranchName "main")
          Left e -> crash $ "expected ok; got: " ++ e,
      scope "compact form with Share-style @owner/proj" $
        case decode "\"@unison/base:main\"" of
          Right pc -> do
            expectEqual (pc.projectName) (UnsafeProjectName "@unison/base")
            expectEqual (pc.branchName) (UnsafeProjectBranchName "main")
          Left _ -> crash "expected ok",
      scope "compact form with release-path branch" $
        case decode "\"@unison/base:releases/drafts/1.0.0\"" of
          Right pc -> do
            expectEqual (pc.projectName) (UnsafeProjectName "@unison/base")
            expectEqual (pc.branchName) (UnsafeProjectBranchName "releases/drafts/1.0.0")
          Left e -> crash $ "expected ok; got: " ++ e,
      scope "compact form without colon → fails" $
        case decode "\"temper\"" of
          Left _ -> ok
          Right _ -> crash "expected failure on missing colon",
      scope "compact form with empty project → fails" $
        case decode "\":main\"" of
          Left _ -> ok
          Right _ -> crash "expected failure on empty project",
      scope "compact form with empty branch → fails" $
        case decode "\"temper:\"" of
          Left _ -> ok
          Right _ -> crash "expected failure on empty branch",
      scope "non-object non-string → fails" $
        case decode "42" of
          Left _ -> ok
          Right _ -> crash "expected failure on numeric input"
    ]
