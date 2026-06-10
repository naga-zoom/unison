{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure helpers in 'Unison.MCP.Tools.DetectStale'.
--
-- The MCP tool itself walks the codebase and is exercised end-to-end via
-- integration smoke. These unit tests cover the staleness predicate.
module Unison.Test.MCP.DetectStale where

import EasyTest
import Unison.MCP.Tools.DetectStale (isStaleRef)
import Unison.Reference qualified as Reference

test :: Test ()
test =
  scope "mcp.detect-stale" . tests $
    [ scope "isStaleRef" staleRefCases
    ]

staleRefCases :: Test ()
staleRefCases =
  tests
    [ scope "builtin never stale" $
        expect (not (isStaleRef (Reference.Builtin "Nat.+"))),
      scope "another builtin never stale" $
        expect (not (isStaleRef (Reference.Builtin "Text.empty"))),
      scope "empty-name builtin (edge) still not stale" $
        expect (not (isStaleRef (Reference.Builtin "")))
    ]
