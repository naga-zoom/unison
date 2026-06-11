{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Unison.MCP.Wire.truncateHash'. Pure, no fixtures
-- needed.
module Unison.Test.MCP.Wire where

import EasyTest
import Unison.MCP.Wire (truncateHash)

test :: Test ()
test =
  scope "mcp.wire.truncateHash" . tests $
    [ scope "long bare hash → 12-char prefix" $
        expectEqual
          (truncateHash "#0qbc2dfom7m4pputtdojo849g2mp5kkr00kvsvjktb07tcmo1jql53bg73bqiib35vja4a7059rcet0raf7jsh4d8vg5582ibinpqj8")
          "#0qbc2dfom7m4",
      scope "long hash with #N constructor suffix → preserves suffix" $
        expectEqual
          (truncateHash "#0qbc2dfom7m4pputtdojo849g2mp5kkr00kvsvjktb07tcmo1jql53bg73bqiib35vja4a7059rcet0raf7jsh4d8vg5582ibinpqj8#0")
          "#0qbc2dfom7m4#0",
      scope "hash with multi-digit constructor index" $
        expectEqual
          (truncateHash "#0qbc2dfom7m4pputtdojo849g2mp5kkr00kvsvjktb07tcmo1jql53bg73bqiib35vja4a7059rcet0raf7jsh4d8vg5582ibinpqj8#42")
          "#0qbc2dfom7m4#42",
      scope "already-short hash passes through unchanged" $
        expectEqual (truncateHash "#abc123") "#abc123",
      scope "exact 12-char body passes through unchanged" $
        expectEqual (truncateHash "#abc123def456") "#abc123def456",
      scope "12-char body + constructor passes through unchanged" $
        expectEqual (truncateHash "#abc123def456#1") "#abc123def456#1",
      scope "non-hash text passes through unchanged" $
        expectEqual (truncateHash "hello world") "hello world",
      scope "empty string passes through unchanged" $
        expectEqual (truncateHash "") "",
      scope "lone # passes through unchanged" $
        expectEqual (truncateHash "#") "#",
      scope "idempotent — truncating twice is the same as once" $
        let h = "#0qbc2dfom7m4pputtdojo849g2mp5kkr00kvsvjktb07tcmo#0"
         in expectEqual (truncateHash (truncateHash h)) (truncateHash h)
    ]
