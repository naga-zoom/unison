{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure helpers in 'Unison.MCP.Tools.Find'.
--
-- The MCP tool itself touches the codebase ('Branch.deepTerms' walk) and
-- is exercised end-to-end via integration. These unit tests cover the
-- pure name-convention classifier and the @kind@ wire-format mapping.
module Unison.Test.MCP.Find where

import EasyTest
import Unison.MCP.Domain.Pattern
  ( OpKind (..),
  )
import Unison.MCP.Tools.Find
  ( classifyTermNameByConvention,
    kindText,
  )

test :: Test ()
test =
  scope "mcp.find" . tests $
    [ scope "classifyTermNameByConvention" classifyCases,
      scope "kindText" kindTextCases
    ]

classifyCases :: Test ()
classifyCases =
  tests
    [ scope "plain term" $
        expectEqual (classifyTermNameByConvention "List.map") OpTerm,
      scope ".doc suffix → Doc" $
        expectEqual (classifyTermNameByConvention "List.doc") OpDoc,
      scope "nested .doc suffix → Doc" $
        expectEqual (classifyTermNameByConvention "x.y.z.doc") OpDoc,
      scope ".tests. infix → Test" $
        expectEqual (classifyTermNameByConvention "Nat.tests.addition") OpTest,
      scope ".tests suffix → Test" $
        expectEqual (classifyTermNameByConvention "Suite.tests") OpTest,
      scope "doc-shaped substring is not enough" $
        expectEqual (classifyTermNameByConvention "documenter") OpTerm,
      scope "tests-shaped substring is not enough" $
        expectEqual (classifyTermNameByConvention "testsuite") OpTerm
    ]

kindTextCases :: Test ()
kindTextCases =
  tests
    [ expectEqual (kindText OpTerm) "term",
      expectEqual (kindText OpType) "type",
      expectEqual (kindText OpCtor) "ctor",
      expectEqual (kindText OpDoc) "doc",
      expectEqual (kindText OpTest) "test",
      expectEqual (kindText OpAbility) "ability"
    ]
