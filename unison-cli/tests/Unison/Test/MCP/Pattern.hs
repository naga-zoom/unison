{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Unison.MCP.Domain.Pattern'.
--
-- Coverage:
--
-- * Parser surface — predicates, Boolean composition, grouping
-- * Algebraic laws: identity, annihilator, double-negation, idempotence
-- * Boolean evaluator correctness over a small operand zoo
-- * Glob matching edge cases
-- * Fingerprint stability under simplification
module Unison.Test.MCP.Pattern where

import Data.Text (Text)
import EasyTest
import Unison.MCP.Domain.Pattern
  ( NameMatcher (..),
    OpKind (..),
    Operand (..),
    Pattern (..),
    fingerprint,
    matchPattern,
    parsePattern,
    simplify,
  )

test :: Test ()
test =
  scope "mcp.pattern" . tests $
    [ scope "parser" parserParity,
      scope "evaluator" evaluatorCases,
      scope "glob" globCases,
      scope "algebraic-laws" algebraicLaws,
      scope "fingerprint" fingerprintCases,
      scope "parity-matrix" parityMatrix
    ]

-- ----------------------------------------------------------------------------
-- Parser surface
-- ----------------------------------------------------------------------------

parserParity :: Test ()
parserParity =
  tests
    [ scope "bare token → Contains" $
        parsesAs "Order" (NameP (Contains "Order")),
      scope "name:Order → Contains" $
        parsesAs "name:Order" (NameP (Contains "Order")),
      scope "kind:type" $
        parsesAs "kind:type" (Kind OpType),
      scope "kind:term" $
        parsesAs "kind:term" (Kind OpTerm),
      scope "kind:ability" $
        parsesAs "kind:ability" (Kind OpAbility),
      scope "kind:doc" $
        parsesAs "kind:doc" (Kind OpDoc),
      scope "kind:test" $
        parsesAs "kind:test" (Kind OpTest),
      scope "project:base" $
        parsesAs "project:base" (Project "base"),
      scope "owner:alice" $
        parsesAs "owner:alice" (Owner "alice"),
      scope "exact via =Order" $
        parsesAs "name:=Order" (NameP (ExactName "Order")),
      scope "glob foo.bar.*" $
        parsesAs "name:foo.bar.*" (NameP (Glob "foo.bar.*")),
      scope "AND composition" $
        parsesAs "kind:term AND project:base" (And (Kind OpTerm) (Project "base")),
      scope "OR composition" $
        parsesAs "kind:type OR kind:ability" (Or (Kind OpType) (Kind OpAbility)),
      scope "NOT" $
        parsesAs "NOT kind:doc" (Not (Kind OpDoc)),
      scope "parentheses override precedence" $
        parsesAs
          "kind:term AND (project:base OR project:stdlib)"
          ( And
              (Kind OpTerm)
              (Or (Project "base") (Project "stdlib"))
          ),
      scope "AND binds tighter than OR" $
        parsesAs
          "kind:term AND project:base OR kind:type"
          ( Or
              (And (Kind OpTerm) (Project "base"))
              (Kind OpType)
          ),
      scope "ANYTHING literal" $
        parsesAs "ANYTHING" Anything,
      scope "NOTHING literal" $
        parsesAs "NOTHING" Nothing',
      scope "reject empty input" $
        rejects "",
      scope "reject naked colon" $
        rejects "kind:",
      scope "reject AND as bare token" $
        rejects "AND"
    ]

parsesAs :: Text -> Pattern -> Test ()
parsesAs input expected = case parsePattern input of
  Right p ->
    if p == expected
      then ok
      else crash ("parsed: " ++ show p ++ " (expected " ++ show expected ++ ")")
  Left err -> crash ("parse failed: " ++ show err)

rejects :: Text -> Test ()
rejects input = case parsePattern input of
  Left _ -> ok
  Right p -> crash ("unexpectedly parsed: " ++ show input ++ " → " ++ show p)

-- ----------------------------------------------------------------------------
-- Evaluator cases
-- ----------------------------------------------------------------------------

evaluatorCases :: Test ()
evaluatorCases =
  tests
    [ scope "kind matches" $
        expect (matchPattern (Kind OpTerm) operandTerm),
      scope "kind mismatch" $
        expect (not (matchPattern (Kind OpType) operandTerm)),
      scope "name contains" $
        expect (matchPattern (NameP (Contains "List")) operandTerm),
      scope "project matches" $
        expect (matchPattern (Project "base") operandTerm),
      scope "owner missing field => false" $
        expect (not (matchPattern (Owner "alice") operandNoOwner)),
      scope "AND short-circuit (left false)" $
        expect (not (matchPattern (And Nothing' (Kind OpTerm)) operandTerm)),
      scope "OR short-circuit (left true)" $
        expect (matchPattern (Or Anything (Kind OpType)) operandTerm),
      scope "NOT inversion" $
        expect (matchPattern (Not (Kind OpType)) operandTerm)
    ]

operandTerm :: Operand
operandTerm =
  Operand
    { opName = "List.map",
      opKind = OpTerm,
      opProject = Just "base",
      opOwner = Just "alice"
    }

operandNoOwner :: Operand
operandNoOwner =
  operandTerm {opOwner = Nothing}

-- ----------------------------------------------------------------------------
-- Glob matching edge cases (hacker-mindset §10)
-- ----------------------------------------------------------------------------

globCases :: Test ()
globCases =
  tests
    [ scope "* matches empty string" $
        expect (matchPattern (NameP (Glob "*")) (operandTerm {opName = ""})),
      scope "* matches any" $
        expect (matchPattern (NameP (Glob "*")) operandTerm),
      scope "prefix glob" $
        expect (matchPattern (NameP (Glob "List.*")) operandTerm),
      scope "prefix glob non-match" $
        expect (not (matchPattern (NameP (Glob "Map.*")) operandTerm)),
      scope "suffix glob" $
        expect (matchPattern (NameP (Glob "*.map")) operandTerm),
      scope "middle glob" $
        expect (matchPattern (NameP (Glob "List.*.map")) (operandTerm {opName = "List.foo.map"})),
      scope "? matches single char" $
        expect (matchPattern (NameP (Glob "List.ma?")) operandTerm),
      scope "? requires a char (not empty)" $
        expect (not (matchPattern (NameP (Glob "List.ma?")) (operandTerm {opName = "List.ma"})))
    ]

-- ----------------------------------------------------------------------------
-- Algebraic laws — verified at example cases (full property-tests deferred)
-- ----------------------------------------------------------------------------

algebraicLaws :: Test ()
algebraicLaws =
  let p = Kind OpTerm
   in tests
        [ scope "identity: AND Anything == self" $
            simplifyEq (And p Anything) p,
          scope "identity: OR Nothing == self" $
            simplifyEq (Or p Nothing') p,
          scope "annihilator: AND Nothing == Nothing" $
            simplifyEq (And p Nothing') Nothing',
          scope "annihilator: OR Anything == Anything" $
            simplifyEq (Or p Anything) Anything,
          scope "double negation" $
            simplifyEq (Not (Not p)) p,
          scope "Not Anything == Nothing" $
            simplifyEq (Not Anything) Nothing',
          scope "Not Nothing == Anything" $
            simplifyEq (Not Nothing') Anything
        ]

simplifyEq :: Pattern -> Pattern -> Test ()
simplifyEq input expected =
  if simplify input == expected
    then ok
    else crash ("simplify " ++ show input ++ " = " ++ show (simplify input) ++ "; expected " ++ show expected)

-- ----------------------------------------------------------------------------
-- Fingerprint stability
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Parity matrix — anchors the documented surface to executable assertions.
-- Each row exercises an exact input string and asserts the parsed pattern's
-- evaluator behaves as documented on a probe operand. AST shapes are not
-- byte-checked (Prefix vs Glob represent equivalent matching semantics).
-- ----------------------------------------------------------------------------

parityMatrix :: Test ()
parityMatrix =
  tests
    [ scope "bare token (Foo) — contains-match" $
        behavesLike "Foo" (operandTerm {opName = "Foo.bar"}) True,
      scope "bare token misses non-containing name" $
        behavesLike "Foo" (operandTerm {opName = "Baz"}) False,
      scope "trailing-dot-star (a.b.*) — prefix-equivalent" $
        behavesLike "a.b.*" (operandTerm {opName = "a.b.c"}) True,
      scope "trailing-dot-star — no match outside prefix" $
        behavesLike "a.b.*" (operandTerm {opName = "x.a.b.c"}) False,
      scope "kind:type" $
        behavesLike "kind:type" (operandTerm {opKind = OpType}) True,
      scope "kind:term mismatch when op is type" $
        behavesLike "kind:term" (operandTerm {opKind = OpType}) False,
      scope "kind:nope is rejected at parse" $
        rejects "kind:nope",
      scope "project:base" $
        behavesLike "project:base" (operandTerm {opProject = Just "base"}) True,
      scope "kernel.* AND kind:type" $
        behavesLike
          "a.* AND kind:type"
          (operandTerm {opName = "a.b", opKind = OpType})
          True,
      scope "kernel.* AND kind:type — fails on kind mismatch" $
        behavesLike
          "a.* AND kind:type"
          (operandTerm {opName = "a.b", opKind = OpTerm})
          False,
      scope "alpha OR beta — left disjunct" $
        behavesLike "alpha OR beta" (operandTerm {opName = "alpha-thing"}) True,
      scope "alpha OR beta — right disjunct" $
        behavesLike "alpha OR beta" (operandTerm {opName = "x.beta"}) True,
      scope "alpha OR beta — neither" $
        behavesLike "alpha OR beta" (operandTerm {opName = "gamma"}) False,
      scope "NOT kind:term" $
        behavesLike "NOT kind:term" (operandTerm {opKind = OpType}) True,
      scope "a OR b AND NOT c — precedence (AND binds tighter)" $
        behavesLike
          "a OR b AND NOT c"
          (operandTerm {opName = "a.thing"})
          True, -- left disjunct matches
      scope "a OR b AND NOT c — right disjunct b ∧ ¬c" $
        behavesLike
          "a OR b AND NOT c"
          (operandTerm {opName = "b.thing"})
          True, -- has b, no c
      scope "a OR b AND NOT c — b ∧ c fails right disjunct" $
        behavesLike
          "a OR b AND NOT c"
          (operandTerm {opName = "b.x.c"})
          False, -- contains b AND c (right disjunct false); no a (left disjunct false)
      scope "(a OR b) AND c — both required" $
        behavesLike
          "(a OR b) AND c"
          (operandTerm {opName = "a.c"})
          True,
      scope "(a OR b) AND c — missing c" $
        behavesLike
          "(a OR b) AND c"
          (operandTerm {opName = "a.x"})
          False,
      scope "NOT NOT alpha — double negation" $
        behavesLike "NOT NOT alpha" (operandTerm {opName = "alpha.thing"}) True,
      scope "(alpha OR beta) AND kind:type AND NOT lib — combined" $
        behavesLike
          "(alpha OR beta) AND kind:type AND NOT lib"
          (operandTerm {opName = "alpha.thing", opKind = OpType})
          True,
      scope "combined query — lib present excludes via NOT" $
        behavesLike
          "(alpha OR beta) AND kind:type AND NOT lib"
          (operandTerm {opName = "lib.alpha", opKind = OpType})
          False
    ]

behavesLike :: Text -> Operand -> Bool -> Test ()
behavesLike input op expected = case parsePattern input of
  Right p ->
    let actual = matchPattern p op
     in if actual == expected
          then ok
          else
            crash $
              "behaviour mismatch for " ++ show input
                ++ "\n  AST: " ++ show p
                ++ "\n  on operand: " ++ show op
                ++ "\n  expected: " ++ show expected
                ++ "\n  actual:   " ++ show actual
  Left err -> crash ("parse failed: " ++ show err)

-- ----------------------------------------------------------------------------
-- Fingerprint stability
-- ----------------------------------------------------------------------------

fingerprintCases :: Test ()
fingerprintCases =
  let p = Kind OpTerm
   in tests
        [ scope "identical patterns → identical fingerprints" $
            expectEqual (fingerprint p) (fingerprint p),
          scope "fingerprint stable under simplification (AND Anything)" $
            expectEqual (fingerprint p) (fingerprint (And p Anything)),
          scope "fingerprint stable under simplification (double negation)" $
            expectEqual (fingerprint p) (fingerprint (Not (Not p))),
          scope "different patterns → different fingerprints" $
            expect (fingerprint (Kind OpTerm) /= fingerprint (Kind OpType))
        ]
