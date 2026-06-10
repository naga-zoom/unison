{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure helpers in 'Unison.MCP.Domain.Resolution'.
--
-- The 'resolveHash' composer itself touches IO + the codebase and is
-- exercised end-to-end via integration smoke. These unit tests cover the
-- pure 'statusOf' classifier and the 'bestGuess' ranking on synthetic
-- 'ResolvedHash' values built directly (no codebase needed).
module Unison.Test.MCP.Resolution where

import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import EasyTest
import Unison.LabeledDependency qualified as LD
import Unison.MCP.Domain.Resolution
  ( Resolution (..),
    ResolvedHash (..),
    Status (..),
    bestGuess,
    statusOf,
  )
import Unison.Name (Name)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.ShortHash qualified as ShortHash
import Unison.Syntax.Name qualified as Name

test :: Test ()
test =
  scope "mcp.resolution" . tests $
    [ scope "statusOf" statusCases,
      scope "bestGuess" bestGuessCases
    ]

-- ----------------------------------------------------------------------------
-- statusOf
-- ----------------------------------------------------------------------------

statusCases :: Test ()
statusCases =
  tests
    [ scope "empty resolutions → NoMatch" $
        expectEqual (statusOf (mkResolved [])) NoMatch,
      scope "single resolution, no names → Unbound" $
        expectEqual (statusOf (mkResolved [unbound])) Unbound,
      scope "single resolution, names present → Bound" $
        expectEqual (statusOf (mkResolved [boundOne])) Bound,
      scope "multiple resolutions → Ambiguous" $
        expectEqual (statusOf (mkResolved [unbound, boundOne])) Ambiguous,
      scope "multiple bound resolutions still Ambiguous" $
        expectEqual (statusOf (mkResolved [boundOne, boundTwo])) Ambiguous
    ]

-- ----------------------------------------------------------------------------
-- bestGuess
-- ----------------------------------------------------------------------------

bestGuessCases :: Test ()
bestGuessCases =
  tests
    [ scope "no resolutions → Nothing" $
        expect $ isNothing (bestGuess (mkResolved [])),
      scope "single bound → that name" $
        expectEqual
          (fmap nameTextOf (bestGuess (mkResolved [boundOne])))
          (Just "List.map"),
      scope "unbound singleton → Nothing" $
        expect $ isNothing (bestGuess (mkResolved [unbound])),
      scope "ambiguous: shortest wins" $
        expectEqual
          (fmap nameTextOf (bestGuess (mkResolved [boundOne, boundTwo])))
          (Just "List.map"),
      scope "ambiguous: ties broken alphabetically" $
        expectEqual
          (fmap nameTextOf (bestGuess (mkResolved [boundOne, boundAlt])))
          (Just "List.map")
    ]

-- ----------------------------------------------------------------------------
-- Fixtures
-- ----------------------------------------------------------------------------

mkResolved :: [Resolution] -> ResolvedHash
mkResolved rs =
  ResolvedHash
    { hashInput =
        let sh = ShortHash.fromText "#abc"
         in case sh of
              Just s -> s
              Nothing -> error "fixture short hash failed to parse",
      resolutions = rs
    }

-- A term referent built from a builtin reference (avoids hash construction).
synthRef :: LD.LabeledDependency
synthRef = LD.TermReferent (Referent.Ref (Reference.Builtin "fixtureTerm"))

synthRef2 :: LD.LabeledDependency
synthRef2 = LD.TermReferent (Referent.Ref (Reference.Builtin "fixtureTerm2"))

unbound :: Resolution
unbound = Resolution {dep = synthRef, names = Set.empty}

boundOne :: Resolution
boundOne =
  Resolution
    { dep = synthRef,
      names = Set.fromList [unsafeName "List.map"]
    }

boundTwo :: Resolution
boundTwo =
  Resolution
    { dep = synthRef2,
      names = Set.fromList [unsafeName "data.collection.List.map"]
    }

-- Same length as List.map (8) but alphabetically later. Used to verify the
-- alphabetic tiebreak.
boundAlt :: Resolution
boundAlt =
  Resolution
    { dep = synthRef2,
      names = Set.fromList [unsafeName "Map.from"]
    }

unsafeName :: Text -> Name
unsafeName t = case Name.parseTextEither t of
  Right n -> n
  Left e -> error $ "fixture name failed to parse: " <> show e

nameTextOf :: Name -> Text
nameTextOf = Name.toText
