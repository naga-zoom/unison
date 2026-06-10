{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hash → name resolution as a typed primitive.
--
-- Composes existing UCM primitives that today require three separate calls:
--
--   1. Parse user input into a 'ShortHash' ('ShortHash.fromText')
--   2. Resolve to a 'Set LabeledDependency' ('Codebase.resolveShortHash')
--   3. Look up names per dependency
--      ('Names.namesForReferent' / 'Names.namesForReference')
--
-- The 'ResolvedHash' return value collects all three steps' output into a
-- single typed structure. Each constructor encodes a proven state; pattern
-- matching downstream is total.
module Unison.MCP.Domain.Resolution
  ( ResolvedHash (..),
    Resolution (..),
    Status (..),
    resolveHash,
    bestGuess,
    statusOf,
  )
where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch.Names qualified as BranchNames
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name

-- | Result of resolving a 'ShortHash' against a codebase branch.
data ResolvedHash = ResolvedHash
  { hashInput :: ShortHash,
    resolutions :: [Resolution]
  }
  deriving (Eq, Show)

-- | One disambiguation of the input hash — a single 'LabeledDependency'
-- (either a term referent or a type reference) with whatever names are
-- currently bound to it in the resolution branch.
data Resolution = Resolution
  { dep :: LabeledDependency,
    names :: Set Name
  }
  deriving (Eq, Show)

-- | Discrete status — derived from the shape of 'resolutions'. Useful for
-- structuring agent responses.
data Status
  = NoMatch
  | Unbound -- one reference, zero names
  | Bound -- one reference, at least one name
  | Ambiguous -- short hash matched more than one reference
  deriving (Eq, Show)

-- | Compose the three steps. Total: every 'ShortHash' input produces a
-- valid 'ResolvedHash' (with @resolutions = []@ being the no-match case).
resolveHash ::
  Codebase IO Symbol Ann ->
  Branch0 IO ->
  ShortHash ->
  IO ResolvedHash
resolveHash codebase branch sh = do
  let nameTable = BranchNames.toNames branch
  deps <- Codebase.runTransaction codebase $ Codebase.resolveShortHash codebase sh
  let resolutions =
        Set.toList deps & map \d ->
          let ns = lookupNames nameTable d
           in Resolution {dep = d, names = ns}
  pure $ ResolvedHash {hashInput = sh, resolutions = resolutions}

-- | Per-dependency name lookup, dispatching on the labeled-dependency shape.
lookupNames :: Names.Names -> LabeledDependency -> Set Name
lookupNames nameTable = \case
  LD.TermReferent ref -> Names.namesForReferent nameTable ref
  LD.TypeReference ref -> Names.namesForReference nameTable ref

-- | Classify the resolution shape.
statusOf :: ResolvedHash -> Status
statusOf r = case r.resolutions of
  [] -> NoMatch
  [one]
    | Set.null one.names -> Unbound
    | otherwise -> Bound
  _ -> Ambiguous

-- | Pick the single most-likely current name. Strategy: union all
-- resolutions' names, prefer the shortest by character count, break ties
-- alphabetically. Returns 'Nothing' if no resolution carries any name.
bestGuess :: ResolvedHash -> Maybe Name
bestGuess r =
  let allNames = concatMap (Set.toList . (.names)) r.resolutions
   in case sortOn nameOrderingKey allNames of
        [] -> Nothing
        (n : _) -> Just n

nameOrderingKey :: Name -> (Int, Text)
nameOrderingKey n =
  let t = Name.toText n
   in (Text.length t, t)
