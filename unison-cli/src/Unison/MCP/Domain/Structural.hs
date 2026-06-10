{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure analyzer for /structural/ issues with type declarations:
-- orphan types (declared constructors that have no current name) and
-- misplaced constructors (named constructors whose namespace doesn't sit
-- under one of the type's namespaces).
--
-- The pure 'analyzeType' takes a pre-fetched 'TypeInfo' and returns a list
-- of 'StructuralIssue's. The IO-bound 'scanBranch' walks a branch's
-- 'deepTypes', fetches each declaration, and runs the analyzer.
--
-- Unblocks @diagnose@, @sanity-fix@, and the orphan-free check that
-- @release@ requires.
module Unison.MCP.Domain.Structural
  ( -- * Domain
    StructuralIssue (..),
    TypeInfo (..),

    -- * Pure analyzer
    analyzeType,

    -- * IO-bound scanner
    scanBranch,
  )
where

import Data.Set qualified as Set
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as BranchNames
import Unison.DataDeclaration qualified as DD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.Util.Relation qualified as R

-- ----------------------------------------------------------------------------
-- Domain
-- ----------------------------------------------------------------------------

-- | A structural problem with a single type declaration.
--
-- 'OrphanType' fires when a type's declared constructor count exceeds the
-- number of currently-named constructors — some constructors are
-- declared but unnamed.
--
-- 'MisplacedConstructor' fires when a named constructor's path is not
-- under any of the type's namespaces. Example: type @foo.Bar@ with a
-- constructor at @baz.X@ would emit one 'MisplacedConstructor' for that
-- constructor (the expected location is @foo.Bar.X@).
data StructuralIssue
  = OrphanType
      { typeRef :: TypeReference,
        typeNames :: Set Name,
        declaredCtors :: Int,
        namedCtors :: Int
      }
  | MisplacedConstructor
      { ctorName :: Name,
        ctorRef :: Referent,
        typeRef :: TypeReference,
        typeNames :: Set Name
      }
  deriving (Eq, Show)

-- | Information about a single type declaration needed by 'analyzeType'.
--
-- Built at the IO/codebase boundary and passed to the pure analyzer.
data TypeInfo = TypeInfo
  { typeRef :: TypeReference,
    typeNames :: Set Name,
    declaredCtors :: Int,
    constructorsByName :: [(Name, Referent)]
  }
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Pure analyzer
-- ----------------------------------------------------------------------------

-- | Pure: analyze one type declaration's structural soundness.
--
-- Total. Emits zero or more 'StructuralIssue's.
analyzeType :: TypeInfo -> [StructuralIssue]
analyzeType info =
  let orphanIssue =
        let namedCount = length info.constructorsByName
         in if info.declaredCtors > namedCount
              then
                [ OrphanType
                    { typeRef = info.typeRef,
                      typeNames = info.typeNames,
                      declaredCtors = info.declaredCtors,
                      namedCtors = namedCount
                    }
                ]
              else []
      misplacedIssues =
        [ MisplacedConstructor
            { ctorName = cn,
              ctorRef = cr,
              typeRef = info.typeRef,
              typeNames = info.typeNames
            }
        | (cn, cr) <- info.constructorsByName,
          not (any (`Name.isPrefixOf` cn) (Set.toList info.typeNames))
        ]
   in orphanIssue <> misplacedIssues

-- ----------------------------------------------------------------------------
-- IO-bound scanner — pre-fetches TypeInfo per declared type in the branch
-- ----------------------------------------------------------------------------

-- | Walk every type in the branch, build 'TypeInfo' for each, run the
-- analyzer, and concatenate the issues. Skips builtin types (which have
-- no decl body and no constructors to analyze in this sense).
scanBranch :: Codebase.Codebase IO Symbol Ann -> Branch0 m -> IO [StructuralIssue]
scanBranch codebase branch = do
  let nameTable = BranchNames.toNames branch
  let allTypes = R.toList (Branch.deepTypes branch)
  results <- traverse (analyzeOneType codebase nameTable) allTypes
  pure (concat results)

analyzeOneType ::
  Codebase.Codebase IO Symbol Ann ->
  Names.Names ->
  (TypeReference, Name) ->
  IO [StructuralIssue]
analyzeOneType codebase nameTable (typeRef, _someName) = case typeRef of
  Reference.Builtin _ -> pure []
  Reference.DerivedId rid -> do
    decl <- Codebase.runTransaction codebase $ Codebase.unsafeGetTypeDeclaration codebase rid
    let declaredCtors = DD.constructorCount (DD.asDataDecl decl)
    let typeNamesSet = Names.namesForReference nameTable typeRef
    let ctorsByName = Names.constructorsForType typeRef nameTable
    let info =
          TypeInfo
            { typeRef = typeRef,
              typeNames = typeNamesSet,
              declaredCtors = declaredCtors,
              constructorsByName = ctorsByName
            }
    pure (analyzeType info)
