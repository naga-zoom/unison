{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @detect-stale@ MCP tool: list definitions whose source body still
-- references hashes that have no current names in the branch.
--
-- A reference is \"stale\" when it points to a content-addressed
-- 'Reference.DerivedId' that the current branch's 'Names' table cannot
-- bind to any name. This happens after a library upgrade, a delete, or a
-- rename that left dependent code anchored to the old hash. Built-in
-- references are intrinsically named and never reported as stale.
--
-- Returns structured JSON
-- @{staleDefs: [{name, kind, hash, unnamedDeps: [{kind, hash}]}],
--   totalCount}@.
module Unison.MCP.Tools.DetectStale
  ( detectStaleTool,
    isStaleRef,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.MCP.Cache qualified as Cache
import Unison.Codebase.Branch.Names qualified as BranchNames
import Unison.DataDeclaration qualified as DD
import Unison.MCP.Cli (cliToMCP)
import Unison.MCP.Types
import Unison.MCP.Wire qualified as Wire
import Unison.MCP.Wrapper
import Unison.Name (Name)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (Reference, TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Term qualified as Term
import Unison.Util.Defns (DefnsF, Defns (..))
import Unison.Util.Relation qualified as R
import UnliftIO qualified

-- ----------------------------------------------------------------------------
-- Wire format
-- ----------------------------------------------------------------------------

data DetectStaleResponse = DetectStaleResponse
  { staleDefs :: [StaleDef],
    totalCount :: Int
  }

instance Aeson.ToJSON DetectStaleResponse where
  toJSON r =
    Aeson.object
      [ "staleDefs" Aeson..= r.staleDefs,
        "totalCount" Aeson..= r.totalCount
      ]

data StaleDef = StaleDef
  { name :: Text,
    kind :: Text,
    hash :: Text,
    unnamedDeps :: [UnnamedDep]
  }

instance Aeson.ToJSON StaleDef where
  toJSON s =
    Aeson.object
      [ "name" Aeson..= s.name,
        "kind" Aeson..= s.kind,
        "hash" Aeson..= s.hash,
        "unnamedDeps" Aeson..= s.unnamedDeps
      ]

data UnnamedDep = UnnamedDep
  { kind :: Text,
    hash :: Text
  }

instance Aeson.ToJSON UnnamedDep where
  toJSON u =
    Aeson.object
      [ "kind" Aeson..= u.kind,
        "hash" Aeson..= u.hash
      ]

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

detectStaleTool :: Tool MCP
detectStaleTool =
  Tool
    { toolName = toToolName DetectStaleTool,
      toolDescription =
        "List definitions in the current branch whose source body references \
        \hashes that have no current names. Identifies code anchored to old \
        \library versions, deleted definitions, or renamed-away targets. \
        \Returns structured JSON \
        \{staleDefs: [{name, kind, hash, unnamedDeps}], totalCount}.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Detect Stale",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @DetectStaleToolArguments,
      toolHandler = \(DetectStaleToolArguments {projectContext}) -> handleToolError $ do
        codebase <- asks (.codebase)
        let noop _ = pure ()
        (mFullBranch, _output) <- cliToMCP projectContext noop Cli.getCurrentBranch
        case mFullBranch of
          Nothing -> throwError "No current branch"
          Just fullBranch -> do
            let b = Branch.head fullBranch
            let cacheKey = Cache.branchCacheKey fullBranch "detect-stale"
            Cache.getOrComputeEMCP cacheKey
              ( do
                  stale <- UnliftIO.liftIO $ enumerateStale codebase b
                  let response = DetectStaleResponse {staleDefs = stale, totalCount = length stale}
                  pure (Aeson.toJSON response)
              )
              >>= \payload ->
                pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode payload
    }

-- ----------------------------------------------------------------------------
-- Enumeration
-- ----------------------------------------------------------------------------

enumerateStale :: Codebase.Codebase IO Symbol Ann -> Branch0 IO -> IO [StaleDef]
enumerateStale codebase branch = do
  let nameTable = BranchNames.toNames branch
  termStale <- traverse (analyzeTerm codebase nameTable) (R.toList (Branch.deepTerms branch))
  typeStale <- traverse (analyzeType codebase nameTable) (R.toList (Branch.deepTypes branch))
  pure $ catMaybes (termStale <> typeStale)

-- | If the given term is content-addressed and has any unnamed dependencies,
-- emit a 'StaleDef'. Otherwise 'Nothing'.
analyzeTerm ::
  Codebase.Codebase IO Symbol Ann ->
  Names.Names ->
  (Referent.Referent, Name) ->
  IO (Maybe StaleDef)
analyzeTerm codebase nameTable (refnt, n) = case refnt of
  Referent.Con {} -> pure Nothing -- constructor refs come from the type's body; the type itself is examined.
  Referent.Ref termRef -> case termRef of
    Reference.Builtin _ -> pure Nothing -- builtins are intrinsic; never stale.
    Reference.DerivedId rid -> do
      term <- Codebase.runTransaction codebase $ Codebase.unsafeGetTerm codebase rid
      let deps = Term.dependencies term
      let unnamed = unnamedFromDefns nameTable deps
      pure $
        if null unnamed
          then Nothing
          else
            Just $
              StaleDef
                { name = Name.toText n,
                  kind = "term",
                  hash = Wire.shortHashText (Referent.toShortHash refnt),
                  unnamedDeps = unnamed
                }

analyzeType ::
  Codebase.Codebase IO Symbol Ann ->
  Names.Names ->
  (TypeReference, Name) ->
  IO (Maybe StaleDef)
analyzeType codebase nameTable (typeRef, n) = case typeRef of
  Reference.Builtin _ -> pure Nothing
  Reference.DerivedId rid -> do
    decl <- Codebase.runTransaction codebase $ Codebase.unsafeGetTypeDeclaration codebase rid
    let deps = DD.declTypeDependencies decl
    -- declTypeDependencies returns Set TypeReference. Wrap in DefnsF for unified handling.
    let depsDefns :: DefnsF Set TermReference TypeReference
        depsDefns = Defns {terms = Set.empty, types = deps}
    let unnamed = unnamedFromDefns nameTable depsDefns
    pure $
      if null unnamed
        then Nothing
        else
          Just $
            StaleDef
              { name = Name.toText n,
                kind = "type",
                hash = Wire.shortHashText (Reference.toShortHash typeRef),
                unnamedDeps = unnamed
              }

-- ----------------------------------------------------------------------------
-- Staleness predicate (pure, unit-testable)
-- ----------------------------------------------------------------------------

unnamedFromDefns :: Names.Names -> DefnsF Set TermReference TypeReference -> [UnnamedDep]
unnamedFromDefns nameTable Defns {terms, types} =
  let staleTerms =
        [ UnnamedDep {kind = "term", hash = Wire.shortHashText (Reference.toShortHash r)}
        | r <- Set.toList terms,
          isStaleRef r,
          Set.null (Names.namesForReferent nameTable (Referent.Ref r))
        ]
      staleTypes =
        [ UnnamedDep {kind = "type", hash = Wire.shortHashText (Reference.toShortHash r)}
        | r <- Set.toList types,
          isStaleRef r,
          Set.null (Names.namesForReference nameTable r)
        ]
   in staleTerms <> staleTypes

-- | Pure: a 'Reference' is eligible to be flagged stale iff it's
-- content-addressed. Builtin references are intrinsic and never stale.
-- Surfaced for unit testing.
isStaleRef :: Reference -> Bool
isStaleRef = \case
  Reference.Builtin _ -> False
  Reference.DerivedId _ -> True
