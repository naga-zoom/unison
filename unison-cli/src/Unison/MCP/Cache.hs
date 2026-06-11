{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Merkle-keyed cache for expensive branch walks.
--
-- The key insight: UCM branches are content-addressed via causal hashes.
-- Mutating any definition produces a new causal hash, so an entry keyed
-- by @(causalHash, resourceKind)@ /becomes unreachable/ when the branch
-- changes — no explicit invalidation is needed. Old entries simply age
-- out: they're inserted on first compute, looked up by exact hash on
-- subsequent calls, and abandoned (eventually GC'd) once nothing
-- references them.
--
-- This is content-addressed structural sharing applied to caching:
-- two branches that share a sub-namespace literally share cache entries
-- because they share the same hash for that path.
--
-- Properties:
--
-- * /Strong consistency./ A cache hit is always correct — the hash IS
--   the content-address.
-- * /Zero invalidation logic./ No "on-write" hooks, no TTL, no etag
--   bookkeeping beyond the hash.
-- * /External-write safe./ A different process mutating the codebase
--   produces a new causal hash on next read; the cache lookup misses
--   and the fresh value is computed.
--
-- /Resource kinds:/ stable Text identifiers like @"deepTerms"@,
-- @"deepTypes"@, @"structural-issues"@, @"detect-stale"@. Each tool
-- module defines its own kinds.
module Unison.MCP.Cache
  ( -- * Cache primitives
    BranchCacheKey,
    branchCacheKey,
    getOrCompute,
    getOrComputeEMCP,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import U.Codebase.HashTags (CausalHash (..))
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Hash32 qualified as Hash32
import Unison.MCP.Types
import Unison.Prelude
import UnliftIO.STM (atomically, modifyTVar', readTVarIO)

-- | A cache key is @(branchHashText, resourceKindText)@. Both 'Text';
-- the first is a base32-encoded causal hash, the second a stable
-- identifier the calling tool picks.
type BranchCacheKey = (Text, Text)

-- | Compute a cache key for a 'Branch' + named resource kind.
branchCacheKey :: Branch m -> Text -> BranchCacheKey
branchCacheKey b kind =
  let CausalHash h = Branch.headHash b
   in (Hash32.toText (Hash32.fromHash h), kind)

-- | Look up @(branchHash, kind)@ in the cache. On miss, run the
-- compute action, store the result, and return it.
getOrCompute :: BranchCacheKey -> MCP Aeson.Value -> MCP Aeson.Value
getOrCompute key compute = do
  cacheTVar <- asks (.branchCache)
  m <- readTVarIO cacheTVar
  case Map.lookup key m of
    Just v -> pure v
    Nothing -> do
      v <- compute
      atomically $ modifyTVar' cacheTVar (Map.insert key v)
      pure v

-- | Same as 'getOrCompute' but the compute action runs in
-- @'ExceptT' 'Text' 'MCP'@ — convenient because most tool handlers
-- live in that monad and 'handleInputMCP' returns it.
getOrComputeEMCP ::
  BranchCacheKey ->
  ExceptT Text MCP Aeson.Value ->
  ExceptT Text MCP Aeson.Value
getOrComputeEMCP key compute = do
  cacheTVar <- lift (asks (.branchCache))
  m <- readTVarIO cacheTVar
  case Map.lookup key m of
    Just v -> pure v
    Nothing -> do
      v <- compute
      atomically $ modifyTVar' cacheTVar (Map.insert key v)
      pure v
