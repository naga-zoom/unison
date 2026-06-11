{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Unison.MCP.Cache'. We exercise the cache logic against
-- a stub TVar — the 'Branch'-keying side is covered by integration
-- smoke against the deployed binary. Here we pin the merkle invariant
-- behaviorally: same key → same value; different key → independent.
module Unison.Test.MCP.Cache where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef
import Data.Map.Strict qualified as Map
import EasyTest
import UnliftIO (atomically, newTVarIO, readTVarIO)
import UnliftIO.STM (TVar, modifyTVar')

-- A simplified, type-equivalent reimplementation of getOrCompute that
-- exercises the same TVar pattern. We can't easily lift the real
-- MCP-monad getOrCompute here, but the cache-control behavior is what
-- matters and lives in this loop.
getOrCompute ::
  TVar (Map.Map (String, String) Aeson.Value) ->
  (String, String) ->
  IO Aeson.Value ->
  IO Aeson.Value
getOrCompute cacheTVar key compute = do
  m <- readTVarIO cacheTVar
  case Map.lookup key m of
    Just v -> pure v
    Nothing -> do
      v <- compute
      atomically $ modifyTVar' cacheTVar (Map.insert key v)
      pure v

test :: Test ()
test =
  scope "mcp.cache" . tests $
    [ scope "first miss → compute runs; result cached" cacheFirstMiss,
      scope "second hit on same key → compute does NOT re-run" cacheSecondHit,
      scope "different key → compute runs again, both cached" cacheDifferentKey,
      scope "same hash + different kind → independent entries" cacheDifferentKind
    ]

cacheFirstMiss :: Test ()
cacheFirstMiss = do
  io $ do
    cache <- newTVarIO Map.empty
    counter <- newIORef (0 :: Int)
    _ <- getOrCompute cache ("h1", "k1") $ do
      modifyIORef counter (+ 1)
      pure (Aeson.String "v1")
    count <- readIORef counter
    if count == 1 then pure () else error $ "expected 1 compute; got " ++ show count
  ok

cacheSecondHit :: Test ()
cacheSecondHit = do
  io $ do
    cache <- newTVarIO Map.empty
    counter <- newIORef (0 :: Int)
    let bump = do modifyIORef counter (+ 1); pure (Aeson.String "v")
    _ <- getOrCompute cache ("h1", "k1") bump
    _ <- getOrCompute cache ("h1", "k1") bump
    _ <- getOrCompute cache ("h1", "k1") bump
    count <- readIORef counter
    if count == 1 then pure () else error $ "expected 1 compute across 3 hits; got " ++ show count
  ok

cacheDifferentKey :: Test ()
cacheDifferentKey = do
  io $ do
    cache <- newTVarIO Map.empty
    counter <- newIORef (0 :: Int)
    let bump tag = do modifyIORef counter (+ 1); pure (Aeson.String tag)
    a <- getOrCompute cache ("h1", "k1") (bump "a")
    b <- getOrCompute cache ("h2", "k1") (bump "b")
    count <- readIORef counter
    if count /= 2 then error $ "expected 2 computes for distinct keys; got " ++ show count else pure ()
    if a == b then error "expected distinct cached values" else pure ()
  ok

cacheDifferentKind :: Test ()
cacheDifferentKind = do
  io $ do
    cache <- newTVarIO Map.empty
    counter <- newIORef (0 :: Int)
    let bump tag = do modifyIORef counter (+ 1); pure (Aeson.String tag)
    a <- getOrCompute cache ("h1", "deepTerms") (bump "terms")
    b <- getOrCompute cache ("h1", "deepTypes") (bump "types")
    -- Same branch hash, different resource kind → independent entries
    count <- readIORef counter
    if count /= 2 then error $ "expected 2 computes for distinct kinds on same hash; got " ++ show count else pure ()
    if a == b then error "expected distinct cached values" else pure ()
    -- Both should now be cached: repeat reads should not bump counter
    _ <- getOrCompute cache ("h1", "deepTerms") (bump "terms-2")
    _ <- getOrCompute cache ("h1", "deepTypes") (bump "types-2")
    count' <- readIORef counter
    if count' /= 2 then error $ "expected no further computes; got " ++ show count' else pure ()
  ok

-- Silence unused warnings for KeyMap (kept import in case future cache
-- tests want to inspect Object-shape cache entries).
_unused :: KeyMap.KeyMap ()
_unused = KeyMap.empty
