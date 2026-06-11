{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Runtime statistics for the ucm-mcp daemon.
--
-- Lightweight counters tracked across tool calls + cache lookups. Cost
-- per call: ~1µs (a TVar increment + a single 'getCurrentTime'). The
-- aggregate is queryable via the @stats@ MCP tool — handy for tuning
-- the merkle cache, identifying slow tools, and verifying that an
-- agent's session is actually hitting cache.
--
-- The Stats record lives in 'Env' so every tool dispatch and every
-- 'Cache.getOrCompute' call can record its activity through the
-- 'recordToolCall' / 'recordCacheHit' / 'recordCacheMiss' helpers.
module Unison.MCP.Stats
  ( Stats (..),
    ToolStats (..),
    newStats,
    recordToolCall,
    recordCacheHit,
    recordCacheMiss,
    snapshotStats,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Unison.Prelude
import UnliftIO.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)

data Stats = Stats
  { startTime :: UTCTime,
    toolCalls :: TVar (Map Text ToolStats),
    cacheHits :: TVar Int,
    cacheMisses :: TVar Int
  }

data ToolStats = ToolStats
  { callCount :: Int,
    errorCount :: Int,
    -- | Sum of call durations in nanoseconds. Combined with
    -- 'callCount' gives mean latency.
    totalNanos :: Integer,
    minNanos :: Integer,
    maxNanos :: Integer
  }
  deriving (Eq, Show)

emptyToolStats :: ToolStats
emptyToolStats =
  ToolStats
    { callCount = 0,
      errorCount = 0,
      totalNanos = 0,
      minNanos = 0,
      maxNanos = 0
    }

newStats :: IO Stats
newStats = do
  startTime <- getCurrentTime
  toolCalls <- newTVarIO Map.empty
  cacheHits <- newTVarIO 0
  cacheMisses <- newTVarIO 0
  pure Stats {startTime, toolCalls, cacheHits, cacheMisses}

-- | Update the per-tool counters. @isError@ true → bumps errorCount.
recordToolCall :: Stats -> Text -> Bool -> Integer -> IO ()
recordToolCall stats toolName isError elapsedNanos =
  atomically $ modifyTVar' stats.toolCalls (Map.alter bump toolName)
  where
    bump =
      Just
        . ( \prev ->
              let current = fromMaybe emptyToolStats prev
                  n = current.callCount + 1
               in current
                    { callCount = n,
                      errorCount = current.errorCount + (if isError then 1 else 0),
                      totalNanos = current.totalNanos + elapsedNanos,
                      minNanos =
                        if current.callCount == 0
                          then elapsedNanos
                          else min current.minNanos elapsedNanos,
                      maxNanos = max current.maxNanos elapsedNanos
                    }
          )

recordCacheHit :: Stats -> IO ()
recordCacheHit s = atomically $ modifyTVar' s.cacheHits (+ 1)

recordCacheMiss :: Stats -> IO ()
recordCacheMiss s = atomically $ modifyTVar' s.cacheMisses (+ 1)

-- | Read-only snapshot of the counters, as JSON. Includes derived
-- fields: uptime (seconds), cache hit ratio, mean latency per tool.
snapshotStats :: Stats -> IO Aeson.Value
snapshotStats s = do
  now <- getCurrentTime
  toolMap <- readTVarIO s.toolCalls
  hits <- readTVarIO s.cacheHits
  misses <- readTVarIO s.cacheMisses
  let uptimeSec = floor (now `diffUTCTime` s.startTime) :: Int
  let totalCacheLookups = hits + misses
  let hitRatio =
        if totalCacheLookups == 0
          then 0 :: Double
          else fromIntegral hits / fromIntegral totalCacheLookups
  pure $
    Aeson.object
      [ "uptimeSec" Aeson..= uptimeSec,
        "cache"
          Aeson..= Aeson.object
            [ "hits" Aeson..= hits,
              "misses" Aeson..= misses,
              "totalLookups" Aeson..= totalCacheLookups,
              "hitRatio" Aeson..= hitRatio
            ],
        "tools" Aeson..= Aeson.object [(AesonKey.fromText n, toolJSON ts) | (n, ts) <- Map.toList toolMap]
      ]
  where
    toolJSON ts =
      let meanNanos = if ts.callCount == 0 then 0 else ts.totalNanos `div` fromIntegral ts.callCount
       in Aeson.object
            [ "calls" Aeson..= ts.callCount,
              "errors" Aeson..= ts.errorCount,
              "minMs" Aeson..= nanosToMs ts.minNanos,
              "maxMs" Aeson..= nanosToMs ts.maxNanos,
              "meanMs" Aeson..= nanosToMs meanNanos
            ]
    nanosToMs :: Integer -> Double
    nanosToMs n = fromIntegral n / 1_000_000.0
