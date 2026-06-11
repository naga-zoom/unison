{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @stats@ MCP tool: queryable snapshot of daemon counters.
--
-- Reports:
--
--   * @uptimeSec@                   — daemon uptime in seconds.
--   * @cache.{hits, misses, hitRatio, totalLookups}@ — merkle-cache stats.
--   * @tools.<name>.{calls, errors, minMs, maxMs, meanMs}@ — per-tool counts +
--     latency.
--
-- Counters are tracked in 'Env.stats' (incremented on every tool call
-- dispatch and on every 'Cache.getOrCompute' lookup). Cost per call: ~1µs
-- (one TVar increment + one wall-clock read).
module Unison.MCP.Tools.Stats
  ( statsTool,
  )
where

import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text
import Unison.MCP.Stats (snapshotStats)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import UnliftIO qualified

statsTool :: Tool MCP
statsTool =
  Tool
    { toolName = toToolName StatsTool,
      toolDescription =
        "Return a snapshot of daemon runtime counters: uptime, per-tool \
        \invocation counts + latency (min/mean/max ms), per-tool error \
        \counts, merkle-cache hits/misses/ratio. Useful for debugging \
        \cache effectiveness and identifying slow tools. The data is \
        \cumulative since daemon start; restart resets counters.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Daemon Stats",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @(),
      toolHandler = \() -> handleToolError $ do
        statsRef <- asks (.stats)
        snapshot <- UnliftIO.liftIO $ snapshotStats statsRef
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode snapshot
    }
