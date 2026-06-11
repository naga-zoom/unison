{-# LANGUAGE OverloadedStrings #-}

-- | Lightweight logging wrapper for the ucm-mcp daemon.
--
-- Uses @co-log-core@'s 'Severity' enum and 'WithSeverity' wrapper, the
-- same logging substrate UCM's LSP server already runs on
-- ('Unison.LSP.Types.logInfo' etc.). One framework across the codebase
-- — no bespoke reinvention.
--
-- Severity levels (verbosity ascending):
--
-- @
-- Error    — failures only
-- Warning  — unusual but recoverable
-- Info     — normal operation             [default]
-- Debug    — implementation-level visibility
-- @
--
-- (Colog doesn't ship a separate "Trace" level — emit verbose
-- per-event traces at 'Debug' and gate them behind a separate flag if
-- the noise becomes a problem.)
--
-- Output goes to /stderr/ — /stdout/ is reserved for JSON-RPC frames.
--
-- Level is set at daemon start via the @UCM_MCP_LOG_LEVEL@ environment
-- variable. Defaults to 'Info'.
module Unison.MCP.Log
  ( Severity (..),
    parseSeverity,
    logAt,
  )
where

import Colog.Core (Severity (..))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (stderr)
import Unison.Prelude

parseSeverity :: Text -> Maybe Severity
parseSeverity raw =
  case Text.toLower (Text.strip raw) of
    "error" -> Just Error
    "warn" -> Just Warning
    "warning" -> Just Warning
    "info" -> Just Info
    "debug" -> Just Debug
    -- "trace" is an alias for Debug — Colog doesn't have a separate
    -- level. Users who want the most verbose output should set Debug
    -- explicitly.
    "trace" -> Just Debug
    _ -> Nothing

renderSeverity :: Severity -> Text
renderSeverity = \case
  Error -> "ERROR"
  Warning -> "WARN "
  Info -> "INFO "
  Debug -> "DEBUG"

-- | Emit a log line if the message is at-or-above the threshold severity.
--
-- Colog's 'Severity' derives @Ord@ as @Debug < Info < Warning < Error@.
-- So threshold=Debug shows everything (Debug + Info + Warning + Error);
-- threshold=Error shows only Error. The filter is @level >= threshold@.
logAt :: (MonadIO m) => Severity -> Severity -> Text -> m ()
logAt threshold level msg
  | level >= threshold = liftIO $ do
      now <- getCurrentTime
      let ts = formatTime defaultTimeLocale "%H:%M:%S%3Q" now
      Text.hPutStrLn stderr $
        "[" <> Text.pack ts <> "] [" <> renderSeverity level <> "] " <> msg
  | otherwise = pure ()
