{-# LANGUAGE OverloadedStrings #-}

-- | Wire-format helpers for MCP responses. Currently:
--
-- * 'truncateHash' — shortens a Unison short-hash to its 12-char prefix
--   for the JSON wire format, preserving any @#N@ constructor suffix.
--   12 base32 chars carry ~60 bits of entropy — collision-free across
--   ~10⁹ definitions per project, well above real-codebase scale.
--   UCM's TUI already displays prefixes; our wire format matches that.
--
-- * 'shortHashText' — convenience wrapper combining 'ShortHash.toText'
--   with 'truncateHash'. Use this in place of 'ShortHash.toText' for
--   any JSON output.
module Unison.MCP.Wire
  ( truncateHash,
    shortHashText,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash

-- | Number of base32 chars to keep after the leading @#@. 12 ≈ 60 bits
-- of entropy. Bump if you ever see a collision in practice.
hashPrefixLen :: Int
hashPrefixLen = 12

-- | Truncate a @#abc…xyz@ or @#abc…xyz#N@ hash to its prefix form.
-- Non-hash strings pass through unchanged.
truncateHash :: Text -> Text
truncateHash t =
  case Text.uncons t of
    Just ('#', rest) ->
      let (body, suffix) = Text.breakOn "#" rest
       in if Text.length body > hashPrefixLen
            then "#" <> Text.take hashPrefixLen body <> suffix
            else t
    _ -> t

shortHashText :: ShortHash -> Text
shortHashText = truncateHash . ShortHash.toText
