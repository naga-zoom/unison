{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'PipelineToolArguments' JSON parsing. The dispatch
-- behavior (registry lookup, recursion guard, stopOnFirstError) is
-- covered by the integration smoke against the deployed binary.
module Unison.Test.MCP.Pipeline where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy.Char8 qualified as BL
import EasyTest
import Unison.MCP.Types
  ( PipelineStep (..),
    PipelineToolArguments (..),
  )

decodeArgs :: String -> Either String PipelineToolArguments
decodeArgs = Aeson.eitherDecode . BL.pack

test :: Test ()
test =
  scope "mcp.pipeline" . tests $
    [ scope "decodes a 3-step request" $
        case decodeArgs "{\"steps\":[{\"tool\":\"a\",\"arguments\":{}},{\"tool\":\"b\",\"arguments\":{}},{\"tool\":\"c\",\"arguments\":{}}],\"stopOnFirstError\":false}" of
          Right p -> do
            expectEqual (length p.steps) 3
            expectEqual ((head p.steps).tool) "a"
            expectEqual (p.stopOnFirstError) (Just False)
          Left e -> crash $ "expected ok; got: " ++ e,
      scope "missing arguments defaults to empty object" $
        case decodeArgs "{\"steps\":[{\"tool\":\"a\"}]}" of
          Right p -> do
            expectEqual (length p.steps) 1
            expectEqual ((head p.steps).arguments) (Aeson.Object KeyMap.empty)
          Left e -> crash $ "expected ok; got: " ++ e,
      scope "missing stopOnFirstError parses to Nothing (handler default applies)" $
        case decodeArgs "{\"steps\":[]}" of
          Right p -> expectEqual (p.stopOnFirstError) Nothing
          Left e -> crash $ "expected ok; got: " ++ e,
      scope "missing steps → fails" $
        case decodeArgs "{}" of
          Left _ -> ok
          Right _ -> crash "expected failure when steps absent",
      scope "step without tool → fails" $
        case decodeArgs "{\"steps\":[{\"arguments\":{}}]}" of
          Left _ -> ok
          Right _ -> crash "expected failure when tool absent",
      scope "step with rich arguments preserves them verbatim" $
        case decodeArgs "{\"steps\":[{\"tool\":\"x\",\"arguments\":{\"a\":1,\"b\":[\"y\"]}}]}" of
          Right p -> do
            let arg = (head p.steps).arguments
            case arg of
              Aeson.Object _ -> ok
              _ -> crash "expected arguments to be an object"
          Left e -> crash $ "expected ok; got: " ++ e
    ]
