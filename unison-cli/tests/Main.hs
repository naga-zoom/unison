module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)
import Unison.Test.ClearCache qualified as ClearCache
import Unison.Test.Cli.Monad qualified as Cli.Monad
import Unison.Test.LSP qualified as LSP
import Unison.Test.MCP.CompleteUpdate qualified as MCP.CompleteUpdate
import Unison.Test.MCP.DetectStale qualified as MCP.DetectStale
import Unison.Test.MCP.Find qualified as MCP.Find
import Unison.Test.MCP.Pattern qualified as MCP.Pattern
import Unison.Test.MCP.Resolution qualified as MCP.Resolution
import Unison.Test.UriParser qualified as UriParser

test :: Test ()
test =
  tests
    [ LSP.test,
      ClearCache.test,
      Cli.Monad.test,
      MCP.CompleteUpdate.test,
      MCP.DetectStale.test,
      MCP.Find.test,
      MCP.Pattern.test,
      MCP.Resolution.test,
      UriParser.test
    ]

main :: IO ()
main = withCP65001 do
  args <- getArgs
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  case args of
    [] -> runOnly "" test
    [prefix] -> runOnly prefix test
    [seed, prefix] -> rerunOnly (read seed) prefix test
    _ -> error "expected no args, a prefix, or a seed and a prefix"
