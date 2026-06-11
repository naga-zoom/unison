{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @probe@ MCP tool: hash → likely current name.
--
-- Wraps 'Unison.MCP.Domain.Resolution.resolveHash' as an MCP tool that
-- accepts a short hash (e.g., @#abc123@) and returns a structured JSON
-- description of the resolution: status (no-match / unbound / bound /
-- ambiguous), every disambiguation with its names, and a single
-- best-guess name picked by shortest-then-alphabetical.
module Unison.MCP.Tools.Probe
  ( probeTool,
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
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.MCP.Cli (cliToMCP)
import Unison.MCP.Domain.Resolution
  ( ResolvedHash (..),
    Resolution (..),
    Status (..),
    bestGuess,
    resolveHash,
    statusOf,
  )
import Unison.MCP.Types
import Unison.MCP.Wire qualified as Wire
import Unison.MCP.Wrapper
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.ShortHash qualified as ShortHash
import Unison.Syntax.Name qualified as Name
import UnliftIO qualified

-- ----------------------------------------------------------------------------
-- Tool
-- ----------------------------------------------------------------------------

probeTool :: Tool MCP
probeTool =
  Tool
    { toolName = toToolName ProbeTool,
      toolDescription =
        "Resolve a short hash (e.g. '#abc123') to its likely current name in \
        \the given branch. Returns structured JSON \
        \{hash, status: no_match|unbound|bound|ambiguous, resolutions: \
        \[{kind: term|type|ctor, fullHash, names}], bestGuess}.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Probe",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @ProbeToolArguments,
      toolHandler = \(ProbeToolArguments {projectContext, hash}) -> handleToolError $ do
        sh <- case ShortHash.fromText hash of
          Nothing -> throwError $ "Not a parseable short hash: " <> hash
          Just sh -> pure sh
        codebase <- asks (.codebase)
        let noop _ = pure ()
        (mb, _output) <- cliToMCP projectContext noop Cli.getCurrentBranch0
        case mb of
          Nothing -> throwError "No current branch"
          Just b -> do
            r <- UnliftIO.liftIO $ resolveHash codebase b sh
            pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode (renderResponse r)
    }

-- ----------------------------------------------------------------------------
-- Wire format
-- ----------------------------------------------------------------------------

data ProbeResponse = ProbeResponse
  { hash :: Text,
    status :: Text,
    resolutions :: [ResolutionJson],
    bestGuessName :: Maybe Text
  }

instance Aeson.ToJSON ProbeResponse where
  toJSON p =
    Aeson.object
      [ "hash" Aeson..= p.hash,
        "status" Aeson..= p.status,
        "resolutions" Aeson..= p.resolutions,
        "bestGuess" Aeson..= p.bestGuessName
      ]

data ResolutionJson = ResolutionJson
  { kind :: Text,
    fullHash :: Text,
    namesText :: [Text]
  }

instance Aeson.ToJSON ResolutionJson where
  toJSON r =
    Aeson.object
      [ "kind" Aeson..= r.kind,
        "fullHash" Aeson..= r.fullHash,
        "names" Aeson..= r.namesText
      ]

renderResponse :: ResolvedHash -> ProbeResponse
renderResponse r =
  ProbeResponse
    { hash = Wire.shortHashText r.hashInput,
      status = statusText (statusOf r),
      resolutions = map renderResolution r.resolutions,
      bestGuessName = Name.toText <$> bestGuess r
    }

renderResolution :: Resolution -> ResolutionJson
renderResolution r =
  ResolutionJson
    { kind = depKind r.dep,
      fullHash = depHash r.dep,
      namesText = map Name.toText (Set.toList r.names)
    }

depKind :: LabeledDependency -> Text
depKind = \case
  LD.ConReference {} -> "ctor"
  LD.TermReference _ -> "term"
  LD.TypeReference _ -> "type"

depHash :: LabeledDependency -> Text
depHash = \case
  LD.TermReferent ref -> Wire.shortHashText (Referent.toShortHash ref)
  LD.TypeReference ref -> Wire.shortHashText (Reference.toShortHash ref)

statusText :: Status -> Text
statusText = \case
  NoMatch -> "no_match"
  Unbound -> "unbound"
  Bound -> "bound"
  Ambiguous -> "ambiguous"
