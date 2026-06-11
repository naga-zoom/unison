{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @release@ MCP tool (v1, local-only): cut a release branch
-- named @releases\/<version>@ from the current branch.
--
-- /v1 scope:/ create the release branch locally. The agent is expected
-- to handle (manually, after the local step succeeds):
--
-- * Adding\/updating @ReleaseNotes : Doc@ and @Readme : Doc@
-- * Pushing the release branch to Share
-- * Tagging
--
-- The version string is validated as @MAJOR.MINOR.PATCH@ (each segment
-- numeric).
module Unison.MCP.Tools.Release
  ( releaseTool,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Char (isDigit)
import Data.Data (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Project (Semver (..))
import Unison.MCP.Cli (CliOutput (..), handleInputMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Prelude

releaseTool :: Tool MCP
releaseTool =
  Tool
    { toolName = toToolName ReleaseTool,
      toolDescription =
        "Cut a release draft (v1, local-only): runs UCM's \
        \`release.draft <version>` which creates the branch \
        \releases/drafts/<version> off the current context. The draft \
        \is later promoted to a final release. v1 does NOT add \
        \ReleaseNotes/Readme, push, or tag — drive those manually \
        \after the local branch creation succeeds. Version must be \
        \MAJOR.MINOR.PATCH (numeric segments). Pass dryRun=true to \
        \see the plan without executing.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Release",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy @ReleaseToolArguments,
      toolHandler = \(ReleaseToolArguments {projectContext, version, dryRun}) -> handleToolError $ do
        case parseSemverTriple version of
          Left err -> pure $ errorToolResult err
          Right semver@(Semver a b c) -> do
            -- UCM's release.draft creates `releases/drafts/X.Y.Z`. The draft is
            -- promoted to a final release later (separate flow not exposed via
            -- this tool yet).
            let releaseBranchText = "releases/drafts/" <> Text.pack (show a) <> "." <> Text.pack (show b) <> "." <> Text.pack (show c)
            case fromMaybe False dryRun of
              True ->
                pure $
                  textToolResult $
                    encode $
                      Aeson.object
                        [ "dryRun" Aeson..= True,
                          "version" Aeson..= version,
                          "willCreateBranch" Aeson..= releaseBranchText,
                          "sourceContext"
                            Aeson..= Aeson.object
                              [ "project" Aeson..= into @Text projectContext.projectName,
                                "branch" Aeson..= into @Text projectContext.branchName
                              ]
                        ]
              False -> do
                -- Use ReleaseDraftI (UCM's `release.draft`) — BranchI rejects
                -- 'releases/X.Y.Z' names as reserved.
                output <-
                  handleInputMCP projectContext [Right (Input.ReleaseDraftI semver)]
                let succeeded = null output.errorMessages
                pure $
                  textToolResult $
                    encode $
                      Aeson.object
                        [ "ok" Aeson..= succeeded,
                          "version" Aeson..= version,
                          "branchCreated" Aeson..= releaseBranchText,
                          "errors" Aeson..= output.errorMessages,
                          "output" Aeson..= output.outputMessages
                        ]
    }
  where
    encode = Text.decodeUtf8 . BL.toStrict . Aeson.encode

parseSemverTriple :: Text -> Either Text Semver
parseSemverTriple v =
  case Text.splitOn "." v of
    [a, b, c]
      | all (Text.all isDigit) [a, b, c],
        all (not . Text.null) [a, b, c],
        Just ai <- readMaybe (Text.unpack a),
        Just bi <- readMaybe (Text.unpack b),
        Just ci <- readMaybe (Text.unpack c) ->
          Right (Semver ai bi ci)
    _ -> Left $ "Invalid version: " <> v <> " — expected MAJOR.MINOR.PATCH"
