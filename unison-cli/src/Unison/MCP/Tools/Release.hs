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
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName (..))
import Unison.MCP.Cli (CliOutput (..), handleInputMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Prelude

releaseTool :: Tool MCP
releaseTool =
  Tool
    { toolName = toToolName ReleaseTool,
      toolDescription =
        "Cut a release (v1, local-only): create the branch \
        \releases/<version> from the current branch. v1 does NOT add \
        \ReleaseNotes/Readme, push, or tag — drive those manually after \
        \the local branch creation succeeds. Version must be \
        \MAJOR.MINOR.PATCH (numeric segments). Pass dryRun=true to see \
        \the plan without executing.",
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
        case parseVersion version of
          Left err -> pure $ errorToolResult err
          Right validVersion -> do
            let releaseBranchText = "releases/" <> validVersion
                releaseBranch = UnsafeProjectBranchName releaseBranchText
            case fromMaybe False dryRun of
              True ->
                pure $
                  textToolResult $
                    encode $
                      Aeson.object
                        [ "dryRun" Aeson..= True,
                          "version" Aeson..= validVersion,
                          "willCreateBranch" Aeson..= releaseBranchText,
                          "sourceContext"
                            Aeson..= Aeson.object
                              [ "project" Aeson..= into @Text projectContext.projectName,
                                "branch" Aeson..= into @Text projectContext.branchName
                              ]
                        ]
              False -> do
                output <-
                  handleInputMCP projectContext
                    [Right $ Input.BranchI Input.BranchSourceI'CurrentContext (ProjectAndBranch Nothing releaseBranch)]
                let succeeded = null output.errorMessages
                pure $
                  textToolResult $
                    encode $
                      Aeson.object
                        [ "ok" Aeson..= succeeded,
                          "version" Aeson..= validVersion,
                          "branchCreated" Aeson..= releaseBranchText,
                          "errors" Aeson..= output.errorMessages,
                          "output" Aeson..= output.outputMessages
                        ]
    }
  where
    encode = Text.decodeUtf8 . BL.toStrict . Aeson.encode

parseVersion :: Text -> Either Text Text
parseVersion v =
  case Text.splitOn "." v of
    [a, b, c]
      | all Text.null [a, b, c] -> Left "Version must be MAJOR.MINOR.PATCH"
      | all (Text.all isDigit) [a, b, c],
        all (not . Text.null) [a, b, c] ->
          Right v
    _ -> Left $ "Invalid version: " <> v <> " — expected MAJOR.MINOR.PATCH"
