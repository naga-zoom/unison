{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @cross-project-move@ MCP tool (v1, local-only): atomically copy
-- a single definition from one project/branch to another, then delete
-- the source binding.
--
-- /v1 scope:/ this version handles only the local steps. The full urf
-- workflow also pushes the destination to Share and re-installs in a
-- consumer branch; those steps are out of scope for v1 and must be
-- driven manually after the local move succeeds.
--
-- Compensation: if the destination write succeeds but the source delete
-- fails, the destination is rolled back (deleted). The orchestrator
-- preserves source on any failure.
--
-- Limitations:
--
-- * The rendered source is captured before the transaction. If the
--   definition has unqualified references to other project-local names,
--   the destination update will fail unless those references resolve in
--   the destination project's namespace.
-- * Text substitution is plain — pick a destination name that doesn't
--   appear as a substring of any other identifier in the source.
module Unison.MCP.Tools.CrossProjectMove
  ( crossProjectMoveTool,
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Unison.Codebase.Editor.Input qualified as Input
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.MCP.Cli (CliOutput (..), handleInputMCP, virtualSourceName)
import Unison.MCP.Domain.Transaction
  ( InverseFailure (..),
    PlanOutcome (..),
    Step (..),
    StepFailure (..),
    runPlan,
  )
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Name (Name)
import Unison.Syntax.Name qualified as Name
import Unison.Prelude
import UnliftIO qualified

crossProjectMoveTool :: Tool MCP
crossProjectMoveTool =
  Tool
    { toolName = toToolName CrossProjectMoveTool,
      toolDescription =
        "Cross-project move (v1, local-only): copy a definition from \
        \src to dest project/branch, then delete from src. Rolled back \
        \atomically if any step fails. v1 does NOT push to Share or \
        \install in a consumer — drive those steps manually after the \
        \move succeeds. Pass dryRun=true to see the plan without \
        \executing.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Cross-Project Move",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy @CrossProjectMoveToolArguments,
      toolHandler = \(CrossProjectMoveToolArguments {srcContext, srcName, destContext, destName, dryRun}) -> handleToolError $ do
        let actualDestName = fromMaybe srcName destName
        case fromMaybe False dryRun of
          True ->
            pure $
              textToolResult $
                encode $
                  Aeson.object
                    [ "dryRun" Aeson..= True,
                      "plan"
                        Aeson..= Aeson.object
                          [ "src" Aeson..= contextJSON srcContext srcName,
                            "dest" Aeson..= contextJSON destContext actualDestName,
                            "steps" Aeson..= (["write-dest", "delete-src"] :: [Text])
                          ]
                    ]
          False -> do
            -- Read source from src (outside the transactional plan).
            srcOut <- handleInputMCP srcContext [Right $ Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal (HQ.NameOnly srcName :| [])]
            let renderedSrc = Text.unlines srcOut.outputMessages
            when (Text.null (Text.strip renderedSrc)) $
              throwError $ "Source definition rendered empty: " <> Name.toText srcName
            let renamedSrc = substituteName srcName actualDestName renderedSrc
            let plan =
                  [ Step
                      "write-dest"
                      (mcpAction $ withSource destContext renamedSrc)
                      (mcpAction $ handleInputMCP destContext [Right $ Input.DeleteI True Input.DeleteTarget'TermOrType [HQ'.NameOnly actualDestName]]),
                    Step
                      "delete-src"
                      (mcpAction $ handleInputMCP srcContext [Right $ Input.DeleteI True Input.DeleteTarget'TermOrType [HQ'.NameOnly srcName]])
                      (mcpAction $ withSource srcContext renderedSrc)
                  ]
            outcome <- lift (runPlan plan)
            pure $ textToolResult (encode (outcomeJSON outcome))
    }
  where
    encode = Text.decodeUtf8 . BL.toStrict . Aeson.encode

substituteName :: Name -> Name -> Text -> Text
substituteName from to = Text.replace (Name.toText from) (Name.toText to)

contextJSON :: ProjectContext -> Name -> Aeson.Value
contextJSON ctx n =
  Aeson.object
    [ "project" Aeson..= (into @Text ctx.projectName :: Text),
      "branch" Aeson..= (into @Text ctx.branchName :: Text),
      "name" Aeson..= Name.toText n
    ]

outcomeJSON :: PlanOutcome -> Aeson.Value
outcomeJSON = \case
  PlanCommitted ->
    Aeson.object ["status" Aeson..= ("committed" :: Text)]
  PlanRolledBack (StepFailure {stepName, reason}) rolled ->
    Aeson.object
      [ "status" Aeson..= ("rolled-back" :: Text),
        "failedStep" Aeson..= stepName,
        "reason" Aeson..= reason,
        "rolledBack" Aeson..= rolled
      ]
  PlanRollbackFailed (StepFailure {stepName, reason}) rolled failedInverses ->
    Aeson.object
      [ "status" Aeson..= ("rollback-failed" :: Text),
        "failedStep" Aeson..= stepName,
        "reason" Aeson..= reason,
        "rolledBack" Aeson..= rolled,
        "failedInverses"
          Aeson..= [ Aeson.object
                       [ "step" Aeson..= n,
                         "reason" Aeson..= r
                       ]
                     | InverseFailure {inverseStepName = n, inverseReason = r} <- failedInverses
                   ]
      ]

mcpAction :: ExceptT Text MCP CliOutput -> MCP ()
mcpAction action = do
  result <- runExceptT action
  case result of
    Left err -> UnliftIO.throwIO (userError (Text.unpack err))
    Right out
      | null out.errorMessages -> pure ()
      | otherwise -> UnliftIO.throwIO (userError ("UCM error: " <> Text.unpack (Text.unlines out.errorMessages)))

withSource :: ProjectContext -> Text -> ExceptT Text MCP CliOutput
withSource projectContext source =
  handleInputMCP projectContext [Left (Input.UnisonFileChanged virtualSourceName source), Right Input.Update2I]
