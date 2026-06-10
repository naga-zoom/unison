{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @source-rename@ MCP tool: render source for one or more
-- definitions, with optional text substitutions applied to the rendered
-- output.
--
-- This is a /lexical/ rename — substitutions are applied to the rendered
-- definition text, /not/ to the codebase. The output is suitable as a
-- scratch buffer for a follow-up 'update-definitions' call.
--
-- For a semantic rename that updates every caller in the codebase, use
-- the 'rename-definition' tool instead.
--
-- Substitutions are plain 'Data.Text.replace' (substring). The caller
-- must pick non-ambiguous identifiers — e.g., renaming @foo@ will also
-- match the @foo@ inside @foobar@. When in doubt, qualify the @from@
-- with a unique prefix.
module Unison.MCP.Tools.SourceRename
  ( sourceRenameTool,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Unison.Codebase.Editor.Input qualified as Input
import Unison.HashQualified qualified as HQ
import Unison.MCP.Cli (CliOutput (..), handleInputMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Prelude

sourceRenameTool :: Tool MCP
sourceRenameTool =
  Tool
    { toolName = toToolName SourceRenameTool,
      toolDescription =
        "View the source of one or more definitions, with optional \
        \lexical renames applied to the rendered output. Substitutions \
        \are plain text replacements — pick non-ambiguous identifiers \
        \(e.g., qualified names) to avoid matching inside larger \
        \identifiers. For a semantic rename that updates every caller, \
        \use rename-definition instead.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Source with Renames",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @SourceRenameToolArguments,
      toolHandler = \(SourceRenameToolArguments {projectContext, names, renames}) -> handleToolError $ do
        case NEL.nonEmpty names of
          Nothing ->
            pure $ errorToolResult "No names provided to render"
          Just nonEmptyNames -> do
            let names' = HQ.NameOnly <$> nonEmptyNames
            output <- handleInputMCP projectContext [Right $ Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal names']
            let renamed = applyRenames renames output
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode renamed
            pure $ textToolResult outputJSON
    }

applyRenames :: [Rename] -> CliOutput -> CliOutput
applyRenames rs out =
  out
    { outputMessages = map (substituteAll rs) out.outputMessages,
      sourceCodeUpdates = map (substituteAll rs) out.sourceCodeUpdates,
      stdout = substituteAll rs out.stdout
    }

substituteAll :: [Rename] -> Text -> Text
substituteAll rs t = foldl' apply t rs
  where
    apply acc r
      | Text.null r.from = acc
      | otherwise = Text.replace r.from r.to acc
