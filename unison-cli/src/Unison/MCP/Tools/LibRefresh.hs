{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @lib-refresh@ MCP tool: install a fresh snapshot of a library,
-- then delete the named old snapshots — in a single agent call.
--
-- Semantics:
--
-- * /Install runs first./ If it fails, /no/ deletions happen — the
--   caller's old snapshots are preserved intact.
-- * /Deletions are best-effort./ A failed deletion is reported but does
--   not roll back the install. This matches @urf lib-refresh@.
--
-- The caller passes the explicit list of old snapshots to delete (e.g.
-- @["unison_base_1_0_0", "unison_base_1_1_0"]@). Enumerating snapshots
-- is a separate call to 'list-project-libraries'.
module Unison.MCP.Tools.LibRefresh
  ( libRefreshTool,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Lens ((^?), ix)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Type (Branch0)
import Unison.Codebase.Editor.HandleInput.InstallLib (handleInstallLib)
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Path qualified as Path
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName (..), ProjectName (..))
import Unison.MCP.Cli (CliOutput (..), cliToMCP, handleInputMCP)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Project (ProjectBranchNameOrLatestRelease (..))
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment

libRefreshTool :: Tool MCP
libRefreshTool =
  Tool
    { toolName = toToolName LibRefreshTool,
      toolDescription =
        "Transactional library bump: install a fresh snapshot, then \
        \delete old snapshots matching the same project prefix. Install \
        \runs first — on install failure, NO deletions happen and the \
        \old snapshots are preserved. Deletions are best-effort: a \
        \failed deletion is reported but does not roll back the install. \
        \\n\nIf oldSnapshots is omitted or empty, the tool auto-enumerates \
        \lib/* children whose name starts with the derived prefix \
        \(e.g. `@unison/base` → `unison_base_*`). Pass dryRun=true to \
        \see the plan without executing.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Refresh Library",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just True
          },
      toolArgType = Proxy @LibRefreshToolArguments,
      toolHandler = \(LibRefreshToolArguments {projectContext, libProjectName, libBranchName, oldSnapshots, dryRun}) -> handleToolError $ do
        -- Auto-enumerate when caller didn't pin a list.
        resolvedSnapshots <-
          if null oldSnapshots
            then enumerateLibSnapshots projectContext (snapshotPrefix libProjectName)
            else pure oldSnapshots
        let plan =
              LibRefreshPlan
                { specProject = libProjectName,
                  specBranch = libBranchName,
                  willDelete = resolvedSnapshots
                }
        case fromMaybe False dryRun of
          True -> pure $ textToolResult (encode (LibRefreshResponse plan Nothing []))
          False -> do
            (_, installOut) <- cliToMCP projectContext (const $ pure ()) $ do
              handleInstallLib False (ProjectAndBranch (UnsafeProjectName libProjectName) (ProjectBranchNameOrLatestRelease'Name . UnsafeProjectBranchName <$> libBranchName))
            case null installOut.errorMessages of
              False -> pure $ textToolResult (encode (LibRefreshResponse plan (Just (toInstallReport installOut)) []))
              True -> do
                -- Re-enumerate AFTER install so we don't delete the new snapshot.
                postInstall <- enumerateLibSnapshots projectContext (snapshotPrefix libProjectName)
                let preInstallSet = resolvedSnapshots
                let stillOld = filter (`elem` postInstall) preInstallSet
                deletions <- for stillOld (deleteSnapshot projectContext)
                pure $ textToolResult (encode (LibRefreshResponse plan (Just (toInstallReport installOut)) deletions))
    }
  where
    encode = Text.decodeUtf8 . BL.toStrict . Aeson.encode

-- | Convention: @@owner/project@ becomes prefix @owner_project_@ (chars in
-- @{/, -}@ are mapped to @_@). Matches the snapshot-naming convention UCM
-- uses when pulling a library into @lib\/@.
snapshotPrefix :: Text -> Text
snapshotPrefix raw =
  let stripped = Text.dropWhile (== '@') raw
      munged = Text.map (\c -> if c == '/' || c == '-' then '_' else c) stripped
   in munged <> "_"

enumerateLibSnapshots :: ProjectContext -> Text -> EMCP [Text]
enumerateLibSnapshots ctx prefix = do
  let noop _ = pure ()
  (mb, _) <- cliToMCP ctx noop Cli.getCurrentBranch0
  case mb of
    Nothing -> pure []
    Just b0 -> pure (filter (Text.isPrefixOf prefix) (libChildNames b0))

libChildNames :: Branch0 m -> [Text]
libChildNames b =
  case b ^? Branch.children_ . ix NameSegment.libSegment . Branch.head_ of
    Nothing -> []
    Just libBranch ->
      Map.keys (libBranch ^. Branch.children_)
        & map NameSegment.toEscapedText

deleteSnapshot :: ProjectContext -> Text -> EMCP DeletionReport
deleteSnapshot projectContext snapshotName = do
  case Name.parseTextEither ("lib." <> snapshotName) of
    Left err -> pure $ DeletionReport snapshotName False (Just err)
    Right name -> do
      let split = Path.splitFromName name
      output <- handleInputMCP projectContext [Right $ Input.DeleteNamespaceI Input.Force (Just split)]
      pure $
        DeletionReport
          snapshotName
          (null output.errorMessages)
          (case output.errorMessages of [] -> Nothing; (e : _) -> Just e)

type EMCP = ExceptT Text MCP

data LibRefreshResponse = LibRefreshResponse
  { plan :: LibRefreshPlan,
    install :: Maybe InstallReport,
    deletions :: [DeletionReport]
  }

data LibRefreshPlan = LibRefreshPlan
  { specProject :: Text,
    specBranch :: Maybe Text,
    willDelete :: [Text]
  }

data InstallReport = InstallReport
  { ok :: Bool,
    errors :: [Text],
    output :: [Text]
  }

toInstallReport :: CliOutput -> InstallReport
toInstallReport out =
  InstallReport
    { ok = null out.errorMessages,
      errors = out.errorMessages,
      output = out.outputMessages
    }

data DeletionReport = DeletionReport
  { snapshot :: Text,
    deleted :: Bool,
    reason :: Maybe Text
  }

instance Aeson.ToJSON LibRefreshResponse where
  toJSON r =
    Aeson.object
      [ "plan" Aeson..= r.plan,
        "install" Aeson..= r.install,
        "deletions" Aeson..= r.deletions
      ]

instance Aeson.ToJSON LibRefreshPlan where
  toJSON p =
    Aeson.object
      [ "specProject" Aeson..= p.specProject,
        "specBranch" Aeson..= p.specBranch,
        "willDelete" Aeson..= p.willDelete
      ]

instance Aeson.ToJSON InstallReport where
  toJSON i =
    Aeson.object
      [ "ok" Aeson..= i.ok,
        "errors" Aeson..= i.errors,
        "output" Aeson..= i.output
      ]

instance Aeson.ToJSON DeletionReport where
  toJSON d =
    Aeson.object
      [ "snapshot" Aeson..= d.snapshot,
        "deleted" Aeson..= d.deleted,
        "reason" Aeson..= d.reason
      ]
