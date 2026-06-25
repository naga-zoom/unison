module Unison.MCP.Tools (tools) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.List.NonEmpty qualified as NEL
import Control.Lens ((&), (^.), (^?), ix)
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Format.ISO8601 (iso8601Show)
import Text.RawString.QQ (r)
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.DbId (RemoteProjectId (..))
import U.Codebase.Sqlite.ProjectReflog qualified as ProjectReflog
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Share.Projects qualified as Share.Projects
import Unison.Cli.Share.Projects.Types (RemoteProject (..))
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput.InstallLib (handleInstallLib)
import Unison.Codebase.Editor.Input (Event (..), FindScope (..), Input (..))
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath
import Unison.Codebase.Runtime.Profile (ProfileSpec (..))
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.ShortHash qualified as SH
import Unison.MCP.Cache qualified as Cache
import Unison.MCP.Cli (CliOutput (..), cliToMCP, handleInputMCP, virtualSourceName)
import Unison.MCP.Share.API (ReadmeResponse (..))
import Unison.MCP.Share.API qualified as Share
import Unison.MCP.Tools.BranchDelete (branchDeleteTool)
import Unison.MCP.Tools.CompleteUpdate (completeUpdateTool)
import Unison.MCP.Tools.DetectStale (detectStaleTool)
import Unison.MCP.Tools.Diagnose (diagnoseTool)
import Unison.MCP.Tools.Find (findTool)
import Unison.MCP.Tools.FindAndAct (findAndActTool)
import Unison.MCP.Tools.Pipeline (pipelineTool)
import Unison.MCP.Tools.Stats (statsTool)
import Unison.MCP.Tools.Merge (mergeTool)
import Unison.MCP.Tools.Probe (probeTool)
import Unison.MCP.Tools.ProjectCreate (projectCreateTool)
import Unison.MCP.Tools.ProjectRename (projectRenameTool)
import Unison.MCP.Tools.Pull (pullTool)
import Unison.MCP.Tools.Push (pushTool)
import Unison.MCP.Tools.CrossProjectDependents (crossProjectDependentsTool)
import Unison.MCP.Tools.CrossProjectMove (crossProjectMoveTool)
import Unison.MCP.Tools.LibRefresh (libRefreshTool)
import Unison.MCP.Tools.ReapTempBranches (reapTempBranchesTool)
import Unison.MCP.Tools.Reanchor (reanchorTool)
import Unison.MCP.Tools.Release (releaseTool)
import Unison.MCP.Tools.SanityFix (sanityFixTool)
import Unison.MCP.Tools.SourceRename (sourceRenameTool)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.MCP.Wrapper qualified as MCPWrapper
import Unison.Name (Name)
import Unison.NameSegment qualified as NameSegment
import Unison.Syntax.Name qualified as Name
import Unison.Prelude (fromMaybe, into, readUtf8)
import Unison.Project (ProjectBranchNameOrLatestRelease (..))
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Relation qualified as R
import UnliftIO qualified

-- MCP errors are just returned to the agent as text.
type MCPError = Text

type EMCP = ExceptT MCPError MCP

tools :: [MCPWrapper.Tool MCP]
tools =
  let baseTools = baseToolsList
      registry = Map.fromList ((\t -> (MCPWrapper.toolName t, t)) <$> baseTools)
   in baseTools <> [pipelineTool registry]

baseToolsList :: [MCPWrapper.Tool MCP]
baseToolsList =
  [ installLibTool,
    shareProjectSearchTool,
    shareProjectInfoTool,
    typecheckCodeTool,
    docsTool,
    runTool,
    shareProjectReadmeTool,
    listProjectDefinitionsTool,
    listProjectLibrariesTool,
    listLibraryDefinitionsTool,
    viewDefinitionsTool,
    updateTool,
    diffUpdateTool,
    listLocalProjectsTool,
    listProjectBranchesTool,
    getCurrentProjectContextTool,
    searchDefinitionsTool,
    searchByTypeTool,
    dependenciesTool,
    dependentsTool,
    runTestsTool,
    deleteDefinitionsTool,
    renameDefinitionTool,
    moveDefinitionTool,
    moveToTool,
    deleteNamespaceTool,
    reflogTool,
    historyTool,
    createBranchTool,
    compileTool,
    libUpgradeTool,
    findTool,
    probeTool,
    detectStaleTool,
    completeUpdateTool,
    diagnoseTool,
    pushTool,
    pullTool,
    mergeTool,
    projectCreateTool,
    projectRenameTool,
    reapTempBranchesTool,
    branchDeleteTool,
    sourceRenameTool,
    libRefreshTool,
    crossProjectMoveTool,
    reanchorTool,
    sanityFixTool,
    releaseTool,
    evalTool,
    crossProjectDependentsTool,
    findAndActTool,
    statsTool
  ]

currentProjectContext :: (MonadIO m, MonadReader Env m) => m ProjectContext
currentProjectContext = do
  Env {codebase} <- ask
  pp <- liftIO $ Codebase.runTransaction codebase $ Codebase.expectCurrentProjectPath
  pure $
    ProjectContext
      { projectName = pp.project.name,
        branchName = pp.branch.name
      }

installLibTool :: Tool MCP
installLibTool =
  Tool
    { toolName = toToolName LibInstallTool,
      toolDescription = "Install a library from Unison Share into the specified project.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Install Library",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just True
          },
      toolArgType = Proxy,
      toolHandler = \(LibInstallToolArguments {projectContext, libProjectName, libBranchName}) -> handleToolError $ do
        (_r, output) <- cliToMCP projectContext (const $ pure ()) $ do
          handleInstallLib False (ProjectAndBranch (UnsafeProjectName libProjectName) (ProjectBranchNameOrLatestRelease'Name . UnsafeProjectBranchName <$> libBranchName))
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

shareProjectSearchTool :: Tool MCP
shareProjectSearchTool =
  Tool
    { toolName = toToolName ShareProjectSearchTool,
      toolDescription = "Search Unison Share for projects and libraries.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Share Project Search",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just True
          },
      toolArgType = Proxy,
      toolHandler = \(ShareProjectSearchToolArguments {query}) -> do
        Env {authenticatedHTTPClient} <- ask
        result <- UnliftIO.liftIO $ Share.shareSearch authenticatedHTTPClient query
        case result of
          Right searchResult -> do
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode searchResult
            pure $ textToolResult outputJSON
          Left err -> do
            let errorMsg = "Error searching Unison Share: " <> Text.pack (show err)
            pure $ errorToolResult errorMsg
    }

shareProjectInfoTool :: Tool MCP
shareProjectInfoTool =
  Tool
    { toolName = toToolName ShareProjectInfoTool,
      toolDescription = "Get project information from Unison Share, including the latest release version. Requires authentication for private projects.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Share Project Info",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just True
          },
      toolArgType = Proxy,
      toolHandler = \(ShareProjectInfoToolArguments {projectName}) -> handleToolError $ do
        -- Use a dummy project context since getProjectByName doesn't need it
        dummyContext <- currentProjectContext
        let parsedName = UnsafeProjectName projectName
        (result, _output) <- cliToMCP dummyContext (const $ pure ()) $ Share.Projects.getProjectByName parsedName
        case result of
          Just (Just remoteProject) -> do
            let response =
                  Aeson.object
                    [ "projectId" Aeson..= remoteProject.projectId.unRemoteProjectId,
                      "projectName" Aeson..= (into @Text remoteProject.projectName),
                      "latestRelease" Aeson..= fmap (into @Text) remoteProject.latestRelease
                    ]
            pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode response
          Just Nothing -> do
            throwError $ "Project not found: " <> projectName
          Nothing -> do
            throwError "Failed to get project info"
    }

-- | Load and typecheck the provided code, THEN run the provided inputs within that scratchfile context.
withCode :: Either FilePath Text -> [Input] -> ProjectContext -> EMCP CallToolResult
withCode code inputs projectContext = do
  (filePath, source) <- case code of
    Left filePath -> (Text.pack filePath,) <$> liftIO (readUtf8 filePath)
    Right codeSnippet -> pure (virtualSourceName, codeSnippet)
  output <- handleInputMCP projectContext ([Left $ UnisonFileChanged filePath source] <> (Right <$> inputs))
  let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
  pure $ textToolResult outputJSON

evalTool :: Tool MCP
evalTool =
  Tool
    { toolName = toToolName EvalTool,
      toolDescription =
        "Evaluate a pure (or pure-ish) Unison expression in the project's \
        \context and return the result. Equivalent to writing `> <expr>` \
        \in a scratch file. For expressions with `IO` effects, use `run` \
        \with a `'{IO,Exception} ()` thunk instead.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Eval Expression",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @EvalToolArguments,
      toolHandler = \(EvalToolArguments {projectContext, expression}) -> handleToolError $ do
        let watch = "> " <> expression
        let inputs =
              [ Left (Input.UnisonFileChanged virtualSourceName watch)
              ]
        output <- handleInputMCP projectContext inputs
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

typecheckCodeTool :: Tool MCP
typecheckCodeTool =
  Tool
    { toolName = toToolName TypecheckCodeTool,
      toolDescription =
        [r| Typecheck a code snippet within the context of a project. Only definitions which which are part of libraries or which have been previously added or updated will be available to reference within code.

          The result will indicate any errors and suggested fixes, or will indicate that the code typechecks and is ready to add or update.

          If you would like to test the behaviour of any pure functions, you may prefix a code snippet with an angle bracket.

          e.g.

          ```
          > 1 + 2
          ```

          Or

          ```
          > let
              isGreaterThan3 x = x > 3
              isGreaterThan3 4
          ```

          If you wish to write unit tests, you may do so like this:

          ```
          test> Nat.tests.additionIsCommutative = test.verify do
            Each.repeat 100
            n = Random.natIn 0 1000
            m = Random.natIn 0 1000
            ensureEqual (n + m) (m + n)
          ```

          If you intend to update code, you may call the Update Definitions tool directly instead, it will typecheck and update in one step.
        |],
      toolAnnotations =
        ToolAnnotations
          { title = Just "Typecheck Code",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(TypecheckCodeToolArguments {code, projectContext}) -> handleToolError do
        -- Just load the code, nothing more
        withCode code [] projectContext
    }

docsTool :: Tool MCP
docsTool =
  Tool
    { toolName = toToolName DocsTool,
      toolDescription = "Fetch documentation for a definition in a local project.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Documentation",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(DocsToolArguments {name, projectContext}) -> handleToolError $ do
        output <- handleInputMCP projectContext [Right $ DocToMarkdownI name]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

runTool :: Tool MCP
runTool =
  Tool
    { toolName = toToolName RunTool,
      toolDescription = "Execute/Run a given definition. If `code` is provided, it will be typechecked first and the definition will be run from the typechecked file without updating the codebase.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Run",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(RunToolArguments {mainFunctionName, projectContext, args, code}) -> handleToolError $ do
        let input = ExecuteI NoProf (HQ.NameOnly mainFunctionName) (Text.unpack <$> args)
        case code of
          Nothing -> do
            output <- handleInputMCP projectContext [Right input]
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
            pure $ textToolResult outputJSON
          Just source ->
            withCode source [input] projectContext
    }

shareProjectReadmeTool :: Tool MCP
shareProjectReadmeTool =
  Tool
    { toolName = toToolName ShareProjectReadmeTool,
      toolDescription = "Fetch the README for a project from Unison Share. Read the markdownReadMe value in the response.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Project README",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just True
          },
      toolArgType = Proxy,
      toolHandler = \(ShareProjectReadmeToolArguments {projectName, projectOwnerHandle}) -> handleToolError $ do
        Env {authenticatedHTTPClient} <- ask
        result <- UnliftIO.liftIO $ Share.shareProjectReadme authenticatedHTTPClient projectOwnerHandle projectName
        case result of
          Right ReadmeResponse {markdownReadMe} -> do
            pure $ textToolResult markdownReadMe
          Left err -> do
            let errorMsg = "Error getting readme from Unison Share: " <> Text.pack (show err)
            pure $ errorToolResult errorMsg
    }

listProjectDefinitionsTool :: Tool MCP
listProjectDefinitionsTool =
  Tool
    { toolName = toToolName ListProjectDefinitionsTool,
      toolDescription =
        "List definitions in the project. Returns structured JSON with \
        \each definition's name + signature line. Supports pagination \
        \(offset, limit; defaults 0 and 100). By default excludes \
        \lib/* — pass includeLibs=true to include them.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List Project Definitions",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @ListProjectDefinitionsArgs,
      toolHandler = \(ListProjectDefinitionsArgs {projectContext, offset, limit, includeLibs}) -> handleToolError $ do
        let noop _ = pure ()
        (mFullBranch, _) <- cliToMCP projectContext noop Cli.getCurrentBranch
        case mFullBranch of
          Nothing -> pure $ errorToolResult "No current branch found"
          Just fullBranch -> do
            let b = Branch.head fullBranch
            let withoutLibs = Branch.deleteLibdeps b
            let baseBranch = if fromMaybe False includeLibs then b else withoutLibs
            if (R.null $ Branch.deepTerms baseBranch) && (R.null $ Branch.deepTypes baseBranch)
              then
                pure $
                  textToolResult $
                    Text.decodeUtf8 . BL.toStrict $
                      Aeson.encode $
                        Aeson.object ["definitions" Aeson..= ([] :: [Aeson.Value]), "totalCount" Aeson..= (0 :: Int)]
              else do
                -- Merkle-key the enumeration. Same branch hash → same cached
                -- entries, mutation produces a new hash → cache miss.
                let kind =
                      "list-project-definitions:"
                        <> if fromMaybe False includeLibs then "with-libs" else "no-libs"
                let cacheKey = Cache.branchCacheKey fullBranch kind
                allEntriesJSON <-
                  Cache.getOrComputeEMCP cacheKey $ do
                    out <- handleInputMCP projectContext [Right $ Input.FindI False (FindLocal Path.Root') []]
                    let entries = parseFindOutput out.outputMessages
                    let mkEntry (n, sig) = Aeson.object ["name" Aeson..= n, "signature" Aeson..= sig]
                    pure (Aeson.toJSON (map mkEntry entries))
                let allEntries = case Aeson.fromJSON allEntriesJSON of
                      Aeson.Success xs -> xs :: [Aeson.Value]
                      Aeson.Error _ -> []
                let total = length allEntries
                let off = fromMaybe 0 offset
                let lim = fromMaybe 100 limit
                let paged = take lim (drop off allEntries)
                let result =
                      Aeson.object
                        [ "definitions" Aeson..= paged,
                          "totalCount" Aeson..= total,
                          "offset" Aeson..= off,
                          "limit" Aeson..= lim,
                          "nextOffset" Aeson..= if off + lim < total then Just (off + lim) else Nothing :: Maybe Int
                        ]
                pure $ textToolResult (Text.decodeUtf8 . BL.toStrict $ Aeson.encode result)
    }

-- | Parse UCM's @find@ text output into @(name, signature)@ pairs.
-- Lines look like @\"   1. name : Type\"@ or @\"   3. type Foo = ...\"@.
parseFindOutput :: [Text] -> [(Text, Text)]
parseFindOutput msgs =
  msgs
    & concatMap Text.lines
    & mapMaybe parseLine
  where
    parseLine raw =
      let t = Text.strip raw
       in case Text.breakOn ". " t of
            (num, rest) | not (Text.null rest) && Text.all isDigit num ->
              let body = Text.stripStart (Text.drop 2 rest)
               in case Text.breakOn " : " body of
                    (name, sig) | not (Text.null sig) -> Just (Text.stripEnd name, Text.strip (Text.drop 3 sig))
                    _ -> Just (Text.strip body, "") -- type decls etc. lack ` : `
            _ -> Nothing
    isDigit c = c >= '0' && c <= '9'

listProjectLibrariesTool :: Tool MCP
listProjectLibrariesTool =
  Tool
    { toolName = toToolName ListProjectLibrariesTool,
      toolDescription =
        "List libraries in the project's lib namespace. Returns \
        \structured JSON with name + term/type counts per snapshot. \
        \Supports pagination (offset, limit; defaults 0 and 100) and \
        \prefix filter.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List Project Libraries",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @ListProjectLibrariesArgs,
      toolHandler = \(ListProjectLibrariesArgs {projectContext, offset, limit, prefix}) -> handleToolError $ do
        let noop _ = pure ()
        (mb, _) <- cliToMCP projectContext noop Cli.getCurrentBranch0
        case mb of
          Nothing -> pure $ errorToolResult "No current branch found"
          Just b -> do
            let allLibs = enumerateLibsWithCounts b
            let filtered = case prefix of
                  Just p -> filter (\(n, _, _) -> Text.isPrefixOf p n) allLibs
                  Nothing -> allLibs
            let total = length filtered
            let off = fromMaybe 0 offset
            let lim = fromMaybe 100 limit
            let paged = take lim (drop off filtered)
            let mkLib (n, terms, types) = Aeson.object ["name" Aeson..= n, "terms" Aeson..= terms, "types" Aeson..= types]
            let result =
                  Aeson.object
                    [ "libraries" Aeson..= map mkLib paged,
                      "totalCount" Aeson..= total,
                      "offset" Aeson..= off,
                      "limit" Aeson..= lim,
                      "nextOffset" Aeson..= if off + lim < total then Just (off + lim) else Nothing :: Maybe Int
                    ]
            pure $ textToolResult (Text.decodeUtf8 . BL.toStrict $ Aeson.encode result)
    }

-- | Walk a branch's @lib\/@ children and tally term/type counts per snapshot.
enumerateLibsWithCounts :: Branch.Branch0 m -> [(Text, Int, Int)]
enumerateLibsWithCounts b =
  case b ^? Branch.children_ . ix NameSegment.libSegment . Branch.head_ of
    Nothing -> []
    Just libBranch ->
      [ (NameSegment.toEscapedText seg, R.size (Branch.deepTerms childHead), R.size (Branch.deepTypes childHead))
        | (seg, childBranchM) <- sortOn fst (Map.toList (libBranch ^. Branch.children_)),
          let childHead = Branch.head childBranchM
      ]

listLibraryDefinitionsTool :: Tool MCP
listLibraryDefinitionsTool =
  Tool
    { toolName = toToolName ListLibraryDefinitionsTool,
      toolDescription = "List all definitions in the specified library.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List Library Definitions",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ListLibraryDefinitionsToolArguments {libName, projectContext}) -> handleToolError $ do
        let libPath = Path.AbsolutePath' $ Path.Absolute (Path.fromList [NameSegment.libSegment, NameSegment.unsafeParseText libName])
        definitions <- handleInputMCP projectContext [Right $ Input.FindI False (FindLocal libPath) []]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
        pure $ textToolResult outputJSON
    }

viewDefinitionsTool :: Tool MCP
viewDefinitionsTool =
  Tool
    { toolName = toToolName ViewDefinitionsTool,
      toolDescription = "View source for definitions. Accepts `names` (e.g. `mynamespace.foo`) and/or `hashes` (e.g. `#abc123`) — either or both. Pass `signaturesOnly=true` to get just type signatures (saves tokens on large bodies).",
      toolAnnotations =
        ToolAnnotations
          { title = Just "View Definitions",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ViewDefinitionsToolArguments {projectContext, names, hashes, signaturesOnly, withDirectDeps}) -> handleToolError $ do
        let parsedHashes = mapMaybe (fmap HQ.HashOnly . SH.fromText) hashes
        let nameQs = HQ.NameOnly <$> names
        case NEL.nonEmpty (nameQs <> parsedHashes) of
          Nothing ->
            pure $ errorToolResult "No names or hashes provided to view"
          Just nonEmpty -> do
            mainDefs <- handleInputMCP projectContext [Right $ Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal nonEmpty]
            depsRendered <-
              case fromMaybe False withDirectDeps of
                False -> pure mempty
                True -> do
                  let depInputs = [Right (Input.ListDependenciesI q) | q <- NEL.toList nonEmpty]
                  depsRaw <- handleInputMCP projectContext depInputs
                  let depNames = mapMaybe parseDepName (concatMap Text.lines depsRaw.outputMessages)
                  case NEL.nonEmpty (HQ.NameOnly <$> depNames) of
                    Nothing -> pure mempty
                    Just depNonEmpty ->
                      handleInputMCP projectContext [Right $ Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal depNonEmpty]
            let combined = mainDefs <> depsRendered
            let trimmed = if fromMaybe False signaturesOnly then trimToSignatures combined else combined
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode trimmed
            pure $ textToolResult outputJSON
    }

-- | Parse one rendered ListDependenciesI line into a Name, if it carries one.
-- The rendered text looks like @"  1. some.qualified.Name"@ for terms and
-- @"  1. type X"@ for types — both forms are extracted.
parseDepName :: Text -> Maybe Name
parseDepName raw =
  let t = Text.strip raw
      withoutNum = case Text.breakOn ". " t of
        (num, rest) | not (Text.null rest) && Text.all (\c -> c >= '0' && c <= '9') num ->
          Text.stripStart (Text.drop 2 rest)
        _ -> t
      candidate = case Text.stripPrefix "type " withoutNum of
        Just rest -> Text.stripStart rest
        Nothing -> case Text.stripPrefix "ability " withoutNum of
          Just rest -> Text.stripStart rest
          Nothing -> withoutNum
   in case Name.parseTextEither candidate of
        Right n -> Just n
        Left _ -> Nothing

trimToSignatures :: CliOutput -> CliOutput
trimToSignatures out =
  out {outputMessages = map signatureOnly out.outputMessages}
  where
    signatureOnly :: Text -> Text
    signatureOnly = Text.unlines . filter isSig . Text.lines
    isSig line =
      let s = Text.stripStart line
       in " : " `Text.isInfixOf` line
            || any (`Text.isPrefixOf` s) ["type ", "unique type ", "structural type ", "ability ", "unique ability ", "structural ability "]

updateTool :: Tool MCP
updateTool =
  Tool
    { toolName = toToolName UpdateDefinitionsTool,
      toolDescription =
        "Typecheck, then update definitions in the codebase to the \
        \provided code. The typecheck is atomic — on any error the \
        \codebase is unchanged. Skip a separate typecheck-code call \
        \unless you want a dry-run validation without persisting; in \
        \that case pass `dryRun: true` here to get the same diff \
        \without writing.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Update Definitions",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(UpdateDefinitionsToolArguments {projectContext, code, dryRun}) -> handleToolError $ do
        case fromMaybe False dryRun of
          True -> withCode code [] projectContext
          False -> withCode code [Input.Update2I] projectContext
    }

diffUpdateTool :: Tool MCP
diffUpdateTool =
  Tool
    { toolName = toToolName DiffUpdateTool,
      toolDescription = "Show a diff of what changes would be made if `update` were run with the provided code. This is a read-only preview that doesn't modify the codebase.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Diff Update",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(DiffUpdateToolArguments {projectContext, code}) -> handleToolError $ do
        withCode code [Input.DiffUpdateI] projectContext
    }

listLocalProjectsTool :: Tool MCP
listLocalProjectsTool =
  Tool
    { toolName = toToolName ListLocalProjectsTool,
      toolDescription = "List all local projects.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List Local Projects",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(()) -> handleToolError $ do
        pc <- currentProjectContext
        projects <- handleInputMCP pc [Right Input.ProjectsI]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode projects
        pure $ textToolResult outputJSON
    }

listProjectBranchesTool :: Tool MCP
listProjectBranchesTool =
  Tool
    { toolName = toToolName ListProjectBranchesTool,
      toolDescription =
        "List branches of a project. Returns structured JSON with each \
        \branch's name. Supports pagination (offset, limit; defaults 0 \
        \and 100) and prefix filter.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List Project Branches",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @ListProjectBranchesArgs,
      toolHandler = \(ListProjectBranchesArgs {projectName, offset, limit, prefix}) -> handleToolError $ do
        codebase <- asks (.codebase)
        mProj <- UnliftIO.liftIO $ Codebase.runTransaction codebase $ Q.loadProjectByName projectName
        case mProj of
          Nothing -> pure $ errorToolResult ("Project not found: " <> into @Text projectName)
          Just project -> do
            allBranches <- UnliftIO.liftIO $ Codebase.runTransaction codebase $
              Q.loadAllProjectBranchesBeginningWith project.projectId Nothing
            let names = [into @Text bn | (_, bn) <- allBranches]
            let filtered = case prefix of
                  Just p -> filter (Text.isPrefixOf p) names
                  Nothing -> names
            let total = length filtered
            let off = fromMaybe 0 offset
            let lim = fromMaybe 100 limit
            let paged = take lim (drop off filtered)
            let result =
                  Aeson.object
                    [ "branches" Aeson..= [Aeson.object ["name" Aeson..= n] | n <- paged],
                      "totalCount" Aeson..= total,
                      "offset" Aeson..= off,
                      "limit" Aeson..= lim,
                      "nextOffset" Aeson..= if off + lim < total then Just (off + lim) else Nothing :: Maybe Int
                    ]
            pure $ textToolResult (Text.decodeUtf8 . BL.toStrict $ Aeson.encode result)
    }

getCurrentProjectContextTool :: Tool MCP
getCurrentProjectContextTool =
  Tool
    { toolName = toToolName GetCurrentProjectContextTool,
      toolDescription = "Get the current project context. This is useful for determining the user's working branch, but all commands take an explicit project context, so it's unnecessary if you already know which context is desired.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Get Current Project Context",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \() -> handleToolError $ do
        projectContext <- currentProjectContext
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode projectContext
        pure $ textToolResult outputJSON
    }

searchDefinitionsTool :: Tool MCP
searchDefinitionsTool =
  Tool
    { toolName = toToolName SearchDefinitionsTool,
      toolDescription = "Search for definitions in the current project or its library dependencies by name.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Search Definitions By Name",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(SearchDefinitionsToolArguments {projectContext, query}) -> handleToolError $ do
        definitions <- handleInputMCP projectContext [Right $ Input.FindI False (FindLocal Path.Root') [Text.unpack query]]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
        pure $ textToolResult outputJSON
    }

searchByTypeTool :: Tool MCP
searchByTypeTool =
  Tool
    { toolName = toToolName SearchByTypeTool,
      toolDescription = "Search for definitions in the current project or its library dependencies by type.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Search Definitions By Type",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(SearchByTypeToolArguments {projectContext, query}) -> handleToolError $ do
        definitions <- handleInputMCP projectContext [Right $ Input.FindI False (FindLocal Path.Root') [":", Text.unpack query]]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
        pure $ textToolResult outputJSON
    }

dependenciesTool :: Tool MCP
dependenciesTool =
  Tool
    { toolName = toToolName DependenciesTool,
      toolDescription = "List the dependencies of a definition.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List all definitions a given term or type depends on.",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ProjectDefinitionNameArgument {projectContext, definitionName}) -> handleToolError $ do
        output <- handleInputMCP projectContext [Right $ Input.ListDependenciesI (HQ.NameOnly definitionName)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

dependentsTool :: Tool MCP
dependentsTool =
  Tool
    { toolName = toToolName DependentsTool,
      toolDescription = "List the dependents of a definition.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List all definitions that depend on a given term or type.",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ProjectDefinitionNameArgument {projectContext, definitionName}) -> handleToolError $ do
        output <- handleInputMCP projectContext [Right $ Input.ListDependentsI (HQ.NameOnly definitionName)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

runTestsTool :: Tool MCP
runTestsTool =
  Tool
    { toolName = toToolName TestsTool,
      toolDescription = "Run the pure tests within a project.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Run Pure Tests",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(TestToolArguments {projectContext, subnamespace}) -> handleToolError $ do
        let testInput =
              Input.TestInput
                { includeLibNamespace = False,
                  path = case subnamespace of
                    Nothing -> mempty
                    Just ns -> ns,
                  showFailures = True,
                  showSuccesses = False
                }
        output <- handleInputMCP projectContext [Right $ Input.TestI testInput]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

deleteDefinitionsTool :: Tool MCP
deleteDefinitionsTool =
  Tool
    { toolName = toToolName DeleteDefinitionsTool,
      toolDescription =
        "Delete one or more named terms or types from the codebase. \
        \Removes the name binding; the underlying definition is GC'd if \
        \no other names point to it. \
        \UCM auto-refuses the delete if dependents exist in the current \
        \project (outside lib.*). For library-surface defs likely to be \
        \used by OTHER local projects, chain `cross-project-dependents` \
        \first (compose both via `pipeline` for an atomic check+delete). \
        \Pass `force: true` to skip the same-project safety check. \
        \\nResponse shape on dependent-blocked refusal: the response carries \
        \the dependents source in `sourceCodeUpdates` (so caller sees \
        \exactly what's blocking the delete) plus an explanatory \
        \`errorMessages` entry. UCM also creates an `update-*` temp branch \
        \as a side effect; clean it up with `cancel` (target the temp \
        \branch's projectContext) once you've read the dependents list. \
        \See also: `delete-namespace` (drop a whole subtree), \
        \`branch-delete` (drop a branch), `list-definition-dependents` \
        \(in-project impact), `cross-project-dependents` (impact across all \
        \local projects), `cancel` (clean up the dependents-blocked temp \
        \branch).",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Delete Definitions",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(DeleteDefinitionsToolArguments {projectContext, names, force}) -> handleToolError $ do
        case NEL.nonEmpty names of
          Nothing ->
            pure $ errorToolResult "No names provided to delete"
          Just nonEmptyNames -> do
            let names' = HQ'.NameOnly <$> NEL.toList nonEmptyNames
            -- When force=False and the target has dependents, UCM's
            -- handleDelete calls expectLatestFile to write the
            -- "dependents that need updating" scratch. MCP mode has no
            -- loaded scratch, so without prep that surfaces as the
            -- misleading NoUnisonFile message ("There's nothing for me
            -- to add right now"). Prep a virtual scratch so the write
            -- lands in MCP's sourceCodeUpdates instead — caller then
            -- sees the actual delete-blocked-by-dependents content.
            -- force=True doesn't touch latestFile; skip the prep.
            let inputs =
                  if force
                    then [Right $ Input.DeleteI True Input.DeleteTarget'TermOrType names']
                    else
                      [ Left (Input.UnisonFileChanged virtualSourceName ""),
                        Right $ Input.DeleteI False Input.DeleteTarget'TermOrType names'
                      ]
            output <- handleInputMCP projectContext inputs
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
            pure $ textToolResult outputJSON
    }

renameDefinitionTool :: Tool MCP
renameDefinitionTool =
  Tool
    { toolName = toToolName RenameDefinitionTool,
      toolDescription =
        "Rename one or more definitions by changing only the final name \
        \segment. Parent path preserved. Single-mode: pass oldName + \
        \newNameSegment. Bulk-mode: pass `renames: [{oldName, \
        \newNameSegment}, ...]` — applied in order, each in its own UCM \
        \input. To move across namespaces use move-to.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Rename Definition",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(RenameDefinitionToolArguments {projectContext, renames}) -> handleToolError $ do
        let inputs = [Right $ Input.RenameI (Path.fromName' n) seg | (n, seg) <- renames]
        output <- handleInputMCP projectContext inputs
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

moveDefinitionTool :: Tool MCP
moveDefinitionTool =
  Tool
    { toolName = toToolName MoveDefinitionTool,
      toolDescription =
        "Move one or more definitions to entirely new paths. Single-mode: \
        \pass oldName + newName. Bulk-mode: pass `moves: [{oldName, \
        \newName}, ...]` — applied in order. Use `move-to` if you want \
        \to drop multiple defs into the same destination namespace.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Move Definition",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(MoveDefinitionToolArguments {projectContext, moves}) -> handleToolError $ do
        let inputs = [Right $ Input.MoveAllI (Path.fromName' o) (Path.fromName' n) | (o, n) <- moves]
        output <- handleInputMCP projectContext inputs
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

moveToTool :: Tool MCP
moveToTool =
  Tool
    { toolName = toToolName MoveToTool,
      toolDescription = "Move one or more definitions or namespaces into a destination namespace. The final segment of each source is preserved. For example, `moveTo foo.bar dest` moves `foo.bar` into namespace `dest`, producing `dest.bar`. Multiple sources can be moved at once: `moveTo foo bar baz dest` moves all three into `dest`.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Move To",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(MoveToToolArguments {projectContext, sources, destination}) -> handleToolError $ do
        case NEL.nonEmpty sources of
          Nothing ->
            pure $ errorToolResult "No sources provided to move"
          Just nonEmptySources -> do
            output <- handleInputMCP projectContext [Right $ Input.MoveToI nonEmptySources destination]
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
            pure $ textToolResult outputJSON
    }

deleteNamespaceTool :: Tool MCP
deleteNamespaceTool =
  Tool
    { toolName = toToolName DeleteNamespaceTool,
      toolDescription = "Delete a namespace and all definitions within it from the codebase.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Delete Namespace",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(DeleteNamespaceToolArguments {projectContext, namespaceName, force}) -> handleToolError $ do
        let split = Path.splitFromName namespaceName
            insistence = if force then Input.Force else Input.Try
        output <- handleInputMCP projectContext [Right $ Input.DeleteNamespaceI insistence (Just split)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

reflogTool :: Tool MCP
reflogTool =
  Tool
    { toolName = toToolName ReflogTool,
      toolDescription = "Get the reflog (history of branch state changes) for a project or branch. Returns structured data about recent changes.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Reflog",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ReflogToolArguments {projectContext, scope, limit, includeTimestamps}) -> handleToolError $ do
        let limitVal = fromMaybe 100 limit
            scopeVal = fromMaybe "branch" scope
            includeTs = fromMaybe True includeTimestamps
        Env {codebase} <- ask

        -- Resolve project and branch from context
        let projName = projectContext.projectName
            branchName = projectContext.branchName

        entries <- liftIO $ Codebase.runTransaction codebase $ do
          schLength <- Codebase.branchHashLength
          mayProject <- Q.loadProjectByName projName
          case mayProject of
            Nothing -> pure []
            Just project -> do
              mayBranch <- Q.loadProjectBranchByName project.projectId branchName
              rawEntries <- case scopeVal of
                "global" -> Codebase.getGlobalReflog (limitVal + 1)
                "project" -> Codebase.getProjectReflog (limitVal + 1) project.projectId
                _ -> case mayBranch of
                  Nothing -> pure []
                  Just branch -> Codebase.getProjectBranchReflog (limitVal + 1) branch.branchId
              -- Convert entries to JSON-friendly format
              pure $ map (reflogEntryToJSON includeTs schLength) rawEntries

        let hasMore = length entries > limitVal
            finalEntries = take limitVal entries
            response =
              Aeson.object
                [ "entries" Aeson..= finalEntries,
                  "hasMore" Aeson..= hasMore
                ]
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode response
    }

reflogEntryToJSON :: Bool -> Int -> ProjectReflog.Entry Project ProjectBranch CausalHash -> Aeson.Value
reflogEntryToJSON includeTimestamps schLength entry =
  Aeson.object $
    [ "project" Aeson..= (into @Text $ entry.project.name),
      "branch" Aeson..= (into @Text $ entry.branch.name),
      "fromHash" Aeson..= fmap (("#" <>) . SCH.toText . SCH.fromHash schLength) entry.fromRootCausalHash,
      "toHash" Aeson..= (("#" <>) . SCH.toText . SCH.fromHash schLength $ entry.toRootCausalHash),
      "reason" Aeson..= entry.reason
    ]
      <> ["time" Aeson..= iso8601Show entry.time | includeTimestamps]

historyTool :: Tool MCP
historyTool =
  Tool
    { toolName = toToolName HistoryTool,
      toolDescription = "Get the causal history of a branch, showing changes over time. Returns structured data about commits and diffs.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "History",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(HistoryToolArguments {projectContext, startHash, limit, diffLimit}) -> handleToolError $ do
        -- Build the BranchId from startHash or use the current branch (root path)
        branchId <- case startHash of
          Just hash -> case SCH.fromText hash of
            Just sch -> pure $ Input.BranchAtSCH sch
            Nothing -> throwError $ "Invalid causal hash: " <> hash
          Nothing -> pure $ Input.BranchAtPath Path.Current'
        output <- handleInputMCP projectContext [Right $ Input.HistoryI limit diffLimit branchId]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode (stripHistoryPreamble output)
        pure $ textToolResult outputJSON
    }

-- | Strip UCM's "Note: The most recent namespace hash is immediately below this
-- message." preamble line from history output — pure noise to agents.
stripHistoryPreamble :: CliOutput -> CliOutput
stripHistoryPreamble out =
  out {outputMessages = map dropPreamble out.outputMessages}
  where
    preamble = "Note: The most recent namespace hash is immediately below this message."
    dropPreamble t =
      let ls = Text.lines t
          ls' = filter (not . Text.isInfixOf preamble) ls
       in Text.dropWhile (== '\n') (Text.unlines ls')

createBranchTool :: Tool MCP
createBranchTool =
  Tool
    { toolName = toToolName CreateBranchTool,
      toolDescription = "Create a new branch in a project. Can create from the current context, as an empty branch, or from an existing branch.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Create Branch",
            readOnlyHint = Just False,
            destructiveHint = Just False,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(CreateBranchToolArguments {projectName, newBranchName, sourceType, sourceBranchProject, sourceBranchName}) -> handleToolError $ do
        -- Build the BranchSourceI based on sourceType
        branchSource <- case sourceType of
          "current" -> pure Input.BranchSourceI'CurrentContext
          "empty" -> pure Input.BranchSourceI'Empty
          "branch" -> case sourceBranchName of
            Nothing -> throwError "sourceBranchName is required when sourceType is 'branch'"
            Just srcBranch ->
              pure $ Input.BranchSourceI'UnresolvedProjectBranch (ProjectAndBranch sourceBranchProject srcBranch)
          _ -> throwError $ "Invalid sourceType: " <> sourceType <> ". Must be 'current', 'empty', or 'branch'"

        -- Use a dummy project context for cliToMCP since BranchI handles project resolution itself
        dummyContext <- currentProjectContext
        let branchInput = Input.BranchI branchSource (ProjectAndBranch (Just projectName) newBranchName)
        output <- handleInputMCP dummyContext [Right branchInput]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

compileTool :: Tool MCP
compileTool =
  Tool
    { toolName = toToolName CompileTool,
      toolDescription = "Compile a Unison definition to a standalone .uc file. The file is written relative to the codebase directory. Run it with: ucm run.compiled <outputPath>.uc",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Compile",
            readOnlyHint = Just False,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(CompileToolArguments {projectContext, mainFunctionName, outputPath}) -> handleToolError $ do
        let input = MakeStandaloneI (Text.unpack outputPath) (HQ.NameOnly mainFunctionName)
        output <- handleInputMCP projectContext [Right input]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

libUpgradeTool :: Tool MCP
libUpgradeTool =
  Tool
    { toolName = toToolName LibUpgradeTool,
      toolDescription = "Upgrade a library dependency from one version to another. Equivalent to `lib.upgrade old new` in UCM.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Lib Upgrade",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(LibUpgradeToolArguments {projectContext, oldLibName, newLibName}) -> handleToolError $ do
        let segs = map NameSegment.unsafeParseText [oldLibName, newLibName]
            input = UpgradeI segs
        output <- handleInputMCP projectContext [Right input]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }
