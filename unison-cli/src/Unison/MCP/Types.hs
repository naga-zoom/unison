{-# LANGUAGE NoFieldSelectors #-}

module Unison.MCP.Types
  ( MCP (..),
    Env (..),
    runMCP,
    ToolKind (..),
    ProjectCodeToolArguments (..),
    LibInstallToolArguments (..),
    ShareProjectSearchToolArguments (..),
    ShareProjectInfoToolArguments (..),
    TypecheckCodeToolArguments (..),
    ShareProjectReadmeToolArguments (..),
    ListLibraryDefinitionsToolArguments (..),
    ViewDefinitionsToolArguments (..),
    UpdateDefinitionsToolArguments (..),
    DiffUpdateToolArguments (..),
    SearchDefinitionsToolArguments (..),
    SearchByTypeToolArguments (..),
    DocsToolArguments (..),
    RunToolArguments (..),
    ProjectContext (..),
    ProjectContextArgument (..),
    ProjectNameArgument (..),
    ProjectDefinitionNameArgument (..),
    TestToolArguments (..),
    DeleteDefinitionsToolArguments (..),
    RenameDefinitionToolArguments (..),
    MoveDefinitionToolArguments (..),
    MoveToToolArguments (..),
    DeleteNamespaceToolArguments (..),
    ReflogToolArguments (..),
    HistoryToolArguments (..),
    CreateBranchToolArguments (..),
    CompileToolArguments (..),
    LibUpgradeToolArguments (..),
    FindToolArguments (..),
    ProbeToolArguments (..),
    DetectStaleToolArguments (..),
    CompleteUpdateToolArguments (..),
    DiagnoseToolArguments (..),
    PushToolArguments (..),
    PushTarget (..),
    PullToolArguments (..),
    PullSource (..),
    MergeToolArguments (..),
    MergeSource (..),
    ProjectCreateToolArguments (..),
    ProjectRenameToolArguments (..),
    ReapTempBranchesToolArguments (..),
    BranchDeleteToolArguments (..),
    BranchDeleteTarget (..),
    SourceRenameToolArguments (..),
    Rename (..),
    LibRefreshToolArguments (..),
    CrossProjectMoveToolArguments (..),
    ReanchorToolArguments (..),
    HashRename (..),
    SanityFixToolArguments (..),
    ReleaseToolArguments (..),
    EvalToolArguments (..),
    ListProjectLibrariesArgs (..),
    ListProjectBranchesArgs (..),
    ListProjectDefinitionsArgs (..),
    Pagination (..),
    CrossProjectDependentsToolArguments (..),
    FindAndActToolArguments (..),
    FindAction (..),
    PipelineToolArguments (..),
    PipelineStep (..),
    toToolName,
    fromToolName,
  )
where

import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.Path qualified as Path
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName), ProjectName (UnsafeProjectName))
import Unison.MCP.Wrapper (HasInputSchema (..))
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Runtime (Runtime)
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment
import Colog.Core (Severity)
import Unison.MCP.Stats (Stats)
import UnliftIO.STM qualified

data Env = Env
  { codebase :: Codebase IO Symbol Ann,
    runtime :: Runtime Symbol,
    sbRuntime :: Runtime Symbol,
    ucmVersion :: UCMVersion,
    workDir :: Maybe FilePath,
    authenticatedHTTPClient :: AuthenticatedHttpClient,
    -- | Merkle-keyed cache for expensive branch walks. Keyed by
    -- @(causalHashText, resourceKindText)@; entries become unreachable
    -- when the branch causal hash changes (no invalidation logic
    -- needed). See 'Unison.MCP.Cache'.
    branchCache :: UnliftIO.STM.TVar (Map (Text, Text) Value),
    -- | Per-tool counters + cache hit/miss accumulators. Queryable via
    -- the @stats@ MCP tool. See 'Unison.MCP.Stats'.
    stats :: Stats,
    -- | Current log-severity threshold for stderr emission. Set at
    -- start via @UCM_MCP_LOG_LEVEL@. Uses Colog's 'Severity' enum so
    -- the MCP logging shares a framework with UCM's LSP logging. See
    -- 'Unison.MCP.Log'.
    logLevel :: Severity
  }

newtype MCP a = MCP
  { unMCP :: ReaderT Env IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader Env)

runMCP :: Env -> MCP a -> IO a
runMCP env (MCP m) = do
  runReaderT m env

data ToolKind
  = ProjectCodeTool
  | LibInstallTool
  | ShareProjectSearchTool
  | ShareProjectReadmeTool
  | ShareProjectInfoTool
  | TypecheckCodeTool
  | DocsTool
  | RunTool
  | ListProjectDefinitionsTool
  | ListProjectLibrariesTool
  | ListLibraryDefinitionsTool
  | ViewDefinitionsTool
  | UpdateDefinitionsTool
  | SearchDefinitionsTool
  | SearchByTypeTool
  | ListLocalProjectsTool
  | ListProjectBranchesTool
  | GetCurrentProjectContextTool
  | DependenciesTool
  | DependentsTool
  | TestsTool
  | DeleteDefinitionsTool
  | RenameDefinitionTool
  | MoveDefinitionTool
  | MoveToTool
  | DeleteNamespaceTool
  | DiffUpdateTool
  | ReflogTool
  | HistoryTool
  | CreateBranchTool
  | CompileTool
  | LibUpgradeTool
  | FindTool
  | ProbeTool
  | DetectStaleTool
  | CompleteUpdateTool
  | DiagnoseTool
  | PushTool
  | PullTool
  | MergeTool
  | ProjectCreateTool
  | ProjectRenameTool
  | ReapTempBranchesTool
  | BranchDeleteTool
  | SourceRenameTool
  | LibRefreshTool
  | CrossProjectMoveTool
  | ReanchorTool
  | SanityFixTool
  | ReleaseTool
  | EvalTool
  | CrossProjectDependentsTool
  | FindAndActTool
  | PipelineTool
  | StatsTool
  deriving (Eq, Ord, Show, Bounded, Enum)

kindNameMapping :: Map ToolKind Text
kindNameMapping =
  Map.fromList
    [ (ProjectCodeTool, "project-code"),
      (LibInstallTool, "lib-install"),
      (ShareProjectSearchTool, "share-project-search"),
      (ShareProjectReadmeTool, "share-project-readme"),
      (ShareProjectInfoTool, "share-project-info"),
      (TypecheckCodeTool, "typecheck-code"),
      (DocsTool, "docs"),
      (RunTool, "run"),
      (ListProjectDefinitionsTool, "list-project-definitions"),
      (ListProjectLibrariesTool, "list-project-libraries"),
      (ListLibraryDefinitionsTool, "list-library-definitions"),
      (ViewDefinitionsTool, "view-definitions"),
      (UpdateDefinitionsTool, "update-definitions"),
      (SearchDefinitionsTool, "search-definitions-by-name"),
      (SearchByTypeTool, "search-by-type"),
      (ListLocalProjectsTool, "list-local-projects"),
      (ListProjectBranchesTool, "list-project-branches"),
      (GetCurrentProjectContextTool, "get-current-project-context"),
      (DependenciesTool, "list-definition-dependencies"),
      (DependentsTool, "list-definition-dependents"),
      (TestsTool, "run-tests"),
      (DeleteDefinitionsTool, "delete-definitions"),
      (RenameDefinitionTool, "rename-definition"),
      (MoveDefinitionTool, "move-definition"),
      (MoveToTool, "move-to"),
      (DeleteNamespaceTool, "delete-namespace"),
      (DiffUpdateTool, "diff-update"),
      (ReflogTool, "reflog"),
      (HistoryTool, "history"),
      (CreateBranchTool, "create-branch"),
      (CompileTool, "compile"),
      (LibUpgradeTool, "lib-upgrade"),
      (FindTool, "find"),
      (ProbeTool, "probe"),
      (DetectStaleTool, "detect-stale"),
      (CompleteUpdateTool, "complete-update"),
      (DiagnoseTool, "diagnose"),
      (PushTool, "push"),
      (PullTool, "pull"),
      (MergeTool, "merge"),
      (ProjectCreateTool, "project-create"),
      (ProjectRenameTool, "project-rename"),
      (ReapTempBranchesTool, "reap-temp-branches"),
      (BranchDeleteTool, "branch-delete"),
      (SourceRenameTool, "source-rename"),
      (LibRefreshTool, "lib-refresh"),
      (CrossProjectMoveTool, "cross-project-move"),
      (ReanchorTool, "reanchor"),
      (SanityFixTool, "sanity-fix"),
      (ReleaseTool, "release"),
      (EvalTool, "eval"),
      (CrossProjectDependentsTool, "cross-project-dependents"),
      (FindAndActTool, "find-and-act"),
      (PipelineTool, "pipeline"),
      (StatsTool, "stats")
    ]

data ProjectDefinitionNameArgument = ProjectDefinitionNameArgument
  { definitionName :: Name,
    projectContext :: ProjectContext
  }
  deriving (Eq, Show)

instance HasInputSchema ProjectDefinitionNameArgument where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "definitionName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the definition to work with, e.g. `mynamespace.foo` or `lib.unison_base_1_0_0.data.List`." :: Text)
                  ],
              "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext)
            ],
        "required" .= ["definitionName", "projectContext" :: Text]
      ]

instance FromJSON ProjectDefinitionNameArgument where
  parseJSON = withObject "ProjectDefinitionNameArgument" $ \o -> do
    definitionNameText <- o .: "definitionName"
    definitionName <- case Name.parseTextEither definitionNameText of
      Left err -> fail $ "Invalid definition name: " ++ show err
      Right definitionName -> pure definitionName
    projectContext <- o .: "projectContext"
    pure $ ProjectDefinitionNameArgument {definitionName, projectContext}

newtype ProjectContextArgument = ProjectContextArgument ProjectContext
  deriving newtype (Eq, Show)

instance HasInputSchema ProjectContextArgument where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext)
            ],
        "required" .= ["projectContext" :: Text]
      ]

instance FromJSON ProjectContextArgument where
  parseJSON = withObject "ProjectContextArgument" $ \o -> do
    projectContext <- o .: "projectContext"
    pure $ ProjectContextArgument projectContext

data ProjectNameArgument = ProjectNameArgument
  { projectName :: ProjectName
  }
  deriving (Eq, Show)

instance HasInputSchema ProjectNameArgument where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of a project to work within, e.g. `@unison/base` or `@ceedubs/json`" :: Text)
                  ]
            ],
        "required" .= ["projectName" :: Text]
      ]

instance FromJSON ProjectNameArgument where
  parseJSON = withObject "ProjectNameArgument" $ \o -> do
    projectName <- UnsafeProjectName <$> o .: "projectName"
    pure $ ProjectNameArgument {projectName}

data SearchByTypeToolArguments = SearchByTypeToolArguments
  { projectContext :: ProjectContext,
    query :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema SearchByTypeToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "query"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("A type to search for, e.g. `[Nat] -> Nat`." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "query" :: Text]
      ]

instance FromJSON SearchByTypeToolArguments where
  parseJSON = withObject "SearchByTypeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    query <- o .: "query"
    pure $ SearchByTypeToolArguments {projectContext, query}

data SearchDefinitionsToolArguments = SearchDefinitionsToolArguments
  { projectContext :: ProjectContext,
    query :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema SearchDefinitionsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "query"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("A name to search for, e.g. `foldl`." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "query" :: Text]
      ]

instance FromJSON SearchDefinitionsToolArguments where
  parseJSON = withObject "SearchDefinitionsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    query <- o .: "query"
    pure $ SearchDefinitionsToolArguments {projectContext, query}

data ViewDefinitionsToolArguments = ViewDefinitionsToolArguments
  { projectContext :: ProjectContext,
    names :: [Name],
    hashes :: [Text],
    signaturesOnly :: Maybe Bool,
    withDirectDeps :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema ViewDefinitionsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "names"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("string" :: Text),
                          "description" .= ("The names of the definitions to view, e.g. `mynamespace.foo`." :: Text)
                        ],
                    "description" .= ("The names of the definitions to view." :: Text)
                  ],
              "hashes"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description" .= ("Optional: view by short-hash references (e.g. `#abc123`). Resolved server-side before rendering, no separate `probe` call required." :: Text)
                  ],
              "signaturesOnly"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("If true, return only type signatures (no bodies). Saves substantial tokens for large definitions. Default false." :: Text)
                  ],
              "withDirectDeps"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("If true, also render the direct dependencies of each requested definition in the same response. Saves one round-trip for the common 'understand this def + its deps' workflow. Default false." :: Text)
                  ]
            ],
        "required" .= ["projectContext" :: Text]
      ]

instance FromJSON ViewDefinitionsToolArguments where
  parseJSON = withObject "ViewDefinitionsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    namesRaw <- o .:? "names"
    let names = maybe [] (map Name.unsafeParseText) namesRaw
    hashes <- fromMaybe [] <$> o .:? "hashes"
    signaturesOnly <- o .:? "signaturesOnly"
    withDirectDeps <- o .:? "withDirectDeps"
    pure $ ViewDefinitionsToolArguments {projectContext, names, hashes, signaturesOnly, withDirectDeps}

data UpdateDefinitionsToolArguments = UpdateDefinitionsToolArguments
  { projectContext :: ProjectContext,
    code :: Either FilePath Text,
    dryRun :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema UpdateDefinitionsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "code"
                .= object
                  [ "description" .= ("The source code to update definitions to. Either the `sourceCode` key or the `filePath`, but not both." :: Text),
                    "type" .= ("object" :: Text),
                    "properties"
                      .= object
                        [ "sourceCode"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("The source code to update definitions to." :: Text)
                              ],
                          "filePath"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("The absolute file path to the source code." :: Text)
                              ]
                        ],
                    "additionalProperties" .= False,
                    "minProperties" .= (1 :: Int),
                    "maxProperties" .= (1 :: Int)
                  ]
            ],
        "required" .= ["projectContext", "code" :: Text]
      ]

instance FromJSON UpdateDefinitionsToolArguments where
  parseJSON = withObject "UpdateDefinitionsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    source <- o .: "code"
    mFilePath <- source .:? "filePath"
    mSourceCode <- source .:? "sourceCode"
    mText <- source .:? "text"
    let providedCount =
          length
            (filter id [isJust mFilePath, isJust mSourceCode, isJust mText])
    when (providedCount == 0) $
      fail "Expected one of: code.filePath, code.sourceCode"
    when (providedCount > 1) $
      fail "Expected exactly one of: code.filePath, code.sourceCode"
    code <- case (mFilePath, mSourceCode, mText) of
      (Just filePath, _, _) -> pure (Left filePath)
      (_, Just sourceCode, _) -> pure (Right sourceCode)
      (_, _, Just text) -> pure (Right text)
      _ -> fail "Expected one of: code.filePath, code.sourceCode"
    dryRun <- o .:? "dryRun"
    pure $ UpdateDefinitionsToolArguments {projectContext, code, dryRun}

data DiffUpdateToolArguments = DiffUpdateToolArguments
  { projectContext :: ProjectContext,
    code :: Either FilePath Text
  }
  deriving (Eq, Show)

instance HasInputSchema DiffUpdateToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "code"
                .= object
                  [ "description" .= ("The source code to diff against the current codebase. If a string, it is the source code itself. If a file path, it is the path to a file containing the source code." :: Text),
                    "oneOf"
                      .= [ object
                             [ "description" .= ("The file path to the source code." :: Text),
                               "type" .= ("object" :: Text),
                               "properties"
                                 .= object
                                   [ "filePath"
                                       .= object
                                         [ "type" .= ("string" :: Text),
                                           "description" .= ("An absolute file path to the source code." :: Text)
                                         ]
                                   ],
                               "required" .= ["filePath" :: Text],
                               "additionalProperties" .= False
                             ],
                           object
                             [ "description" .= ("The source code to use." :: Text),
                               "type" .= ("object" :: Text),
                               "properties"
                                 .= object
                                   [ "text"
                                       .= object
                                         [ "type" .= ("string" :: Text),
                                           "description" .= ("The source code." :: Text)
                                         ]
                                   ],
                               "required" .= ["text" :: Text],
                               "additionalProperties" .= False
                             ]
                         ]
                  ]
            ],
        "required" .= ["projectContext", "code" :: Text]
      ]

instance FromJSON DiffUpdateToolArguments where
  parseJSON = withObject "DiffUpdateToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    source <- o .: "code"
    code <-
      source .:? "filePath" >>= \case
        Just filePath -> pure $ Left filePath
        Nothing -> do
          text <- source .: "text"
          pure $ Right text
    pure $ DiffUpdateToolArguments {projectContext, code}

data ListLibraryDefinitionsToolArguments = ListLibraryDefinitionsToolArguments
  { projectContext :: ProjectContext,
    libName :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema ListLibraryDefinitionsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "libName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the library to list definitions for, e.g. `base` or `json`." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "libName" :: Text]
      ]

instance FromJSON ListLibraryDefinitionsToolArguments where
  parseJSON = withObject "ListLibraryDefinitionsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    libName <- o .: "libName"
    pure $ ListLibraryDefinitionsToolArguments {projectContext, libName}

data ShareProjectReadmeToolArguments = ShareProjectReadmeToolArguments
  { projectName :: Text,
    projectOwnerHandle :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema ShareProjectReadmeToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the project to fetch the README for. E.g. in a project reference like `@owner/project-name` this would be `project-name`" :: Text)
                  ],
              "projectOwnerHandle"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The handle of the project owner, e.g. in a project reference like `@owner/project-name` this would be `owner`" :: Text)
                  ]
            ],
        "required" .= ["projectName", "projectOwnerHandle" :: Text]
      ]

instance FromJSON ShareProjectReadmeToolArguments where
  parseJSON = withObject "ShareProjectReadmeToolArguments" $ \o -> do
    projectName <- o .: "projectName"
    projectOwnerHandle <- o .: "projectOwnerHandle"
    pure $ ShareProjectReadmeToolArguments {projectName, projectOwnerHandle}

data TypecheckCodeToolArguments = TypecheckCodeToolArguments
  { projectContext :: ProjectContext,
    code :: Either FilePath Text
  }
  deriving (Eq, Show)

instance HasInputSchema TypecheckCodeToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "code"
                .= object
                  [ "description" .= ("The source code to typecheck. Either the `sourceCode` key or the `filePath`, but not both." :: Text),
                    "type" .= ("object" :: Text),
                    "properties"
                      .= object
                        [ "sourceCode"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("The source code to typecheck." :: Text)
                              ],
                          "filePath"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("The absolute file path to the source code to typecheck." :: Text)
                              ]
                        ],
                    "additionalProperties" .= False,
                    "minProperties" .= (1 :: Int),
                    "maxProperties" .= (1 :: Int)
                  ]
            ],
        "required" .= ["projectContext", "code" :: Text]
      ]

instance FromJSON TypecheckCodeToolArguments where
  parseJSON = withObject "TypecheckCodeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    source <- o .: "code"
    source .:? "filePath" >>= \case
      Just filePath -> pure $ TypecheckCodeToolArguments {projectContext, code = Left filePath}
      Nothing -> do
        text <- source .: "sourceCode"
        pure $ TypecheckCodeToolArguments {projectContext, code = Right text}

data DocsToolArguments = DocsToolArguments
  { projectContext :: ProjectContext,
    name :: Name
  }
  deriving (Eq, Show)

instance HasInputSchema DocsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "name"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The definition name to fetch documentation for. E.g. `README` or `data.Map.fromList`" :: Text)
                  ],
              "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext)
            ],
        "required" .= ["name", "projectContext" :: Text]
      ]

instance FromJSON DocsToolArguments where
  parseJSON = withObject "DocsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    name <- Name.unsafeParseText <$> o .: "name"
    pure $ DocsToolArguments {projectContext, name}

data RunToolArguments = RunToolArguments
  { projectContext :: ProjectContext,
    mainFunctionName :: Name,
    args :: [Text],
    code :: Maybe (Either FilePath Text)
  }
  deriving (Eq, Show)

instance HasInputSchema RunToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "mainFunctionName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the main function to run, e.g. `main` or `mynamespace.myprogram`." :: Text)
                  ],
              "args"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("string" :: Text),
                          "description" .= ("An argument to pass to the main function." :: Text)
                        ],
                    "description" .= ("The arguments to pass to the main function." :: Text)
                  ],
              "code"
                .= object
                  [ "description" .= ("Optional source code to typecheck before running. Allows running definitions without updating the codebase. Either the `sourceCode` key or the `filePath`, but not both." :: Text),
                    "type" .= ("object" :: Text),
                    "properties"
                      .= object
                        [ "sourceCode"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("The source code to typecheck." :: Text)
                              ],
                          "filePath"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("An absolute file path to the source code." :: Text)
                              ]
                        ],
                    "additionalProperties" .= False,
                    "minProperties" .= (1 :: Int),
                    "maxProperties" .= (1 :: Int)
                  ]
            ],
        "required" .= ["projectContext", "mainFunctionName", "args" :: Text]
      ]

instance FromJSON RunToolArguments where
  parseJSON = withObject "RunToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    mainFunctionNameText <- o .: "mainFunctionName"
    mainFunctionName <- case Name.parseTextEither mainFunctionNameText of
      Left err -> fail $ "Invalid main function name: " ++ show err
      Right name -> pure name
    args <- o .: "args"
    code <-
      o .:? "code" >>= \case
        Nothing -> pure Nothing
        Just source ->
          source .:? "filePath" >>= \case
            Just filePath -> pure $ Just (Left filePath)
            Nothing -> do
              text <- source .: "sourceCode"
              pure $ Just (Right text)
    pure $ RunToolArguments {projectContext, mainFunctionName, args, code}

data ProjectCodeToolArguments = ProjectCodeToolArguments
  { projectContext :: ProjectContext
  }

instance FromJSON ProjectCodeToolArguments where
  parseJSON = withObject "ProjectCodeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    pure $ ProjectCodeToolArguments {projectContext}

data ProjectContext = ProjectContext
  { projectName :: ProjectName,
    branchName :: ProjectBranchName
  }
  deriving (Eq, Show)

instance HasInputSchema ProjectContext where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the project to work within, e.g. `@unison/base` or `@ceedubs/json`" :: Text)
                  ],
              "branchName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The branch of the project to work within, e.g. `main` or `develop`" :: Text)
                  ]
            ],
        "required" .= ["projectName", "branchName" :: Text]
      ]

-- | Accepts two equivalent JSON shapes:
--
-- 1. The full object form:
--    @{"projectName": "temper", "branchName": "main"}@ — ~60 bytes.
-- 2. The compact-path form:
--    @"temper:main"@ — ~14 bytes.
--
-- The compact form uses @:@ as the project\/branch separator (UCM
-- project names use @\/@ internally — e.g. @\@unison\/base@ — so a
-- distinct separator avoids ambiguity with release paths like
-- @releases\/drafts\/1.0.0@).
--
-- This is the cheapest way to cut the per-call projectContext token
-- cost without introducing server-side state. See
-- @docs\/mcp-spec-extensions\/session-state-and-cached-resources.md@.
instance FromJSON ProjectContext where
  parseJSON v = case v of
    Object o -> do
      projectName <- UnsafeProjectName <$> o .: "projectName"
      branchName <- UnsafeProjectBranchName <$> o .: "branchName"
      pure $ ProjectContext {projectName, branchName}
    String s ->
      case Text.splitOn ":" s of
        [proj, br] | not (Text.null proj), not (Text.null br) ->
          pure $
            ProjectContext
              { projectName = UnsafeProjectName proj,
                branchName = UnsafeProjectBranchName br
              }
        _ ->
          fail $ "Compact projectContext must be \"<project>:<branch>\", got: " <> Text.unpack s
    _ -> fail "Expected projectContext as object or compact \"proj:branch\" string"

instance ToJSON ProjectContext where
  toJSON (ProjectContext (UnsafeProjectName projectName) (UnsafeProjectBranchName branchName)) =
    object
      [ "projectName" .= projectName,
        "branchName" .= branchName
      ]

data LibInstallToolArguments = LibInstallToolArguments
  { projectContext :: ProjectContext,
    libProjectName :: Text,
    libBranchName :: Maybe Text
  }

instance FromJSON LibInstallToolArguments where
  parseJSON = withObject "LibInstallToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    libProjectName <- o .: "libProjectName"
    libBranchName <- o .:? "libBranchName"
    pure $
      LibInstallToolArguments
        { projectContext,
          libProjectName,
          libBranchName
        }

instance HasInputSchema LibInstallToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "libProjectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "The user-qualified name of the library project to install, e.g. `@unison/base` or `@ceedubs/json`" .= ("The name of the library project to install" :: Text)
                  ],
              "libBranchName"
                .= object
                  [ "type" .= ["string" :: Text, "null"],
                    "description" .= ("The optional branch of the library project to install, E.g. `main`. If null, the latest release will be used." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "libProjectName" :: Text]
      ]

data ShareProjectSearchToolArguments = ShareProjectSearchToolArguments
  { query :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema ShareProjectSearchToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "query"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The search query to use. E.g. \"http client\". By default, each search word is ANDed together, but you can use OR to search for multiple terms. E.g. \"http OR client\" will return results that match either term. You can also exclude results using \"-\", e.g. \"-http\" will exclude results that match the term \"http\". Wrap a term in quotes to search for an exact phrase, e.g. \"\"http client\"\" will search for the exact phrase \"http client\"." :: Text)
                  ]
            ],
        "required" .= ["query" :: Text]
      ]

instance FromJSON ShareProjectSearchToolArguments where
  parseJSON = withObject "ShareProjectSearchToolArguments" $ \o -> do
    query <- o .: "query"
    pure $ ShareProjectSearchToolArguments {query}

data ShareProjectInfoToolArguments = ShareProjectInfoToolArguments
  { projectName :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema ShareProjectInfoToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The full project name including owner, e.g. `@unison/base` or `@owner/project-name`" :: Text)
                  ]
            ],
        "required" .= ["projectName" :: Text]
      ]

instance FromJSON ShareProjectInfoToolArguments where
  parseJSON = withObject "ShareProjectInfoToolArguments" $ \o -> do
    projectName <- o .: "projectName"
    pure $ ShareProjectInfoToolArguments {projectName}

data TestToolArguments = TestToolArguments
  { projectContext :: ProjectContext,
    subnamespace :: Maybe Path.Path
  }
  deriving (Eq, Show)

instance HasInputSchema TestToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "subnamespace"
                .= object
                  [ "type" .= ["string" :: Text, "null"],
                    "description" .= ("An optional subnamespace within the project to run tests in. E.g. `mynamespace.tests`. If null, tests in the entire project will be run." :: Text)
                  ]
            ],
        "required" .= ["projectContext" :: Text]
      ]

instance FromJSON TestToolArguments where
  parseJSON = withObject "TestToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    subnamespace <- fmap Path.unsafeParseText <$> (o .:? "subnamespace")
    pure $ TestToolArguments {projectContext, subnamespace}

data DeleteDefinitionsToolArguments = DeleteDefinitionsToolArguments
  { projectContext :: ProjectContext,
    names :: [Name],
    force :: Bool
  }
  deriving (Eq, Show)

instance HasInputSchema DeleteDefinitionsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "names"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("string" :: Text),
                          "description" .= ("A definition name to delete, e.g. `mynamespace.foo` or `MyType`." :: Text)
                        ],
                    "description" .= ("The names of the definitions to delete." :: Text)
                  ],
              "force"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("If true, force delete even if the definition has dependents. Default is false." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "names" :: Text]
      ]

instance FromJSON DeleteDefinitionsToolArguments where
  parseJSON = withObject "DeleteDefinitionsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    names <- fmap Name.unsafeParseText <$> o .: "names"
    force <- o .:? "force" .!= False
    pure $ DeleteDefinitionsToolArguments {projectContext, names, force}

data RenameDefinitionToolArguments = RenameDefinitionToolArguments
  { projectContext :: ProjectContext,
    renames :: [(Name, NameSegment)]
  }
  deriving (Eq, Show)

instance HasInputSchema RenameDefinitionToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "oldName" .= object ["type" .= ("string" :: Text), "description" .= ("Single-rename mode: the current name, e.g. `mynamespace.foo`." :: Text)],
              "newNameSegment" .= object ["type" .= ("string" :: Text), "description" .= ("Single-rename mode: the new final segment (parent path preserved)." :: Text)],
              "renames"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("object" :: Text),
                          "properties"
                            .= object
                              [ "oldName" .= object ["type" .= ("string" :: Text)],
                                "newNameSegment" .= object ["type" .= ("string" :: Text)]
                              ],
                          "required" .= (["oldName", "newNameSegment"] :: [Text])
                        ],
                    "description" .= ("Bulk-rename mode: list of {oldName, newNameSegment} pairs. Either pass `renames` OR the single-mode `oldName` + `newNameSegment` fields." :: Text)
                  ]
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON RenameDefinitionToolArguments where
  parseJSON = withObject "RenameDefinitionToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    mBulk <- o .:? "renames"
    case mBulk of
      Just bulkObjs -> do
        parsed <- traverse parseRename bulkObjs
        pure $ RenameDefinitionToolArguments {projectContext, renames = parsed}
      Nothing -> do
        oldName <- Name.unsafeParseText <$> o .: "oldName"
        newNameSegment <- NameSegment.unsafeParseText <$> o .: "newNameSegment"
        pure $ RenameDefinitionToolArguments {projectContext, renames = [(oldName, newNameSegment)]}
    where
      parseRename = withObject "Rename pair" $ \r -> do
        oldN <- Name.unsafeParseText <$> r .: "oldName"
        newSeg <- NameSegment.unsafeParseText <$> r .: "newNameSegment"
        pure (oldN, newSeg)

data MoveDefinitionToolArguments = MoveDefinitionToolArguments
  { projectContext :: ProjectContext,
    moves :: [(Name, Name)]
  }
  deriving (Eq, Show)

instance HasInputSchema MoveDefinitionToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "oldName" .= object ["type" .= ("string" :: Text), "description" .= ("Single-move mode: current full path." :: Text)],
              "newName" .= object ["type" .= ("string" :: Text), "description" .= ("Single-move mode: new full path (can change namespace)." :: Text)],
              "moves"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("object" :: Text),
                          "properties"
                            .= object
                              [ "oldName" .= object ["type" .= ("string" :: Text)],
                                "newName" .= object ["type" .= ("string" :: Text)]
                              ],
                          "required" .= (["oldName", "newName"] :: [Text])
                        ],
                    "description" .= ("Bulk-move mode: list of {oldName, newName} pairs." :: Text)
                  ]
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON MoveDefinitionToolArguments where
  parseJSON = withObject "MoveDefinitionToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    mBulk <- o .:? "moves"
    case mBulk of
      Just bulkObjs -> do
        parsed <- traverse parseMove bulkObjs
        pure $ MoveDefinitionToolArguments {projectContext, moves = parsed}
      Nothing -> do
        oldName <- Name.unsafeParseText <$> o .: "oldName"
        newName <- Name.unsafeParseText <$> o .: "newName"
        pure $ MoveDefinitionToolArguments {projectContext, moves = [(oldName, newName)]}
    where
      parseMove = withObject "Move pair" $ \m -> do
        oldN <- Name.unsafeParseText <$> m .: "oldName"
        newN <- Name.unsafeParseText <$> m .: "newName"
        pure (oldN, newN)

data MoveToToolArguments = MoveToToolArguments
  { projectContext :: ProjectContext,
    sources :: [Path.Path'],
    destination :: Path.Path'
  }
  deriving (Eq, Show)

instance HasInputSchema MoveToToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "sources"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("string" :: Text),
                          "description" .= ("A path to move, e.g. `mynamespace.foo` or `MyType`." :: Text)
                        ],
                    "description" .= ("The paths of the definitions or namespaces to move. The final segment of each source is preserved in the destination." :: Text),
                    "minItems" .= (1 :: Int)
                  ],
              "destination"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The destination namespace to move the sources into, e.g. `othernamespace` or `foo.bar`. Each source's final segment is preserved." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "sources", "destination" :: Text]
      ]

instance FromJSON MoveToToolArguments where
  parseJSON = withObject "MoveToToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    sources <- fmap Path.unsafeParseText' <$> o .: "sources"
    destination <- Path.unsafeParseText' <$> o .: "destination"
    pure $ MoveToToolArguments {projectContext, sources, destination}

data DeleteNamespaceToolArguments = DeleteNamespaceToolArguments
  { projectContext :: ProjectContext,
    namespaceName :: Name,
    force :: Bool
  }
  deriving (Eq, Show)

instance HasInputSchema DeleteNamespaceToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "namespaceName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the namespace to delete, e.g. `mynamespace` or `foo.bar`. This will delete the namespace and all definitions within it." :: Text)
                  ],
              "force"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("If true, force delete even if definitions in the namespace have dependents. Default is false." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "namespaceName" :: Text]
      ]

instance FromJSON DeleteNamespaceToolArguments where
  parseJSON = withObject "DeleteNamespaceToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    namespaceName <- Name.unsafeParseText <$> o .: "namespaceName"
    force <- o .:? "force" .!= False
    pure $ DeleteNamespaceToolArguments {projectContext, namespaceName, force}

-- | Arguments for the reflog tool
data ReflogToolArguments = ReflogToolArguments
  { projectContext :: ProjectContext,
    scope :: Maybe Text, -- "branch" | "project" | "global", default "branch"
    limit :: Maybe Int,
    includeTimestamps :: Maybe Bool -- default True
  }
  deriving (Eq, Show)

instance HasInputSchema ReflogToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "scope"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["branch", "project", "global"] :: [Text]),
                    "description" .= ("The scope of the reflog: 'branch' (default) shows entries for the current branch, 'project' shows entries for all branches in the project, 'global' shows entries for all projects." :: Text)
                  ],
              "limit"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Maximum number of entries to return. Default is 100." :: Text)
                  ],
              "includeTimestamps"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("Whether to include timestamps in the output. Default is true." :: Text)
                  ]
            ],
        "required" .= ["projectContext" :: Text]
      ]

instance FromJSON ReflogToolArguments where
  parseJSON = withObject "ReflogToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    scope <- o .:? "scope"
    limit <- o .:? "limit"
    includeTimestamps <- o .:? "includeTimestamps"
    pure $ ReflogToolArguments {projectContext, scope, limit, includeTimestamps}

-- | Arguments for the history tool
data HistoryToolArguments = HistoryToolArguments
  { projectContext :: ProjectContext,
    startHash :: Maybe Text, -- optional starting causal hash
    limit :: Maybe Int,
    diffLimit :: Maybe Int
  }
  deriving (Eq, Show)

instance HasInputSchema HistoryToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "startHash"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Optional causal hash to start history from. If not provided, starts from the current branch head." :: Text)
                  ],
              "limit"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Maximum number of history entries to return. Default is 100." :: Text)
                  ],
              "diffLimit"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Maximum number of diff elements to show per entry. Default is 10." :: Text)
                  ]
            ],
        "required" .= ["projectContext" :: Text]
      ]

instance FromJSON HistoryToolArguments where
  parseJSON = withObject "HistoryToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    startHash <- o .:? "startHash"
    limit <- o .:? "limit"
    diffLimit <- o .:? "diffLimit"
    pure $ HistoryToolArguments {projectContext, startHash, limit, diffLimit}

-- | Arguments for the create-branch tool
data CreateBranchToolArguments = CreateBranchToolArguments
  { projectName :: ProjectName,
    newBranchName :: ProjectBranchName,
    sourceType :: Text, -- "current" | "empty" | "branch"
    sourceBranchProject :: Maybe ProjectName,
    sourceBranchName :: Maybe ProjectBranchName
  }
  deriving (Eq, Show)

instance HasInputSchema CreateBranchToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the project to create the branch in, e.g. `@unison/base` or `@ceedubs/json`" :: Text)
                  ],
              "newBranchName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name for the new branch, e.g. `feature/my-feature` or `develop`" :: Text)
                  ],
              "sourceType"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["current", "empty", "branch"] :: [Text]),
                    "description" .= ("The source for the new branch: 'current' creates from current context, 'empty' creates an empty branch, 'branch' creates from an existing branch specified by sourceBranchProject and sourceBranchName" :: Text)
                  ],
              "sourceBranchProject"
                .= object
                  [ "type" .= ["string" :: Text, "null"],
                    "description" .= ("When sourceType is 'branch', the project containing the source branch. Optional - defaults to the target project if not specified." :: Text)
                  ],
              "sourceBranchName"
                .= object
                  [ "type" .= ["string" :: Text, "null"],
                    "description" .= ("When sourceType is 'branch', the name of the source branch to copy from." :: Text)
                  ]
            ],
        "required" .= ["projectName", "newBranchName", "sourceType" :: Text]
      ]

instance FromJSON CreateBranchToolArguments where
  parseJSON = withObject "CreateBranchToolArguments" $ \o -> do
    projectName <- UnsafeProjectName <$> o .: "projectName"
    newBranchName <- UnsafeProjectBranchName <$> o .: "newBranchName"
    sourceType <- o .: "sourceType"
    sourceBranchProject <- fmap UnsafeProjectName <$> o .:? "sourceBranchProject"
    sourceBranchName <- fmap UnsafeProjectBranchName <$> o .:? "sourceBranchName"
    pure $ CreateBranchToolArguments {projectName, newBranchName, sourceType, sourceBranchProject, sourceBranchName}

data CompileToolArguments = CompileToolArguments
  { projectContext :: ProjectContext,
    mainFunctionName :: Name,
    outputPath :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema CompileToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "mainFunctionName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The main function to compile, e.g. `myMain` or `mynamespace.myprogram`." :: Text)
                  ],
              "outputPath"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Output file path (without .uc extension). UCM writes the .uc file relative to the codebase directory." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "mainFunctionName", "outputPath" :: Text]
      ]

instance FromJSON CompileToolArguments where
  parseJSON = withObject "CompileToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    mainFunctionName <- Name.unsafeParseText <$> o .: "mainFunctionName"
    outputPath <- o .: "outputPath"
    pure $ CompileToolArguments {projectContext, mainFunctionName, outputPath}

-- | Each element is a pair (old, new); the list is flattened: [old1, new1, old2, new2, ...]
data LibUpgradeToolArguments = LibUpgradeToolArguments
  { projectContext :: ProjectContext,
    oldLibName :: Text,
    newLibName :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema LibUpgradeToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "oldLibName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The current library name segment to upgrade from, e.g. `unison_base_1_0_0`." :: Text)
                  ],
              "newLibName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The new library name segment to upgrade to, e.g. `unison_base_2_0_0`." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "oldLibName", "newLibName" :: Text]
      ]

instance FromJSON LibUpgradeToolArguments where
  parseJSON = withObject "LibUpgradeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    oldLibName <- o .: "oldLibName"
    newLibName <- o .: "newLibName"
    pure $ LibUpgradeToolArguments {projectContext, oldLibName, newLibName}

data FindToolArguments = FindToolArguments
  { projectContext :: ProjectContext,
    query :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema FindToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "query"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description"
                      .= ( "Pattern DSL query. Supports kind:term|type|ctor|doc|test|ability, "
                             <> "name:NAME (with * and ? globs, =EXACT, or bare token for contains), "
                             <> "project:NAME, owner:NAME, Boolean composition (AND OR NOT), and parens. "
                             <> "Examples: 'kind:term AND name:List.*'  -- 'kind:type OR (kind:ctor AND NOT name:lib.*)'." ::
                             Text
                         )
                  ]
            ],
        "required" .= (["projectContext", "query"] :: [Text])
      ]

instance FromJSON FindToolArguments where
  parseJSON = withObject "FindToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    query <- o .: "query"
    pure $ FindToolArguments {projectContext, query}

data ProbeToolArguments = ProbeToolArguments
  { projectContext :: ProjectContext,
    hash :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema ProbeToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "hash"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description"
                      .= ( "A short hash to resolve, e.g. '#abc123'. Leading '#' is required; "
                             <> "cycle/cid suffixes (e.g. '#abc.1.2') are accepted." ::
                             Text
                         )
                  ]
            ],
        "required" .= (["projectContext", "hash"] :: [Text])
      ]

instance FromJSON ProbeToolArguments where
  parseJSON = withObject "ProbeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    hash <- o .: "hash"
    pure $ ProbeToolArguments {projectContext, hash}

newtype DetectStaleToolArguments = DetectStaleToolArguments
  { projectContext :: ProjectContext
  }
  deriving newtype (Eq, Show)

instance HasInputSchema DetectStaleToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext)
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON DetectStaleToolArguments where
  parseJSON = withObject "DetectStaleToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    pure $ DetectStaleToolArguments {projectContext}

newtype CompleteUpdateToolArguments = CompleteUpdateToolArguments
  { projectContext :: ProjectContext
  }
  deriving newtype (Eq, Show)

instance HasInputSchema CompleteUpdateToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext)
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON CompleteUpdateToolArguments where
  parseJSON = withObject "CompleteUpdateToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    pure $ CompleteUpdateToolArguments {projectContext}

newtype DiagnoseToolArguments = DiagnoseToolArguments
  { projectContext :: ProjectContext
  }
  deriving newtype (Eq, Show)

instance HasInputSchema DiagnoseToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext)
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON DiagnoseToolArguments where
  parseJSON = withObject "DiagnoseToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    pure $ DiagnoseToolArguments {projectContext}

data PushTarget = PushTarget
  { project :: Maybe Text,
    branch :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON PushTarget where
  parseJSON = withObject "PushTarget" $ \o -> do
    project <- o .:? "project"
    branch <- o .:? "branch"
    pure $ PushTarget {project, branch}

data PushToolArguments = PushToolArguments
  { projectContext :: ProjectContext,
    target :: Maybe PushTarget,
    pushBehavior :: Maybe Text
  }
  deriving (Eq, Show)

instance HasInputSchema PushToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "target"
                .= object
                  [ "type" .= ("object" :: Text),
                    "description"
                      .= ( "Optional remote target. When absent, uses the source branch's remote-tracking. \
                           \At least one of {project, branch} must be present when 'target' is provided." ::
                             Text
                         ),
                    "properties"
                      .= object
                        [ "project"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("Target project name on Share, e.g. '@unison/base'." :: Text)
                              ],
                          "branch"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("Target branch name on Share, e.g. 'main' or 'releases/2.0.0'." :: Text)
                              ]
                        ]
                  ],
              "pushBehavior"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["force", "require-empty", "require-non-empty"] :: [Text]),
                    "description"
                      .= ( "How to handle the remote namespace. 'force' overwrites; \
                           \'require-empty' insists the remote namespace be empty; \
                           \'require-non-empty' (default) refuses to push to an empty namespace." ::
                             Text
                         )
                  ]
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON PushToolArguments where
  parseJSON = withObject "PushToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    target <- o .:? "target"
    pushBehavior <- o .:? "pushBehavior"
    pure $ PushToolArguments {projectContext, target, pushBehavior}

data PullSource = PullSource
  { project :: Maybe Text,
    branch :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON PullSource where
  parseJSON = withObject "PullSource" $ \o -> do
    project <- o .:? "project"
    branch <- o .:? "branch"
    pure $ PullSource {project, branch}

data PullToolArguments = PullToolArguments
  { projectContext :: ProjectContext,
    source :: PullSource,
    pullMode :: Maybe Text
  }
  deriving (Eq, Show)

instance HasInputSchema PullToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "source"
                .= object
                  [ "type" .= ("object" :: Text),
                    "description"
                      .= ( "Remote source on Share. At least one of {project, branch} must be present. \
                           \Use branch='latest-release' to fetch the latest released version." ::
                             Text
                         ),
                    "properties"
                      .= object
                        [ "project"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("Remote project name on Share, e.g. '@unison/base'." :: Text)
                              ],
                          "branch"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("Remote branch name, e.g. 'main' or 'releases/2.0.0' or 'latest-release'." :: Text)
                              ]
                        ]
                  ],
              "pullMode"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["with-history", "without-history"] :: [Text]),
                    "description"
                      .= ( "Whether to fetch the full causal history (default) or only the current head." ::
                             Text
                         )
                  ]
            ],
        "required" .= (["projectContext", "source"] :: [Text])
      ]

instance FromJSON PullToolArguments where
  parseJSON = withObject "PullToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    source <- o .: "source"
    pullMode <- o .:? "pullMode"
    pure $ PullToolArguments {projectContext, source, pullMode}

data MergeSource = MergeSource
  { project :: Maybe Text,
    branch :: Text
  }
  deriving (Eq, Show)

instance FromJSON MergeSource where
  parseJSON = withObject "MergeSource" $ \o -> do
    project <- o .:? "project"
    branch <- o .: "branch"
    pure $ MergeSource {project, branch}

data MergeToolArguments = MergeToolArguments
  { projectContext :: ProjectContext,
    source :: MergeSource
  }
  deriving (Eq, Show)

instance HasInputSchema MergeToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "source"
                .= object
                  [ "type" .= ("object" :: Text),
                    "description"
                      .= ( "The branch to merge from. 'branch' is required; \
                           \'project' is optional and defaults to the target's project." ::
                             Text
                         ),
                    "properties"
                      .= object
                        [ "project"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("Optional source project name. Omit to merge from a branch in the same project." :: Text)
                              ],
                          "branch"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("Required source branch name." :: Text)
                              ]
                        ],
                    "required" .= (["branch"] :: [Text])
                  ]
            ],
        "required" .= (["projectContext", "source"] :: [Text])
      ]

instance FromJSON MergeToolArguments where
  parseJSON = withObject "MergeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    source <- o .: "source"
    pure $ MergeToolArguments {projectContext, source}

data ProjectCreateToolArguments = ProjectCreateToolArguments
  { projectContext :: ProjectContext,
    projectName :: Maybe Text,
    downloadBase :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema ProjectCreateToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "projectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Optional desired project name. UCM auto-generates a name if omitted." :: Text)
                  ],
              "downloadBase"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("Whether to install @unison/base into the new project (default: true)." :: Text)
                  ]
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON ProjectCreateToolArguments where
  parseJSON = withObject "ProjectCreateToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    projectName <- o .:? "projectName"
    downloadBase <- o .:? "downloadBase"
    pure $ ProjectCreateToolArguments {projectContext, projectName, downloadBase}

data ProjectRenameToolArguments = ProjectRenameToolArguments
  { projectContext :: ProjectContext,
    newName :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema ProjectRenameToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "newName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("New project name. UCM refuses if another project already has this name." :: Text)
                  ]
            ],
        "required" .= (["projectContext", "newName"] :: [Text])
      ]

instance FromJSON ProjectRenameToolArguments where
  parseJSON = withObject "ProjectRenameToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    newName <- o .: "newName"
    pure $ ProjectRenameToolArguments {projectContext, newName}

data ReapTempBranchesToolArguments = ReapTempBranchesToolArguments
  { projectContext :: ProjectContext,
    apply :: Maybe Bool,
    force :: Maybe Bool,
    extraPrefixes :: Maybe [Text]
  }
  deriving (Eq, Show)

instance HasInputSchema ReapTempBranchesToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "apply"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description"
                      .= ( "Whether to actually delete the safe candidates. Default false (dry-run): \
                           \just reports which temp branches would be deleted. \
                           \When true, deletes branches whose head is an ancestor of the target's head." ::
                             Text
                         )
                  ],
              "force"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description"
                      .= ( "Bypass the ancestor check. When true (and apply=true), every matching branch is \
                           \deleted regardless of whether its work has been integrated. Use carefully — \
                           \unintegrated work is lost." ::
                             Text
                         )
                  ],
              "extraPrefixes"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description"
                      .= ( "Additional branch-name prefixes to consider beyond the conventional temp set \
                           \(update-/merge-/upgrade-). Example: [\"feature-\", \"bugfix-\", \"fix-\", \"wip-\"]. \
                           \Each candidate still goes through the ancestor check unless force=true, so \
                           \unintegrated work is protected by default." ::
                             Text
                         )
                  ]
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON ReapTempBranchesToolArguments where
  parseJSON = withObject "ReapTempBranchesToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    apply <- o .:? "apply"
    force <- o .:? "force"
    extraPrefixes <- o .:? "extraPrefixes"
    pure $ ReapTempBranchesToolArguments {projectContext, apply, force, extraPrefixes}

data BranchDeleteTarget = BranchDeleteTarget
  { project :: Maybe Text,
    branch :: Text
  }
  deriving (Eq, Show)

instance FromJSON BranchDeleteTarget where
  parseJSON = withObject "BranchDeleteTarget" $ \o -> do
    project <- o .:? "project"
    branch <- o .: "branch"
    pure $ BranchDeleteTarget {project, branch}

data BranchDeleteToolArguments = BranchDeleteToolArguments
  { projectContext :: ProjectContext,
    target :: BranchDeleteTarget,
    force :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema BranchDeleteToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "target"
                .= object
                  [ "type" .= ("object" :: Text),
                    "description" .= ("The branch to delete. 'branch' required; 'project' optional and defaults to projectContext's project." :: Text),
                    "properties"
                      .= object
                        [ "project"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("Optional project name; defaults to projectContext's project." :: Text)
                              ],
                          "branch"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("Required branch name to delete." :: Text)
                              ]
                        ],
                    "required" .= (["branch"] :: [Text])
                  ],
              "force"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("Bypass the protected-branch refusal. Default false. Required to delete main, releases/*, or update-*/merge-*/upgrade-* branches." :: Text)
                  ]
            ],
        "required" .= (["projectContext", "target"] :: [Text])
      ]

instance FromJSON BranchDeleteToolArguments where
  parseJSON = withObject "BranchDeleteToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    target <- o .: "target"
    force <- o .:? "force"
    pure $ BranchDeleteToolArguments {projectContext, target, force}

data Rename = Rename
  { from :: Text,
    to :: Text
  }
  deriving (Eq, Show)

instance FromJSON Rename where
  parseJSON = withObject "Rename" $ \o -> do
    from <- o .: "from"
    to <- o .: "to"
    pure $ Rename {from, to}

data SourceRenameToolArguments = SourceRenameToolArguments
  { projectContext :: ProjectContext,
    names :: [Name],
    renames :: [Rename]
  }
  deriving (Eq, Show)

instance HasInputSchema SourceRenameToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "names"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("string" :: Text),
                          "description" .= ("The names of the definitions to render." :: Text)
                        ],
                    "description" .= ("The names of the definitions to render." :: Text)
                  ],
              "renames"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("object" :: Text),
                          "properties"
                            .= object
                              [ "from" .= object ["type" .= ("string" :: Text), "description" .= ("Identifier to replace in the rendered source." :: Text)],
                                "to" .= object ["type" .= ("string" :: Text), "description" .= ("Replacement identifier." :: Text)]
                              ],
                          "required" .= (["from", "to"] :: [Text])
                        ],
                    "description" .= ("Optional list of plain-text substitutions to apply to the rendered output. Use qualified or otherwise unambiguous identifiers — substitutions are not word-boundary aware." :: Text)
                  ]
            ],
        "required" .= (["projectContext", "names"] :: [Text])
      ]

instance FromJSON SourceRenameToolArguments where
  parseJSON = withObject "SourceRenameToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    names <- fmap Name.unsafeParseText <$> o .: "names"
    renames <- fromMaybe [] <$> o .:? "renames"
    pure $ SourceRenameToolArguments {projectContext, names, renames}

data LibRefreshToolArguments = LibRefreshToolArguments
  { projectContext :: ProjectContext,
    libProjectName :: Text,
    libBranchName :: Maybe Text,
    oldSnapshots :: [Text],
    dryRun :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema LibRefreshToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "libProjectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Library project name to install, e.g. `@unison/base`." :: Text)
                  ],
              "libBranchName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Optional branch/release of the library; omit for latest release." :: Text)
                  ],
              "oldSnapshots"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description" .= ("Names of old lib snapshots to delete AFTER the install succeeds, e.g. `[\"unison_base_1_0_0\"]`. Each is interpreted relative to `lib/`." :: Text)
                  ],
              "dryRun"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("If true, echo the planned operations without executing. Default false." :: Text)
                  ]
            ],
        "required" .= (["projectContext", "libProjectName"] :: [Text])
      ]

instance FromJSON LibRefreshToolArguments where
  parseJSON = withObject "LibRefreshToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    libProjectName <- o .: "libProjectName"
    libBranchName <- o .:? "libBranchName"
    oldSnapshots <- fromMaybe [] <$> o .:? "oldSnapshots"
    dryRun <- o .:? "dryRun"
    pure $ LibRefreshToolArguments {projectContext, libProjectName, libBranchName, oldSnapshots, dryRun}

data CrossProjectMoveToolArguments = CrossProjectMoveToolArguments
  { srcContext :: ProjectContext,
    srcName :: Name,
    destContext :: ProjectContext,
    destName :: Maybe Name,
    dryRun :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema CrossProjectMoveToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "srcContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "srcName" .= object ["type" .= ("string" :: Text), "description" .= ("Name of the definition in src." :: Text)],
              "destContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "destName" .= object ["type" .= ("string" :: Text), "description" .= ("Optional new name in dest; defaults to srcName." :: Text)],
              "dryRun" .= object ["type" .= ("boolean" :: Text), "description" .= ("If true, return the plan without executing." :: Text)]
            ],
        "required" .= (["srcContext", "srcName", "destContext"] :: [Text])
      ]

instance FromJSON CrossProjectMoveToolArguments where
  parseJSON = withObject "CrossProjectMoveToolArguments" $ \o -> do
    srcContext <- o .: "srcContext"
    srcName <- Name.unsafeParseText <$> o .: "srcName"
    destContext <- o .: "destContext"
    destName <- fmap Name.unsafeParseText <$> o .:? "destName"
    dryRun <- o .:? "dryRun"
    pure $ CrossProjectMoveToolArguments {srcContext, srcName, destContext, destName, dryRun}

data HashRename = HashRename
  { hash :: Text,
    name :: Text
  }
  deriving (Eq, Show)

instance FromJSON HashRename where
  parseJSON = withObject "HashRename" $ \o -> do
    hash <- o .: "hash"
    name <- o .: "name"
    pure $ HashRename {hash, name}

data ReanchorToolArguments = ReanchorToolArguments
  { projectContext :: ProjectContext,
    names :: [Name],
    mappings :: [HashRename]
  }
  deriving (Eq, Show)

instance HasInputSchema ReanchorToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "names"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description" .= ("Names of the definitions to render." :: Text)
                  ],
              "mappings"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("object" :: Text),
                          "properties"
                            .= object
                              [ "hash" .= object ["type" .= ("string" :: Text), "description" .= ("Hash reference to replace, e.g. `#abc123`." :: Text)],
                                "name" .= object ["type" .= ("string" :: Text), "description" .= ("Name to substitute in place of the hash." :: Text)]
                              ],
                          "required" .= (["hash", "name"] :: [Text])
                        ],
                    "description" .= ("List of hash → name substitutions to apply to the rendered source." :: Text)
                  ]
            ],
        "required" .= (["projectContext", "names", "mappings"] :: [Text])
      ]

instance FromJSON ReanchorToolArguments where
  parseJSON = withObject "ReanchorToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    names <- fmap Name.unsafeParseText <$> o .: "names"
    mappings <- o .: "mappings"
    pure $ ReanchorToolArguments {projectContext, names, mappings}

data SanityFixToolArguments = SanityFixToolArguments
  { projectContext :: ProjectContext,
    apply :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema SanityFixToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "apply" .= object ["type" .= ("boolean" :: Text), "description" .= ("If true, attempt to auto-fix misplaced constructors via move.term. Default false (report only)." :: Text)]
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON SanityFixToolArguments where
  parseJSON = withObject "SanityFixToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    apply <- o .:? "apply"
    pure $ SanityFixToolArguments {projectContext, apply}

data ReleaseToolArguments = ReleaseToolArguments
  { projectContext :: ProjectContext,
    version :: Text,
    dryRun :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema ReleaseToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "version" .= object ["type" .= ("string" :: Text), "description" .= ("Semantic version for the release, e.g. \"1.0.0\". The release branch will be created at releases/<version>." :: Text)],
              "dryRun" .= object ["type" .= ("boolean" :: Text), "description" .= ("If true, return the plan without executing. Default false." :: Text)]
            ],
        "required" .= (["projectContext", "version"] :: [Text])
      ]

instance FromJSON ReleaseToolArguments where
  parseJSON = withObject "ReleaseToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    version <- o .: "version"
    dryRun <- o .:? "dryRun"
    pure $ ReleaseToolArguments {projectContext, version, dryRun}

data EvalToolArguments = EvalToolArguments
  { projectContext :: ProjectContext,
    expression :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema EvalToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "expression"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("A Unison expression to evaluate, e.g. `List.map (n -> n * n) [1,2,3]` or `myProject.myFunction 42`. The expression is wrapped in a `>` watch line and typechecked + evaluated in one step." :: Text)
                  ]
            ],
        "required" .= (["projectContext", "expression"] :: [Text])
      ]

instance FromJSON EvalToolArguments where
  parseJSON = withObject "EvalToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    expression <- o .: "expression"
    pure $ EvalToolArguments {projectContext, expression}

-- | Pagination cursor returned alongside a paged list.
data Pagination = Pagination
  { offset :: Int,
    limit :: Int
  }
  deriving (Eq, Show)

data ListProjectLibrariesArgs = ListProjectLibrariesArgs
  { projectContext :: ProjectContext,
    offset :: Maybe Int,
    limit :: Maybe Int,
    prefix :: Maybe Text
  }
  deriving (Eq, Show)

instance HasInputSchema ListProjectLibrariesArgs where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "offset" .= object ["type" .= ("integer" :: Text), "description" .= ("Zero-based offset for pagination; default 0." :: Text)],
              "limit" .= object ["type" .= ("integer" :: Text), "description" .= ("Maximum number of libraries to return; default 100." :: Text)],
              "prefix" .= object ["type" .= ("string" :: Text), "description" .= ("Optional name-prefix filter, e.g. `unison_base_`." :: Text)]
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON ListProjectLibrariesArgs where
  parseJSON = withObject "ListProjectLibrariesArgs" $ \o -> do
    projectContext <- o .: "projectContext"
    offset <- o .:? "offset"
    limit <- o .:? "limit"
    prefix <- o .:? "prefix"
    pure $ ListProjectLibrariesArgs {projectContext, offset, limit, prefix}

data ListProjectBranchesArgs = ListProjectBranchesArgs
  { projectName :: ProjectName,
    offset :: Maybe Int,
    limit :: Maybe Int,
    prefix :: Maybe Text
  }
  deriving (Eq, Show)

instance HasInputSchema ListProjectBranchesArgs where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectName" .= object ["type" .= ("string" :: Text), "description" .= ("The project to list branches for." :: Text)],
              "offset" .= object ["type" .= ("integer" :: Text), "description" .= ("Zero-based offset for pagination; default 0." :: Text)],
              "limit" .= object ["type" .= ("integer" :: Text), "description" .= ("Maximum number of branches to return; default 100." :: Text)],
              "prefix" .= object ["type" .= ("string" :: Text), "description" .= ("Optional name-prefix filter." :: Text)]
            ],
        "required" .= (["projectName"] :: [Text])
      ]

instance FromJSON ListProjectBranchesArgs where
  parseJSON = withObject "ListProjectBranchesArgs" $ \o -> do
    projectName <- UnsafeProjectName <$> o .: "projectName"
    offset <- o .:? "offset"
    limit <- o .:? "limit"
    prefix <- o .:? "prefix"
    pure $ ListProjectBranchesArgs {projectName, offset, limit, prefix}

data ListProjectDefinitionsArgs = ListProjectDefinitionsArgs
  { projectContext :: ProjectContext,
    offset :: Maybe Int,
    limit :: Maybe Int,
    includeLibs :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema ListProjectDefinitionsArgs where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "offset" .= object ["type" .= ("integer" :: Text), "description" .= ("Zero-based offset for pagination; default 0." :: Text)],
              "limit" .= object ["type" .= ("integer" :: Text), "description" .= ("Maximum number of definitions to return; default 100." :: Text)],
              "includeLibs" .= object ["type" .= ("boolean" :: Text), "description" .= ("Include definitions from lib/* in the listing. Default false." :: Text)]
            ],
        "required" .= (["projectContext"] :: [Text])
      ]

instance FromJSON ListProjectDefinitionsArgs where
  parseJSON = withObject "ListProjectDefinitionsArgs" $ \o -> do
    projectContext <- o .: "projectContext"
    offset <- o .:? "offset"
    limit <- o .:? "limit"
    includeLibs <- o .:? "includeLibs"
    pure $ ListProjectDefinitionsArgs {projectContext, offset, limit, includeLibs}

data CrossProjectDependentsToolArguments = CrossProjectDependentsToolArguments
  { definitionName :: Name,
    projects :: Maybe [Text],
    branchName :: Maybe Text
  }
  deriving (Eq, Show)

instance HasInputSchema CrossProjectDependentsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "definitionName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The fully-qualified name of the definition whose dependents to find." :: Text)
                  ],
              "projects"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description" .= ("Optional explicit list of local project names to scan. When omitted, all local projects are scanned." :: Text)
                  ],
              "branchName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Branch name to use in each project; default `main`." :: Text)
                  ]
            ],
        "required" .= (["definitionName"] :: [Text])
      ]

instance FromJSON CrossProjectDependentsToolArguments where
  parseJSON = withObject "CrossProjectDependentsToolArguments" $ \o -> do
    definitionName <- Name.unsafeParseText <$> o .: "definitionName"
    projects <- o .:? "projects"
    branchName <- o .:? "branchName"
    pure $ CrossProjectDependentsToolArguments {definitionName, projects, branchName}

data FindAction
  = FindActionDelete
  | FindActionMoveTo Name
  deriving (Eq, Show)

instance FromJSON FindAction where
  parseJSON = withObject "FindAction" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "delete" -> pure FindActionDelete
      "move-to" -> do
        dst <- Name.unsafeParseText <$> o .: "destNamespace"
        pure (FindActionMoveTo dst)
      other -> fail $ "Unknown find-and-act action type: " <> Text.unpack other

data FindAndActToolArguments = FindAndActToolArguments
  { projectContext :: ProjectContext,
    query :: Text,
    action :: FindAction,
    dryRun :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema FindAndActToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "query" .= object ["type" .= ("string" :: Text), "description" .= ("Pattern DSL query, same syntax as `find`." :: Text)],
              "action"
                .= object
                  [ "type" .= ("object" :: Text),
                    "description" .= ("Action to apply to each match. Either {type: \"delete\"} or {type: \"move-to\", destNamespace: \"foo.bar\"}." :: Text),
                    "properties"
                      .= object
                        [ "type" .= object ["type" .= ("string" :: Text), "enum" .= (["delete", "move-to"] :: [Text])],
                          "destNamespace" .= object ["type" .= ("string" :: Text)]
                        ],
                    "required" .= (["type"] :: [Text])
                  ],
              "dryRun" .= object ["type" .= ("boolean" :: Text), "description" .= ("Default true. Pass false to actually perform the action on every match." :: Text)]
            ],
        "required" .= (["projectContext", "query", "action"] :: [Text])
      ]

instance FromJSON FindAndActToolArguments where
  parseJSON = withObject "FindAndActToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    query <- o .: "query"
    action <- o .: "action"
    dryRun <- o .:? "dryRun"
    pure $ FindAndActToolArguments {projectContext, query, action, dryRun}

data PipelineStep = PipelineStep
  { tool :: Text,
    arguments :: Value
  }
  deriving (Eq, Show)

instance FromJSON PipelineStep where
  parseJSON = withObject "PipelineStep" $ \o -> do
    tool <- o .: "tool"
    arguments <- o .:? "arguments" .!= object []
    pure $ PipelineStep {tool, arguments}

data PipelineToolArguments = PipelineToolArguments
  { steps :: [PipelineStep],
    stopOnFirstError :: Maybe Bool
  }
  deriving (Eq, Show)

instance HasInputSchema PipelineToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "steps"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("object" :: Text),
                          "properties"
                            .= object
                              [ "tool" .= object ["type" .= ("string" :: Text), "description" .= ("MCP tool name." :: Text)],
                                "arguments" .= object ["type" .= ("object" :: Text), "description" .= ("Arguments for the tool — same shape as a normal tools/call." :: Text)]
                              ],
                          "required" .= (["tool"] :: [Text])
                        ],
                    "description" .= ("Ordered list of steps. Each step's arguments are the same shape you'd pass to a normal tools/call." :: Text)
                  ],
              "stopOnFirstError" .= object ["type" .= ("boolean" :: Text), "description" .= ("Default true. If false, continues through all steps even when one fails." :: Text)]
            ],
        "required" .= (["steps"] :: [Text])
      ]

instance FromJSON PipelineToolArguments where
  parseJSON = withObject "PipelineToolArguments" $ \o -> do
    steps <- o .: "steps"
    stopOnFirstError <- o .:? "stopOnFirstError"
    pure $ PipelineToolArguments {steps, stopOnFirstError}

nameKindMapping :: Map Text ToolKind
nameKindMapping =
  (Map.toList kindNameMapping)
    & map (\(k, v) -> (v, k))
    & Map.fromList

toToolName :: ToolKind -> Text.Text
toToolName kind =
  (Map.lookup kind kindNameMapping)
    & fromMaybe (error $ "Unknown tool kind: " ++ show kind)

fromToolName :: Text.Text -> Maybe ToolKind
fromToolName name = Map.lookup name nameKindMapping
