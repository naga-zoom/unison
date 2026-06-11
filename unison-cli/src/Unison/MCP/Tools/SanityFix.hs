{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @sanity-fix@ MCP tool (v1, report-only by default): scan a
-- branch for structural issues and, when @apply=true@, attempt to
-- auto-fix misplaced constructors via @move.term@.
--
-- v1 scope:
--
-- * 'OrphanType' — reported only. Auto-fix is intentionally not
--   attempted: the right fix depends on context (rename, delete, or
--   re-anchor) and risks corrupting the branch if guessed wrong.
-- * 'MisplacedConstructor' — when @apply=true@ and the type has
--   exactly one name, the constructor is moved under that type. Other
--   cases are reported but not fixed.
module Unison.MCP.Tools.SanityFix
  ( sanityFixTool,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Except qualified
import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Path qualified as Path
import Unison.MCP.Cli (CliOutput (..), cliToMCP, handleInputMCP)
import Unison.MCP.Domain.Structural (StructuralIssue (..), scanBranch)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.Name (Name)
import Unison.Name qualified as Name (lastSegment, makeAbsolute)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Prelude
import UnliftIO qualified

sanityFixTool :: Tool MCP
sanityFixTool =
  Tool
    { toolName = toToolName SanityFixTool,
      toolDescription =
        "Scan the branch for structural issues (orphan types, \
        \misplaced constructors). Default is report-only. Pass \
        \apply=true to attempt safe auto-fixes — currently only \
        \misplaced constructors with a single-named owning type. \
        \Orphan types are always reported, never auto-fixed.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Sanity Fix",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy @SanityFixToolArguments,
      toolHandler = \(SanityFixToolArguments {projectContext, apply}) -> handleToolError $ do
        codebase <- asks (.codebase)
        let doApply = fromMaybe False apply
        let noop _ = pure ()
        (mb, _) <- cliToMCP projectContext noop Cli.getCurrentBranch0
        branch <- case mb of
          Nothing -> Control.Monad.Except.throwError "No current branch"
          Just b -> pure b
        -- Note: sanity-fix's scanBranch result isn't currently cached
        -- because Suggestion-derivation needs the typed StructuralIssue
        -- values, not just their JSON rendering. Caching here would
        -- require ToJSON/FromJSON round-trip; deferred. The diagnose
        -- tool DOES cache its scan (it only needs JSON output).
        issues <- UnliftIO.liftIO $ scanBranch codebase branch
        let suggested = mapMaybe suggestedFix issues
        applied <-
          if doApply
            then for suggested (applyOne projectContext)
            else pure []
        let result =
              Aeson.object
                [ "issues" Aeson..= map issueJSON issues,
                  "suggested" Aeson..= map suggestionJSON suggested,
                  "applied" Aeson..= map applicationJSON applied,
                  "totalIssues" Aeson..= length issues
                ]
        pure $ textToolResult $ Text.decodeUtf8 . BL.toStrict $ Aeson.encode result
    }

data Suggestion = Suggestion
  { ctorName :: Name,
    targetName :: Name
  }

suggestedFix :: StructuralIssue -> Maybe Suggestion
suggestedFix (MisplacedConstructor {ctorName, typeNames}) =
  case Set.toList typeNames of
    [singleType] ->
      let suffix = lastSegmentText ctorName
          targetText = Name.toText singleType <> "." <> suffix
       in case Name.parseTextEither targetText of
            Right target -> Just (Suggestion ctorName target)
            Left _ -> Nothing
    _ -> Nothing
suggestedFix _ = Nothing

lastSegmentText :: Name -> Text
lastSegmentText = NameSegment.toEscapedText . Name.lastSegment

data Application = Application
  { suggestion :: Suggestion,
    ok :: Bool,
    reason :: Maybe Text
  }

applyOne :: ProjectContext -> Suggestion -> EMCP Application
applyOne ctx s = do
  let src = Path.fromName' (Name.makeAbsolute s.ctorName)
      dst = Path.fromName' (Name.makeAbsolute s.targetName)
  output <- handleInputMCP ctx [Right $ Input.MoveAllI src dst]
  pure $
    Application
      { suggestion = s,
        ok = null output.errorMessages,
        reason = case output.errorMessages of [] -> Nothing; (e : _) -> Just e
      }

type EMCP = ExceptT Text MCP

issueJSON :: StructuralIssue -> Aeson.Value
issueJSON (OrphanType {typeRef, typeNames, declaredCtors, namedCtors}) =
  Aeson.object
    [ "kind" Aeson..= ("orphan-type" :: Text),
      "typeRef" Aeson..= Text.pack (show typeRef),
      "typeNames" Aeson..= map Name.toText (Set.toList typeNames),
      "declaredCtors" Aeson..= declaredCtors,
      "namedCtors" Aeson..= namedCtors
    ]
issueJSON (MisplacedConstructor {ctorName, ctorRef, typeRef, typeNames}) =
  Aeson.object
    [ "kind" Aeson..= ("misplaced-constructor" :: Text),
      "ctorName" Aeson..= Name.toText ctorName,
      "ctorRef" Aeson..= Text.pack (show ctorRef),
      "typeRef" Aeson..= Text.pack (show typeRef),
      "typeNames" Aeson..= map Name.toText (Set.toList typeNames)
    ]

suggestionJSON :: Suggestion -> Aeson.Value
suggestionJSON s =
  Aeson.object
    [ "ctorName" Aeson..= Name.toText s.ctorName,
      "targetName" Aeson..= Name.toText s.targetName
    ]

applicationJSON :: Application -> Aeson.Value
applicationJSON a =
  Aeson.object
    [ "suggestion" Aeson..= suggestionJSON a.suggestion,
      "ok" Aeson..= a.ok,
      "reason" Aeson..= a.reason
    ]
