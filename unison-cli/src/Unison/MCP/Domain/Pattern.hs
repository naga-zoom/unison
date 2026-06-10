{-# LANGUAGE OverloadedStrings #-}

-- | Composable Boolean query language for definitions in a Unison codebase.
--
-- The DSL lets callers describe a set of definitions via:
--
-- * atomic predicates: @kind:term@, @name:List.*@, @project:base@,
--   @owner:alice@
-- * Boolean composition: @AND@, @OR@, @NOT@
-- * grouping: @( ... )@
--
-- Examples:
--
-- > kind:term AND project:base AND name:internal.*
-- > kind:type OR (kind:ctor AND NOT name:lib.*)
-- > project:base AND owner:alice
--
-- The same 'Pattern' AST supports multiple interpreters:
--
-- * 'matchPattern' — pure Boolean evaluator (used by find, detect-stale, …)
-- * 'fingerprint'  — canonical byte serialization (used by policy diff
--   and audit)
-- * a future @toFindQuery@ — translate to a codebase query (added when the
--   @find@ tool is composed on top of this primitive)
--
-- Encoded as a plain ADT (initial encoding) rather than 'Free Selective'
-- because the operators are algebraic (associativity, commutativity,
-- idempotence) rather than conditional. Multiple interpreters traverse the
-- AST directly without runtime overhead.
module Unison.MCP.Domain.Pattern
  ( -- * AST
    Pattern (..),
    OpKind (..),
    NameMatcher (..),
    Operand (..),

    -- * Parser
    parsePattern,
    PatternParseError,

    -- * Evaluator
    matchPattern,

    -- * Canonical serialization (for audit / policy fingerprints)
    fingerprint,

    -- * Algebraic helpers
    simplify,
  )
where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<?>), (<|>))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as L

-- =============================================================================
-- AST
-- =============================================================================

-- | A Boolean predicate over codebase definitions.
--
-- 'Anything' is the identity of 'And' and the annihilator of 'Or'.
-- 'Nothing'' is the identity of 'Or' and the annihilator of 'And'.
data Pattern
  = Kind OpKind
  | NameP NameMatcher
  | Project Text
  | Owner Text
  | And Pattern Pattern
  | Or Pattern Pattern
  | Not Pattern
  | Anything
  | Nothing'
  deriving (Eq, Show)

-- | The kind classifier for a definition.
--
-- Doc and Test are heuristic: a term whose name ends in @.doc@ is treated as
-- a doc; a term whose name contains @.tests.@ as a test. These conventions
-- match what UCM already uses elsewhere.
data OpKind
  = OpTerm
  | OpType
  | OpCtor
  | OpDoc
  | OpTest
  | OpAbility
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Name matcher with the four standard forms plus a simple glob.
--
-- Glob characters: @*@ (any string) and @?@ (any single char). All other
-- characters match literally.
data NameMatcher
  = ExactName Text
  | Prefix Text
  | Suffix Text
  | Contains Text
  | Glob Text
  deriving (Eq, Show)

-- | The operand a 'Pattern' is evaluated against. Callers populate the
-- fields they know; fields they leave as 'Nothing' cause predicates that
-- require them to evaluate to 'False'.
--
-- Keeping this as plain 'Text' (rather than UCM's 'Name' / 'ProjectName')
-- means the pattern primitive is testable in isolation; integration
-- adapters convert at the boundary.
data Operand = Operand
  { opName :: Text,
    opKind :: OpKind,
    opProject :: Maybe Text,
    opOwner :: Maybe Text
  }
  deriving (Eq, Show)

-- =============================================================================
-- Evaluator
-- =============================================================================

-- | Pure Boolean evaluator. Total over the AST.
matchPattern :: Pattern -> Operand -> Bool
matchPattern p op = case p of
  Kind k -> opKind op == k
  NameP m -> matchName m (opName op)
  Project pr -> opProject op == Just pr
  Owner ow -> opOwner op == Just ow
  And a b -> matchPattern a op && matchPattern b op
  Or a b -> matchPattern a op || matchPattern b op
  Not q -> not (matchPattern q op)
  Anything -> True
  Nothing' -> False

matchName :: NameMatcher -> Text -> Bool
matchName m candidate = case m of
  ExactName t -> candidate == t
  Prefix t -> t `Text.isPrefixOf` candidate
  Suffix t -> t `Text.isSuffixOf` candidate
  Contains t -> t `Text.isInfixOf` candidate
  Glob g -> globMatch g candidate

-- | Simple glob: @*@ matches any sequence, @?@ matches any single char.
-- Greedy with backtracking. Adequate for namespace patterns; no character
-- classes or alternation by design.
globMatch :: Text -> Text -> Bool
globMatch glob input = go (Text.unpack glob) (Text.unpack input)
  where
    go :: String -> String -> Bool
    go [] [] = True
    go [] _ = False
    go ('*' : gs) cs =
      -- match zero chars, or any prefix
      go gs cs || (case cs of [] -> False; (_ : cs') -> go ('*' : gs) cs')
    go ('?' : gs) (_ : cs) = go gs cs
    go ('?' : _) [] = False
    go (g : gs) (c : cs)
      | g == c = go gs cs
      | otherwise = False
    go (_ : _) [] = False

-- =============================================================================
-- Simplification (algebraic laws as rewrites)
-- =============================================================================

-- | Canonicalize a 'Pattern' by applying identity / annihilator laws.
-- Useful before 'fingerprint' so semantically-equivalent patterns get the
-- same bytes. Not a normal form (doesn't reorder associative chains).
simplify :: Pattern -> Pattern
simplify = \case
  And a b -> case (simplify a, simplify b) of
    (Anything, x) -> x
    (x, Anything) -> x
    (Nothing', _) -> Nothing'
    (_, Nothing') -> Nothing'
    (x, y) -> And x y
  Or a b -> case (simplify a, simplify b) of
    (Nothing', x) -> x
    (x, Nothing') -> x
    (Anything, _) -> Anything
    (_, Anything) -> Anything
    (x, y) -> Or x y
  Not q -> case simplify q of
    Not q' -> q' -- double negation
    Anything -> Nothing'
    Nothing' -> Anything
    q' -> Not q'
  leaf -> leaf

-- =============================================================================
-- Canonical serialization (for policy fingerprint / audit)
-- =============================================================================

-- | Deterministic textual fingerprint. Two patterns that 'simplify' to the
-- same AST produce the same bytes. Sufficient for audit-row equality
-- checks without requiring a cryptographic-strength hash.
fingerprint :: Pattern -> Text
fingerprint = go . simplify
  where
    go :: Pattern -> Text
    go = \case
      Kind k -> "kind:" <> kindToken k
      NameP m -> "name:" <> nameMatcherToken m
      Project p -> "project:" <> p
      Owner o -> "owner:" <> o
      And a b -> "(" <> go a <> " AND " <> go b <> ")"
      Or a b -> "(" <> go a <> " OR " <> go b <> ")"
      Not q -> "NOT(" <> go q <> ")"
      Anything -> "ANYTHING"
      Nothing' -> "NOTHING"

kindToken :: OpKind -> Text
kindToken = \case
  OpTerm -> "term"
  OpType -> "type"
  OpCtor -> "ctor"
  OpDoc -> "doc"
  OpTest -> "test"
  OpAbility -> "ability"

nameMatcherToken :: NameMatcher -> Text
nameMatcherToken = \case
  ExactName t -> "=" <> t
  Prefix t -> t <> "*"
  Suffix t -> "*" <> t
  Contains t -> "*" <> t <> "*"
  Glob g -> g

-- =============================================================================
-- Parser
-- =============================================================================

type Parser = Parsec Void Text

type PatternParseError = M.ParseErrorBundle Text Void

-- | Parse a pattern string. Returns 'Left' with a structured error on
-- syntax failure; pattern matching downstream is total.
parsePattern :: Text -> Either PatternParseError Pattern
parsePattern = M.parse (sc *> patternP <* M.eof) "<pattern>"

-- Lexer helpers
sc :: Parser ()
sc = L.space MC.space1 M.empty M.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser ()
reserved word = lexeme (M.try (MC.string' word *> M.notFollowedBy MC.alphaNumChar))

-- Top-level: OR is the lowest precedence.
patternP :: Parser Pattern
patternP = orExpr <?> "pattern"

orExpr :: Parser Pattern
orExpr = do
  first <- andExpr
  rest <- M.many (reserved "OR" *> andExpr)
  pure $ foldl Or first rest

andExpr :: Parser Pattern
andExpr = do
  first <- notExpr
  rest <- M.many (reserved "AND" *> notExpr)
  pure $ foldl And first rest

notExpr :: Parser Pattern
notExpr =
  (reserved "NOT" *> (Not <$> notExpr))
    <|> atom

atom :: Parser Pattern
atom =
  parenthesized
    <|> M.try (reserved "ANYTHING" $> Anything)
    <|> M.try (reserved "NOTHING" $> Nothing')
    <|> M.try predicate
    <|> bareTokenP

-- | Bare identifier — interpreted as a name matcher; @Order@ is treated
-- as @name:Order@ with contains-matching semantics. Reserved Boolean
-- keywords @AND@ @OR@ @NOT@ @ANYTHING@ @NOTHING@ are not eligible.
bareTokenP :: Parser Pattern
bareTokenP = do
  raw <- identifierText
  if raw `elem` reservedWords
    then M.failure Nothing mempty
    else pure (NameP (classifyNameMatcher raw))

reservedWords :: [Text]
reservedWords = ["AND", "OR", "NOT", "ANYTHING", "NOTHING"]

parenthesized :: Parser Pattern
parenthesized = M.between (symbol "(") (symbol ")") patternP

predicate :: Parser Pattern
predicate =
  M.choice
    [ M.try (kindP <* sc),
      M.try (nameP <* sc),
      M.try (projectP <* sc),
      ownerP <* sc
    ]
    <?> "predicate"

kindP :: Parser Pattern
kindP = do
  _ <- lexeme (MC.string' "kind") *> lexeme (MC.char ':')
  k <-
    M.choice
      [ MC.string' "term" $> OpTerm,
        MC.string' "type" $> OpType,
        MC.string' "ctor" $> OpCtor,
        MC.string' "doc" $> OpDoc,
        MC.string' "test" $> OpTest,
        MC.string' "ability" $> OpAbility
      ]
  pure (Kind k)

nameP :: Parser Pattern
nameP = do
  _ <- lexeme (MC.string' "name") *> lexeme (MC.char ':')
  raw <- identifierText
  pure (NameP (classifyNameMatcher raw))

projectP :: Parser Pattern
projectP = do
  _ <- lexeme (MC.string' "project") *> lexeme (MC.char ':')
  raw <- identifierText
  pure (Project raw)

ownerP :: Parser Pattern
ownerP = do
  _ <- lexeme (MC.string' "owner") *> lexeme (MC.char ':')
  raw <- identifierText
  pure (Owner raw)

-- | One unbroken run of identifier-ish characters. Used for project / owner
-- names and as the raw text from which 'NameMatcher' is classified.
--
-- Allows letters, digits, @_@, @.@, @-@, @/@, @@@, @*@, @?@ — enough to
-- cover Unison names like @lib.unison_base_1_0_0.data.List@ and globs.
identifierText :: Parser Text
identifierText =
  lexeme (Text.pack <$> M.some (M.satisfy isIdentChar) <?> "identifier")
  where
    isIdentChar c =
      c == '_'
        || c == '.'
        || c == '-'
        || c == '/'
        || c == '@'
        || c == '*'
        || c == '?'
        || c == '='
        || c `elem` ['a' .. 'z']
        || c `elem` ['A' .. 'Z']
        || c `elem` ['0' .. '9']

-- | Classify an identifier-shaped string into the appropriate 'NameMatcher'.
--
-- * Leading @=@ → 'ExactName' (rest of the string)
-- * Contains @*@ or @?@ → 'Glob' (full text used as-is)
-- * Otherwise → 'Contains' — a bare token like @Plan@ matches anything
--   whose name contains @Plan@ (the conventional substring-find semantics).
classifyNameMatcher :: Text -> NameMatcher
classifyNameMatcher t
  | Just rest <- Text.stripPrefix "=" t = ExactName rest
  | Text.any (\c -> c == '*' || c == '?') t = Glob t
  | otherwise = Contains t
