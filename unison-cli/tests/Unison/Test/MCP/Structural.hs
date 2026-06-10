{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure analyzer in 'Unison.MCP.Domain.Structural'.
--
-- The IO-bound 'scanBranch' is exercised end-to-end via integration smoke.
-- Here we verify the algebra of 'analyzeType' on hand-built 'TypeInfo'.
module Unison.Test.MCP.Structural where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64)
import EasyTest
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.MCP.Domain.Structural
  ( StructuralIssue (..),
    TypeInfo (..),
    analyzeType,
  )
import Unison.Name (Name)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.Name qualified as Name

test :: Test ()
test =
  scope "mcp.structural" . tests $
    [ scope "well-formed type produces no issues" wellFormed,
      scope "type with fewer named ctors than declared → OrphanType" orphan,
      scope "ctor outside type's namespace → MisplacedConstructor" misplaced,
      scope "type with multiple names — ctor under any name is fine" multipleNames,
      scope "combined: orphan + misplaced" combined
    ]

-- ----------------------------------------------------------------------------
-- Cases
-- ----------------------------------------------------------------------------

wellFormed :: Test ()
wellFormed =
  let info =
        TypeInfo
          { typeRef = synthType "fixtureType",
            typeNames = Set.fromList [n "foo.Bar"],
            declaredCtors = 2,
            constructorsByName =
              [ (n "foo.Bar.X", synthCtor "fixtureType" 0),
                (n "foo.Bar.Y", synthCtor "fixtureType" 1)
              ]
          }
   in expectEqual (analyzeType info) []

orphan :: Test ()
orphan =
  let info =
        TypeInfo
          { typeRef = synthType "fixtureType",
            typeNames = Set.fromList [n "foo.Bar"],
            declaredCtors = 3,
            constructorsByName =
              [ (n "foo.Bar.X", synthCtor "fixtureType" 0)
              ]
          }
      issues = analyzeType info
   in case issues of
        [OrphanType {declaredCtors = 3, namedCtors = 1}] -> ok
        other -> crash ("expected one OrphanType (3 declared, 1 named); got " ++ show other)

misplaced :: Test ()
misplaced =
  let info =
        TypeInfo
          { typeRef = synthType "fixtureType",
            typeNames = Set.fromList [n "foo.Bar"],
            declaredCtors = 2,
            constructorsByName =
              [ (n "foo.Bar.X", synthCtor "fixtureType" 0),
                (n "baz.Wrong", synthCtor "fixtureType" 1)
              ]
          }
      issues = analyzeType info
   in case issues of
        [MisplacedConstructor {ctorName}] | nameText ctorName == "baz.Wrong" -> ok
        other -> crash ("expected one MisplacedConstructor for baz.Wrong; got " ++ show other)

multipleNames :: Test ()
multipleNames =
  -- Type has two names: foo.Bar and aliased.lib.foo.Bar.
  -- A constructor under EITHER namespace counts as well-placed.
  let info =
        TypeInfo
          { typeRef = synthType "fixtureType",
            typeNames = Set.fromList [n "foo.Bar", n "aliased.lib.foo.Bar"],
            declaredCtors = 2,
            constructorsByName =
              [ (n "foo.Bar.X", synthCtor "fixtureType" 0),
                (n "aliased.lib.foo.Bar.Y", synthCtor "fixtureType" 1)
              ]
          }
   in expectEqual (analyzeType info) []

combined :: Test ()
combined =
  -- Declared 3 ctors, only 2 named, and one of those is misplaced.
  let info =
        TypeInfo
          { typeRef = synthType "fixtureType",
            typeNames = Set.fromList [n "foo.Bar"],
            declaredCtors = 3,
            constructorsByName =
              [ (n "foo.Bar.X", synthCtor "fixtureType" 0),
                (n "baz.Stray", synthCtor "fixtureType" 1)
              ]
          }
      issues = analyzeType info
   in case issues of
        [OrphanType {}, MisplacedConstructor {ctorName}] | nameText ctorName == "baz.Stray" -> ok
        other -> crash ("expected OrphanType + MisplacedConstructor; got " ++ show other)

-- ----------------------------------------------------------------------------
-- Fixtures
-- ----------------------------------------------------------------------------

synthType :: Text -> Reference.TypeReference
synthType name = Reference.Builtin name

synthCtor :: Text -> Word64 -> Referent.Referent
synthCtor typeName cid =
  Referent.Con
    (ConstructorReference (synthType typeName) cid)
    (error "ctor type not inspected in these tests")

n :: Text -> Name
n t = case Name.parseTextEither t of
  Right name -> name
  Left e -> error ("fixture name failed to parse: " <> show e)

nameText :: Name -> Text
nameText = Name.toText
