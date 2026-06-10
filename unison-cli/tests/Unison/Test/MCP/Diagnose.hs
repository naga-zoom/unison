{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure JSON projection in 'Unison.MCP.Tools.Diagnose'.
--
-- The branch-scanning logic is exercised end-to-end via integration smoke
-- (the Structural primitive's analyzer is itself tested separately). Here
-- we verify the wire format — that each 'StructuralIssue' constructor
-- renders with the expected JSON shape and 'kind' discriminator.
module Unison.Test.MCP.Diagnose where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Set qualified as Set
import Data.Text (Text)
import EasyTest
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.MCP.Domain.Structural (StructuralIssue (..))
import Unison.MCP.Tools.Diagnose (issueToJson)
import Unison.Name (Name)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.Name qualified as Name

test :: Test ()
test =
  scope "mcp.diagnose" . tests $
    [ scope "orphan_type kind discriminator" orphanCase,
      scope "misplaced_ctor kind discriminator" misplacedCase
    ]

orphanCase :: Test ()
orphanCase =
  let issue =
        OrphanType
          { typeRef = Reference.Builtin "fixtureType",
            typeNames = Set.fromList [n "foo.Bar"],
            declaredCtors = 3,
            namedCtors = 1
          }
      j = issueToJson issue
   in tests
        [ expectEqual (lookupString "kind" j) (Just "orphan_type"),
          expectEqual (lookupInt "declaredCtors" j) (Just 3),
          expectEqual (lookupInt "namedCtors" j) (Just 1)
        ]

misplacedCase :: Test ()
misplacedCase =
  let issue =
        MisplacedConstructor
          { ctorName = n "baz.Wrong",
            ctorRef = Referent.Con (ConstructorReference (Reference.Builtin "fixtureType") 0) CT.Data,
            typeRef = Reference.Builtin "fixtureType",
            typeNames = Set.fromList [n "foo.Bar"]
          }
      j = issueToJson issue
   in tests
        [ expectEqual (lookupString "kind" j) (Just "misplaced_ctor"),
          expectEqual (lookupString "ctorName" j) (Just "baz.Wrong")
        ]

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

lookupString :: Text -> Aeson.Value -> Maybe Text
lookupString k v = case v of
  Aeson.Object km -> case AesonKM.lookup (AesonKey.fromText k) km of
    Just (Aeson.String s) -> Just s
    _ -> Nothing
  _ -> Nothing

lookupInt :: Text -> Aeson.Value -> Maybe Int
lookupInt k v = case v of
  Aeson.Object km -> case AesonKM.lookup (AesonKey.fromText k) km of
    Just (Aeson.Number n') -> Just (floor n')
    _ -> Nothing
  _ -> Nothing

n :: Text -> Name
n t = case Name.parseTextEither t of
  Right name -> name
  Left e -> error ("fixture name failed to parse: " <> show e)
