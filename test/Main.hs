{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Test suite for Database.DQL.

module Main where

import Database.DQL.Combinators
import Database.DQL.Parsers
--import Database.DQL.Pretty
import Database.DQL.Tokenizer (tokenize)
import Database.DQL.Types.Syntax
import Database.DQL.Types.Tokens

import Control.Applicative
import Control.Monad hiding (ap)
import Data.Bifunctor
import Data.Text (Text)
import Test.Hspec
import Text.Parsec.Prim

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = do
  describe "tokenizer" tokenizer
  describe "parser"    parser
  describe "printer"   printer

-- | Tests for the tokenizer.
tokenizer :: Spec
tokenizer =
  do it "makes a single identifier into a token"
        (tokenize "identifier" == Right [Identifier (1,1) "identifier"])
     it "does not produces tokens for the empty string"
        (tokenize "" == Right [])
     it "period"
        (tokenize "." == Right [Period (1,1)])
     it "colon"
        (tokenize ":" == Right [Colon (1,1)])
     it "at"
        (tokenize "@" == Right [At (1,1)])
     it "colon"
        (tokenize "," == Right [Comma (1,1)])
     it "lbracket"
        (tokenize "[" == Right [LeftBracket (1,1)])
     it "rbracket"
        (tokenize "]" == Right [RightBracket(1,1)])
     it "langle"
        (tokenize "<" == Right [LeftAngle (1,1)])
     it "rangle"
        (tokenize ">" == Right [RightAngle (1,1)])
     it "lbrace"
        (tokenize "{" == Right [LeftBrace (1,1)])
     it "rbrace"
        (tokenize "}" == Right [RightBrace (1,1)])
     it "question mark"
        (tokenize "?" == Right [QuestionMark (1,1)])
     it "quotation"
        (tokenize "\"foo\"" == Right [QuotedString (1,1) "foo"])
     it "seqeuence of identifiers"
        (tokenize "foo bar" == Right [Identifier (1,1) "foo",Identifier (1,5) "bar"])
     it "sequence of identifiers with newline"
        (tokenize "foo\nbar" == Right [Identifier (1,1) "foo", Identifier (2,1) "bar"])
     it "does not parse comments"
        (tokenize "#bar\nfoo" == Right [Identifier (2,1) "foo"])
     it "tokenizes a larger program"
        (tokenize "node foo { field name : String }"
         == Right  [ Identifier (1, 1) "node"
                   , Identifier (1, 6) "foo"
                   , LeftBrace  (1,10)
                   , Identifier (1,12) "field"
                   , Identifier (1,18) "name"
                   , Colon      (1,23)
                   , Identifier (1,25) "String"
                   , RightBrace (1,32)
                   ])

parser :: Spec
parser = do
   it "a simple node"
       (parsed nodep "node foo { name : String }"
      == (Right $ Node "foo" [ Field Unindexed Required Visible "name" StringT ]))
   it "multiple fields"
       (parsed nodep "node foo { name : Bar \n baz : String }"
      == (Right $ Node "foo" [ Field Unindexed Required Visible "name" (UserT "Bar"),
                               Field Unindexed Required Visible "baz" StringT ]))
   it "an optional field"
       (parsed nodep "node foo { ?name : String }"
      == (Right $ Node "foo" [ Field Unindexed Optional Visible "name" StringT ]))
   it "an indexed field"
       (parsed nodep "node foo { @name : String }"
      == (Right $ Node "foo" [ Field Indexed Required Visible "name" StringT ]))
   it "an hidden field"
       (parsed nodep "node foo { .name : String }"
      == (Right $ Node "foo" [ Field Unindexed Required Hidden "name" StringT ]))
   it "a hidden optional field"
       (parsed nodep "node foo { ?.name : String }"
      == (Right $ Node "foo" [ Field Unindexed Optional Hidden "name" StringT ]))
   it "a hidden optional field (permuted)"
       (parsed nodep "node foo { .?name : String }"
      == (Right $ Node "foo" [ Field Unindexed Optional Hidden "name" StringT ]))
   it "a user type"
       (parsed nodep "node foo { name : Bar }"
      == (Right $ Node "foo" [ Field Unindexed Required Visible "name" (UserT "Bar") ]))
   it "an obj type"
       (parsed nodep "node foo { name : {fiz : Bar\nbiz: Integer} }"
      == (Right $ Node "foo" [ Field Unindexed Required Visible "name"
                                 (ObjT [Field Unindexed Required Visible "fiz" (UserT "Bar"),
                                        Field Unindexed Required Visible "biz" IntegerT]) ]))
   it "a list type"
       (parsed nodep "node foo { name : [Bar] }"
      == (Right $ Node "foo" [ Field Unindexed Required Visible "name" (ListT (UserT "Bar")) ]))

printer :: Spec
printer = it "works" True

-- | Is that left?
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- | Get the parsed result after tokenizing.
parsed :: Parsec [Token] () c -> Text -> Either String c
parsed p = tokenize >=> bimap show id . runParser (p <* eof) () "<testcase>"

