-- | Types for the syntax tree.

module Database.DQL.Types.Syntax where

   import Data.Text (Text)
   import Prelude hiding (String)

   data Schema = Schema [Node]
    deriving (Show,Eq)

   data Node = Node Text [Field]
    deriving (Show,Eq)

   data Index = Unindexed | Indexed
    deriving (Show,Eq)

   data Presence = Required | Optional
    deriving (Show,Eq)

   data Visibility = Visible | Hidden
    deriving (Show,Eq)

   data Field = Field Index Presence Visibility Text Type
    deriving (Show,Eq)

   data Type = IntegerT
             | StringT
             | UserT Text
             | ListT Type
             | ObjT  [Field]
    deriving (Show,Eq)


