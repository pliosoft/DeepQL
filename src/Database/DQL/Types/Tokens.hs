-- | Tokens for DQL Schema
module Database.DQL.Types.Tokens(Token(..), tokenPos) where
   import Data.Text (Text)

   -- | A token
   data Token
      = Identifier   !(Int, Int) !Text
      | QuotedString !(Int, Int) !Text
      | Period       !(Int, Int)
      | Comma        !(Int, Int)
      | At           !(Int, Int)
      | Colon        !(Int, Int)
      | QuestionMark !(Int, Int)
      | LeftBracket  !(Int, Int)
      | RightBracket !(Int, Int)
      | LeftAngle    !(Int, Int)
      | RightAngle   !(Int, Int)
      | LeftBrace    !(Int, Int)
      | RightBrace   !(Int, Int)
         deriving (Eq,Show)

   -- | Get the position of the token.
   tokenPos :: Token -> (Int, Int)
   tokenPos t = case t of
      Identifier pos _   -> pos
      QuotedString pos _ -> pos
      Period pos         -> pos
      Comma pos          -> pos
      At pos             -> pos
      Colon pos          -> pos
      QuestionMark pos   -> pos
      LeftAngle pos      -> pos
      RightAngle pos     -> pos
      LeftBracket pos    -> pos
      RightBracket pos   -> pos
      LeftBrace pos      -> pos
      RightBrace pos     -> pos
