{-# LANGUAGE FlexibleContexts #-}

-- | Parser combinators.
module Database.DQL.Combinators where

   import           Database.DQL.Types.Tokens

   import           Data.Text (Text)
   import qualified Data.Text as T
   import           Text.Parsec.Pos
   import           Text.Parsec.Prim

   -- | Match a word with the given string.
   string :: Stream s m Token => Text -> ParsecT s u m Text
   string s = satisfy
      (\t ->
         case t of
            Identifier _ t' -> if t' == s then Just t' else Nothing
            _ -> Nothing)


   -- | Match a word with the given string.
   identifier :: Stream s m Token => ParsecT s u m Text
   identifier = satisfy
      (\t ->
         case t of
            Identifier _ t' -> Just t'
            _ -> Nothing)




   -- | Quoted string.
   quoted :: Stream s m Token => ParsecT s u m Text
   quoted = satisfy
      (\t ->
         case t of
            QuotedString _ t' -> Just t'
            _ -> Nothing)

   -- | An at.
   at :: Stream s m Token => ParsecT s u m ()
   at = satisfy
      (\t ->
         case t of
            At _ -> Just ()
            _ -> Nothing)


   -- | A comma.
   comma :: Stream s m Token => ParsecT s u m ()
   comma = satisfy
      (\t ->
         case t of
            Comma _ -> Just ()
            _ -> Nothing)


   -- | A colon.
   colon :: Stream s m Token => ParsecT s u m ()
   colon = satisfy
      (\t ->
         case t of
            Colon _ -> Just ()
            _ -> Nothing)

   -- | A period.
   period :: Stream s m Token => ParsecT s u m ()
   period = satisfy
      (\t ->
         case t of
            Period _ -> Just ()
            _ -> Nothing)

   -- | A question mark.
   question :: Stream s m Token => ParsecT s u m ()
   question = satisfy
      (\t ->
         case t of
            QuestionMark _ -> Just ()
            _ -> Nothing)


   -- | right bracket.
   rbracket :: Stream s m Token => ParsecT s u m ()
   rbracket = satisfy
      (\t ->
         case t of
            RightBracket _ -> Just ()
            _ -> Nothing)

   -- | left bracket..
   lbracket :: Stream s m Token => ParsecT s u m ()
   lbracket = satisfy
      (\t ->
         case t of
            LeftBracket _ -> Just ()
            _ -> Nothing)

   -- | right brace.
   rbrace :: Stream s m Token => ParsecT s u m ()
   rbrace = satisfy
      (\t ->
         case t of
            RightBrace _ -> Just ()
            _ -> Nothing)

   -- | left brace.
   lbrace :: Stream s m Token => ParsecT s u m ()
   lbrace = satisfy
      (\t ->
         case t of
            LeftBrace _ -> Just ()
            _ -> Nothing)

   -- | angle bracket.
   rangle :: Stream s m Token => ParsecT s u m ()
   rangle = satisfy
      (\t ->
         case t of
            RightAngle _ -> Just ()
            _ -> Nothing)

   -- | angle bracket.
   langle :: Stream s m Token => ParsecT s u m ()
   langle = satisfy
      (\t ->
         case t of
            LeftAngle _ -> Just ()
            _ -> Nothing)


   -- | @between open close p@ parses @open@, followed by @p@ and @close@.
   -- Returns the value returned by @p@.
   --
   -- >  braces  = between (symbol "{") (symbol "}")

   between :: (Stream s m t) => ParsecT s u m open -> ParsecT s u m close
               -> ParsecT s u m a -> ParsecT s u m a
   between open close p
                     = do{ _ <- open; x <- p; _ <- close; return x }


   -- | Try to match all the given strings, or none at all.
   strings :: Stream s m Token => [Text] -> ParsecT s u m ()
   strings ss = try (mapM_ string ss)

   -- | Satisfy the given predicate from the token stream.
   satisfy :: Stream s m Token => (Token -> Maybe a) -> ParsecT s u m a
   satisfy = tokenPrim tokenString tokenPosition

   -- | The parser @anyToken@ accepts any kind of token. It is for example
   -- used to implement 'eof'. Returns the accepted token.
   anyToken :: (Stream s m Token) => ParsecT s u m Token
   anyToken = satisfy Just

   -- | Make a string out of the token, for error message purposes.
   tokenString :: Token -> String
   tokenString t = case t of
                        Identifier _ w -> "identifier \"" ++ T.unpack w ++ "\""
                        QuotedString _ s -> "quotation \"" ++ T.unpack s ++ "\""
                        Period{} -> "period"
                        Comma{} -> "comma"
                        Colon{} -> "colon"
                        At{} -> "at symbol"
                        LeftBracket{} -> "left bracket"
                        RightBracket{} -> "right bracket"
                        LeftBrace{} -> "left brace"
                        RightBrace{} -> "right brace"
                        LeftAngle{} -> "left angle bracket"
                        RightAngle{} -> "right angle bracket"
                        QuestionMark{} -> "question mark"

   -- | Update the position by the token.
   tokenPosition :: SourcePos -> Token -> t -> SourcePos
   tokenPosition pos t _ = setSourceColumn (setSourceLine pos line) col
      where (line,col) = tokenPos t

   -- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
   -- does not consume any input. This parser can be used to implement the
   -- \'longest match\' rule. For example, when recognizing keywords (for
   -- example @let@), we want to make sure that a keyword is not followed
   -- by a legal identifier character, in which case the keyword is
   -- actually an identifier (for example @lets@). We can program this
   -- behaviour as follows:
   --
   -- >  keywordLet  = try (do{ string "let"
   -- >                       ; notFollowedBy alphaNum
   -- >                       })
   notFollowedBy :: (Stream s m Token) => ParsecT s u m Token -> ParsecT s u m ()
   notFollowedBy p = try ((do c <- try p
                              unexpected (tokenString c)) <|>
                           return ())

   many1 :: (Stream s m Token) => ParsecT s u m a -> ParsecT s u m [a]
   many1 p             = do{ x <- p; xs <- many p; return (x:xs) }

   -- | This parser only succeeds at the end of the input. This is not a
   -- primitive parser but it is defined using 'notFollowedBy'.
   eof :: (Stream s m Token) => ParsecT s u m ()
   eof = notFollowedBy anyToken <?> "end of input"

