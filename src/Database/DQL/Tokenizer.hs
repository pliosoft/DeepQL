{-# LANGUAGE BangPatterns #-}
-- | Tokenizer for DQL
module Database.DQL.Tokenizer where
   import           Prelude hiding (takeWhile)
   import           Database.DQL.Types.Tokens
   import           Control.Applicative
   import           Control.Arrow
   import           Control.Monad
   import           Data.Attoparsec.Text hiding (number)
   import           Data.Char
   import           Data.Text (Text)
   import qualified Data.Text as T

   type Position = (Int, Int)

   tokenize :: Text -> Either String [Token]
   tokenize t = parseOnly (fmap fst tokenizer <* spaces (0,1) <* endOfInput) t

   tokenizer :: Parser ([Token], Position)
   tokenizer = manyWithPos (spaces >=> token) (1,1)

   -- | Parse a token.
   token :: Position -> Parser (Token, Position)
   token pos = quotedString pos
           <|> period       pos
           <|> comma        pos
           <|> at           pos
           <|> colon        pos
           <|> lbrace       pos
           <|> rbrace       pos
           <|> lbracket     pos
           <|> rbracket     pos
           <|> langle       pos
           <|> rangle       pos
           <|> questionMark pos
           <|> identifier   pos


   -- | Parse a quoted string, @\"fizbuz\"@.
   quotedString :: Position -> Parser (Token, Position)
   quotedString pos = char '"' *> (cons <$> takeWhile1 (/= '"')) <* char '"'
     where
         cons x = (QuotedString pos x, second (+ (fromIntegral $ T.length x + 2)) pos)

   -- | Helper that takes a simple constructor and a position, and constructs the appropriate result
   pret :: (Position -> Token) -> Position -> Parser (Token, Position)
   pret x pos = pure (x pos, second (+1) pos)

   -- | Parse a period \".\".
   period :: Position -> Parser (Token, Position)
   period pos = char '.' *> pret Period pos

   -- | Parse an at sign \"@\".
   at :: Position -> Parser (Token, Position)
   at pos = char '@' *> pret At pos

   -- | Parse a comma \",\".
   comma :: Position -> Parser (Token, Position)
   comma pos = char ',' *> pret Comma pos

   -- | Parse a colon \":\".
   colon :: Position -> Parser (Token, Position)
   colon pos = char ':' *> pret Colon pos

   -- | Parse a left bracket \"[\".
   lbracket :: Position -> Parser (Token, Position)
   lbracket pos = char '[' *> pret LeftBracket pos

   -- | Parse a right bracket \"]\".
   rbracket :: Position -> Parser (Token, Position)
   rbracket pos = char ']' *> pret RightBracket pos

   -- | Parse a left brace \"{\".
   lbrace :: Position -> Parser (Token, Position)
   lbrace pos = char '{' *> pret LeftBrace pos

   -- | Parse a right brace \"}\".
   rbrace :: Position -> Parser (Token, Position)
   rbrace pos = char '}' *> pret RightBrace pos

   -- | Parse a left angle bracket \"<\".
   langle :: Position -> Parser (Token, Position)
   langle pos = char '<' *> pret LeftAngle pos

   -- | Parse a right angle bracket \">\".
   rangle :: Position -> Parser (Token, Position)
   rangle pos = char '>' *> pret RightAngle pos

   -- | Parse a question mark \"?\".
   questionMark :: Position -> Parser (Token, Position)
   questionMark pos = char '?' *> pret QuestionMark pos

   -- | An identifier is an alpha, followed by alphanumeric
   identifier :: Position -> Parser (Token, Position)
   identifier pos = cons <$> takeWhile1 wordChar
     where
      cons w = (Identifier pos w, second (+ (fromIntegral $ T.length w)) pos)
      wordChar c = 
               not (isSpace c) &&
               c /= '"' &&
               c /= '.' &&
               c /= ',' &&
               c /= ':' &&
               c /= '?' &&
               c /= '@' &&
               c /= '{' &&
               c /= '}' &&
               c /= '[' &&
               c /= ']' &&
               c /= '<' &&
               c /= '>'


   -- | Like 'many', but retains the current source position
   manyWithPos :: (Position -> Parser ( a,  Position)) 
               ->  Position -> Parser ([a], Position)
   manyWithPos p pos = do r <- fmap (first Just) (p pos) <|> pure (Nothing, pos)
                          case r of
                           (Nothing, _) ->
                              return ([], pos)
                           (Just x, newpos@(!_, !_)) ->
                                    do (xs, finalpos) <- manyWithPos p newpos
                                       return (x:xs, finalpos)


   -- | Skip spaces (space, newline, tab (=4 spaces)) and keep
   -- positioning information up to date.
   spaces :: Position -> Parser Position
   spaces (sline, scol) = go sline scol
    where
      go line col =
         do c <- peekChar
            case c of
               Just '\n' -> anyChar *> go (line+1) 1
               Just ' '  -> anyChar *> go line (col+1)
               Just '#'  -> anyChar *> takeWhile1 (/='\n') *> go line (col+1)
               Just '\t' -> anyChar *> go line (col+4)
               _ -> return (line, col)
