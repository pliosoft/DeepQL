{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Simple test program that just parses STDIN

module Main where

   import Database.DQL.Combinators
   import Database.DQL.Parsers
   import Database.DQL.Tokenizer (tokenize,spaces)
   import Database.DQL.Types.Syntax
   import Database.DQL.Types.Tokens

   import Data.Bifunctor
   import Control.Applicative hiding (many)
   import Control.Monad hiding (ap)
   import Control.Monad.Identity hiding (ap)
   import Data.Text (Text)
   import Data.Text.Lazy (toStrict)
   import Data.Text.Lazy.Builder (fromText,toLazyText)
   import Data.Text.IO as TIO
   import Text.Parsec (Stream,ParsecT,runParser,try,Parsec,ParseError)
   import Text.Parsec.Prim

   main :: IO ()
   main = do ctn <- TIO.getContents
             case parseSchema ctn of
               Left err -> Prelude.putStrLn err
               Right res -> Prelude.putStrLn $ show res


   parseSchema :: Text -> Either String Schema
   parseSchema = tokenize >=> 
                 bimap show id .
                 runParser (schemap <* eof) () "<stdin>"

