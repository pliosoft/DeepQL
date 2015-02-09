{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

-- | Parsers for Database.DQL syntax types.

module Database.DQL.Parsers where

import Database.DQL.Combinators
import Database.DQL.Types.Syntax
import Control.Applicative
import Text.Parsec.Perm

schemap = do nodes <- many1 nodep
             return $ Schema nodes

nodep = do _      <- string "node"
           iden   <- identifier
           fields <- between lbrace rbrace (many1 fieldp)
           return $ Node iden fields

fieldp = do (k,on,hid) <- (permute ((,,) <$?> (Unindexed, Indexed  <$ at)
                                         <|?> (Required,  Optional <$ question)
                                         <|?> (Visible,   Hidden   <$ period)))
            name <- identifier
            colon
            t    <- typep
            return $ Field k on hid name t


typep = (string "String"  *> pure StringT)
    <|> (string "Integer" *> pure IntegerT)
    <|> (ObjT  <$> between lbrace rbrace (many fieldp))
    <|> (ListT <$> between lbracket rbracket typep)
    <|> (UserT <$> identifier)

