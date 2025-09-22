{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.Parser.Literal (
    bool,
    text,
    numeric,
    literal,
)
where

import Text.Megaparsec

import Cacco.Syntax.Literal
import Cacco.Syntax.Parser.Internal (Parser)
import Cacco.Syntax.Parser.Lexer
import Cacco.Syntax.Parser.Numeric

bool :: Parser Literal
{- ^ Parse a boolean literal

>>> parseTest bool "true"
Bool True

>>> parseTest bool "false"
Bool False
-}
bool = Bool <$> (true <|> false) <?> "boolean literal: true or false"

--
undef :: Parser Literal
undef = reserved "undefined" >> return Undef <?> "undefined"

text :: Parser Literal
text = Text <$> stringLiteral

literal :: Parser Literal
literal = undef <|> bool <|> text <|> numeric
