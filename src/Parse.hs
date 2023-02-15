{-# LANGUAGE OverloadedLists #-}

module Parse where

import AST
import Data.Bifunctor
import Data.List.NonEmpty as NE (fromList)
import qualified Data.Map as M
import RIO hiding (first, many, some, try)
import qualified RIO.Text as Text
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

spacers :: Parser ()
spacers = space <|> void (char ',')

symbol :: String -> Parser String
symbol = L.symbol spacers

expression :: Parser AST
expression =
  appl <|> stringLiteral <|> try numberLiteral <|> try boolLiteral
    <|> symbolLiteral
    <|> listLiteral
    <|> try recordLiteral
    <|> funcLiteral

boolLiteral :: Parser AST
boolLiteral = do
  s <- symbolLexeme
  case s of
    "T" -> return $ Boolean True
    "F" -> return $ Boolean False
    _ -> empty

stringLiteral :: Parser AST
stringLiteral =
  Str . Text.pack <$> L.lexeme spacers (char '"' *> manyTill L.charLiteral (char '"'))

numberLiteral :: Parser AST
numberLiteral =
  Number
    <$> do
      sign <- optional $ char '-'
      num <- L.lexeme spacers L.decimal
      return $
        case sign of
          Just _ -> negate num
          Nothing -> num

symbolLexeme :: Parser Text
symbolLexeme = Text.pack <$> L.lexeme spacers (some (noneOf ("\t\n\r ()[]{}<>:," :: String)))

symbolLiteral :: Parser AST
symbolLiteral = Symbol <$> symbolLexeme

listLiteral :: Parser AST
listLiteral = List <$> list

-- singleBinding :: Parser AST
-- singleBinding = do
--   between (symbol "(") (symbol ")") $
--     do
--       _ <- L.lexeme spacers (char '=')
--       bindingName <- symbolLexeme
--       expr <- expression
--       return (Bindings [(bindingName, expr)])

recordLiteral :: Parser AST
recordLiteral =
  Bindings . M.fromList
    <$> do
      between (symbol "{") (symbol "}") $ bind `sepBy` symbol ","
  where
    bind = do
      key <- symbolLexeme
      _ <- symbol ":"
      expr <- expression
      return (key, expr)

funcLiteral :: Parser AST
funcLiteral =
  L.lexeme spacers . between (symbol "{") (symbol "}") $
    do
      args <- argList
      expr <- expression
      return (FuncDef args expr)
  where
    argList :: Parser (NonEmpty Text)
    argList =
      NE.fromList
        <$> between (symbol "[") (symbol "]") (symbolLexeme `sepBy1` spacers)

appl :: Parser AST
appl =
  between
    (symbol "(")
    (symbol ")")
    (Appl <$> expression <*> (expression `sepBy1` spacers))

list :: Parser [AST]
list = between (symbol "[") (symbol "]") (many expression)

parse :: String -> Either String AST
parse s = first errorBundlePretty $ MP.parse (expression <* eof) "" s
