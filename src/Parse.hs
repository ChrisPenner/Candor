{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Parse where

import Data.Functor.Foldable
import RIO hiding (first, many, some, try)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as MP

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import Data.Bifunctor

type Parser = Parsec Void String

spacers :: Parser ()
spacers = space <|> void (char ',')

symbol :: String -> Parser String
symbol = L.symbol spacers

expression :: Parser AST
expression =
  appl <|> stringLiteral <|> try numberLiteral <|> try boolLiteral <|>
  try recLiteral <|>
  symbolLiteral <|>
  listLiteral <|>
  funcLiteral

boolLiteral :: Parser (AST)
boolLiteral = do
  s <- symbolLexeme
  case s of
    "T" -> return $ Fix $ Boolean True
    "F" -> return $ Fix $ Boolean False
    _ -> empty

recLiteral :: Parser AST
recLiteral = do
  s <- symbolLexeme
  case s of
    "$" -> return $ (Fix Rec)
    _ -> empty

stringLiteral :: Parser (AST)
stringLiteral =
  Fix . Str <$> L.lexeme spacers (char '"' *> manyTill L.charLiteral (char '"'))

numberLiteral :: Parser (AST)
numberLiteral =
  Fix . Number <$> do
    sign <- optional $ char '-'
    n <- L.lexeme spacers L.decimal
    return $
      case sign of
        Just _ -> negate n
        Nothing -> n

symbolLexeme :: Parser String
symbolLexeme = L.lexeme spacers (some (noneOf ("\t\n\r ()[]{}<>:," :: String)))

symbolLiteral :: Parser (AST)
symbolLiteral = Fix . Symbol <$> symbolLexeme

listLiteral :: Parser AST
listLiteral = Fix . List <$> list

-- singleBinding :: Parser AST
-- singleBinding = do
--   between (symbol "(") (symbol ")") $ do
--     _ <- L.lexeme spacers (char '=')
--     _ <- char ':'
--     bindingName <- symbolLexeme
--     expr <- expression
--     return (Fix $ Binding bindingName expr)
funcLiteral :: Parser (AST)
funcLiteral =
  L.lexeme spacers . between (symbol "{") (symbol "}") $ do
    arg <- symbolLexeme
    expr <- expression
    return (Fix $ FuncDef arg expr)

appl :: Parser (AST)
appl = do
  (f, args) <-
    between (symbol "(") (symbol ")") $ do
      f <- expression
      args <- expression `sepBy1` spacers
      return (f, args)
  return $ foldl' go f args
  where
    go :: AST -> AST -> AST
    go f arg = Fix (Appl f arg)

list :: Parser [AST]
list = between (symbol "[") (symbol "]") (many expression)

parse :: String -> Either String (AST)
parse s = first (parseErrorPretty' s) $ MP.parse (expression <* eof) "" s
