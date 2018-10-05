{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Parse where

import Data.Functor.Foldable
import RIO hiding (first, many, some, try)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as MP

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Bifunctor
import ParseAST

type Parser = Parsec Void String

spacers :: Parser ()
spacers = space <|> void (char ',')

symbol :: String -> Parser String
symbol = L.symbol spacers

expression :: Parser ParseAST
expression =
  try singleBinding <|> appl <|> stringLiteral <|> try numberLiteral <|>
  try boolLiteral <|>
  bindSymbol <|>
  symbolLiteral <|>
  listLiteral <|>
  -- try recordLiteral <|>
  funcLiteral

boolLiteral :: Parser (ParseAST)
boolLiteral = do
  s <- symbolLexeme
  case s of
    "T" -> return $ Fix $ Boolean True
    "F" -> return $ Fix $ Boolean False
    _ -> empty

stringLiteral :: Parser (ParseAST)
stringLiteral =
  Fix . Str <$> L.lexeme spacers (char '"' *> manyTill L.charLiteral (char '"'))

numberLiteral :: Parser (ParseAST)
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

symbolLiteral :: Parser (ParseAST)
symbolLiteral = Fix . Symbol <$> symbolLexeme

bindSymbol :: Parser ParseAST
bindSymbol =
  Fix . BindingSymbol <$> do
    _ <- char ':'
    symbolLexeme

listLiteral :: Parser ParseAST
listLiteral = Fix . List <$> list

singleBinding :: Parser ParseAST
singleBinding = do
  between (symbol "(") (symbol ")") $ do
    _ <- L.lexeme spacers (char '=')
    _ <- char ':'
    bindingName <- symbolLexeme
    expr <- expression
    return (Fix $ Binding bindingName expr)

-- recordLiteral :: Parser ParseAST
-- recordLiteral =
--   Fix . Bindings . M.fromList <$> do
--     between (symbol "{") (symbol "}") . many $ do
--       key <- symbolLexeme
--       _ <- symbol ":"
--       expr <- expression
--       _ <- symbol ","
--       return (key, expr)
funcLiteral :: Parser (ParseAST)
funcLiteral =
  L.lexeme spacers . between (symbol "{") (symbol "}") $ do
    arg <- symbolLexeme
    expr <- expression
    return (Fix $ FuncDef arg expr)

appl :: Parser (ParseAST)
appl = do
  (f, args) <-
    between (symbol "(") (symbol ")") $ do
      f <- expression
      args <- expression `sepBy1` spacers
      return (f, args)
  return $ foldl' go f args
  where
    go :: ParseAST -> ParseAST -> ParseAST
    go f arg = Fix (Appl f arg)

list :: Parser [ParseAST]
list = between (symbol "[") (symbol "]") (many expression)

parse :: String -> Either String (ParseAST)
parse s = first (parseErrorPretty' s) $ MP.parse (expression <* eof) "" s
