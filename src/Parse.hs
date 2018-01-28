module Parse where

import RIO hiding (try, some, first, many)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as MP

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.List.NonEmpty as NE

import AST
import Data.Bifunctor

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

expression :: Parser AST
expression = appl <|> stringLiteral <|> try numberLiteral <|> symbolLiteral <|> funcLiteral <|> listLiteral

stringLiteral :: Parser AST
stringLiteral = Str <$> (char '"' *> manyTill L.charLiteral (char '"'))

numberLiteral :: Parser AST
numberLiteral = Number <$> do
  sign <- optional $ char '-'
  num <- L.lexeme space L.decimal
  return $ case sign of
    Just _ -> negate num
    Nothing -> num

symbolLiteral :: Parser AST
symbolLiteral =
  Symbol <$> L.lexeme space (some (noneOf ("\t\n\r ()[]{}<>" :: String)))

funcLiteral :: Parser AST
funcLiteral =
  between (symbol "{") (symbol "}") $ do
    args <- list
    expr <- expression
    return $ Func args expr

listLiteral :: Parser AST
listLiteral = List <$> list

appl :: Parser AST
appl = Appl . NE.fromList <$> between (symbol "(") (symbol ")") (expression `sepBy1` space)

list :: Parser [AST]
list = between (symbol "[") (symbol "]") (many expression)

parse :: String -> Either String AST
parse s = first (parseErrorPretty' s)  $ MP.parse expression "" s
