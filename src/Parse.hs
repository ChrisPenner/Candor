module Parse where

import RIO hiding (try, some, first, many)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as MP

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.List.NonEmpty as NE

import AST
import ParseType
import Data.Bifunctor

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

typedef :: Parser Type
typedef = between (symbol "<") (symbol ">") typeP

expression :: Parser AST
expression = chunk <|> atom

val :: Parser Val
val = stringLiteral <|> try numberLiteral <|> symbolLiteral <|> funcLiteral <|> listLiteral

symbolString :: Parser String
symbolString = L.lexeme space (some (noneOf ("\t\n\r ()[]{}<>" :: String)))

stringLiteral, numberLiteral, symbolLiteral, funcLiteral, listLiteral :: Parser Val
stringLiteral = Str <$> (char '"' >> manyTill L.charLiteral (char '"'))
numberLiteral = Number <$> do
  sign <- optional $ char '-'
  num <- L.lexeme space L.decimal
  return $ case sign of
    Just _ -> negate num
    Nothing -> num
symbolLiteral = Symbol <$> symbolString
funcLiteral =
  between (symbol "{") (symbol "}") $ do
    args <- list
    expr <- expression
    return $ Func args expr
listLiteral = List <$> list


atom :: Parser AST
atom = Atom <$> val

chunk :: Parser AST
chunk = between (symbol "(") (symbol ")") $ do
  typ <- optional typedef
  case typ of
    Just t -> Typed t <$> appl
    Nothing -> appl

appl :: Parser AST
appl = Appl . NE.fromList <$> (chunk <|> atom) `sepBy1` space

list :: Parser [AST]
list = between (symbol "[") (symbol "]") (many expression)

parse :: String -> Either String AST
parse s = first (parseErrorPretty' s)  $ MP.parse expression "" s
