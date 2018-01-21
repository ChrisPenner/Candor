module Parse where

import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as MP

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.List.NonEmpty as NE

import Data.Void
import Data.Bifunctor
import Data.Maybe
import AST
import Control.Monad
import ParseType
-- import Control.Applicative

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

typedef :: Parser Type
typedef = between (symbol "<") (symbol ">") typeP

expression :: Parser AST
expression = chunk

val :: Parser Val
val = stringLiteral <|> try numberLiteral <|> symbolLiteral

symbolString :: Parser String
symbolString = L.lexeme space (some (noneOf "\t\n\r ()<>"))

stringLiteral, numberLiteral, symbolLiteral :: Parser Val
stringLiteral = Str <$> (char '"' >> manyTill L.charLiteral (char '"'))
numberLiteral = Number <$> do
  sign <- optional $ char '-'
  num <- L.lexeme space L.decimal
  return $ case sign of
    Just _ -> negate num
    Nothing -> num
symbolLiteral = Symbol <$> symbolString

atom :: Parser AST
atom = Atom <$> val

chunk :: Parser AST
chunk = between (symbol "(") (symbol ")") $ do
  typ <- optional typedef
  case typ of
    Just t -> Typed t <$> list
    Nothing -> list

list :: Parser AST
list = List . NE.fromList <$> (chunk <|> decl <|> atom) `sepBy1` space 

arglist :: Parser [String]
arglist = between (symbol "{") (symbol "}") (some symbolString)

decl :: Parser AST
decl = do
  _ <- char '!'
  sym <- symbolString
  args <- fromMaybe [] <$> optional arglist
  exp <- expression
  return $ Decl sym args exp


parse :: String -> Either String AST
parse s = first (parseErrorPretty' s)  $ MP.parse expression "" s
