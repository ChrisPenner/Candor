module Parse where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.List.NonEmpty as NE

import Data.Void
import AST
import ParseType
-- import Control.Applicative

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

typedef :: Parser Type
typedef = between (symbol "<") (symbol ">") typeP

expression :: Parser AST
expression = list

atom :: Parser AST
atom = Atom <$> L.lexeme space (some (noneOf " ()\t\n"))

chunk :: Parser AST
chunk = between (symbol "(") (symbol ")") $ do
  typ <- optional typedef
  case typ of
    Just t -> Typed t <$> expression
    Nothing -> expression

list :: Parser AST
list = List . NE.fromList <$> (chunk <|> atom) `sepBy1` space 

qp p s = parse p "" s
