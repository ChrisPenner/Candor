module Parse where

import RIO hiding (try, some, first, many)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as MP

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List.NonEmpty as NE (NonEmpty, fromList)

import AST
import Data.Bifunctor

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

expression :: Parser AST
expression = appl <|> stringLiteral <|> try numberLiteral <|> try boolLiteral <|> symbolLiteral <|> listLiteral <|> funcLiteral

boolLiteral :: Parser AST
boolLiteral = do
  s <- symbolLexeme
  case s of
    "T" -> return $ Boolean True
    "F" -> return $ Boolean False
    _ -> empty

stringLiteral :: Parser AST
stringLiteral = Str <$> (char '"' *> manyTill L.charLiteral (char '"'))

numberLiteral :: Parser AST
numberLiteral = Number <$> do
  sign <- optional $ char '-'
  num <- L.lexeme space L.decimal
  return $ case sign of
    Just _ -> negate num
    Nothing -> num

symbolLexeme :: Parser String
symbolLexeme = L.lexeme space (some (noneOf ("\t\n\r ()[]{}<>" :: String)))

symbolLiteral :: Parser AST
symbolLiteral = Symbol <$> symbolLexeme

listLiteral :: Parser AST
listLiteral = List <$> list

funcLiteral :: Parser AST
funcLiteral = 
  between (symbol "{") (symbol "}") $ do
    args <- argList
    expr <- expression
    return (FuncDef args expr)
  where
    argList :: Parser (NonEmpty String)
    argList = NE.fromList <$>
      between (symbol "[") (symbol "]") (symbolLexeme `sepBy1` space)

appl :: Parser AST
appl = between (symbol "(") (symbol ")") 
        (Appl <$> expression <*> (expression `sepBy1` space))

list :: Parser [AST]
list = between (symbol "[") (symbol "]") (many expression)

parse :: String -> Either String AST
parse s = first (parseErrorPretty' s)  $ MP.parse (expression <* eof) "" s
