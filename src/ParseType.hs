module ParseType (typeP) where

import RIO hiding (some)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.List.NonEmpty as NE
import AST
import Data.Void
import Control.Monad

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

typeP :: Parser Type
typeP = do
  start <- NE.fromList . map SingletonT <$> L.lexeme space (some alphaNumChar) `manyTill` (eof <|> void (symbol ":"))
  endoffile <- optional eof
  case endoffile of
    Nothing -> do
      end <- optional typeP
      return $ Type start end
    Just _ -> return $ Type start Nothing
