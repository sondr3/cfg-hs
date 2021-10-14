{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module CFG.Parsers.Python where

import CFG.Parsers.Parser (Parser, ParserError (..))
import CFG.Types (AST (..))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "\"\"\"" "\"\"\"")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keyword :: Text -> Parser Text
keyword p = (lexeme . try) $ string p <* space

skipToEol :: Parser Text -> Parser ()
skipToEol end = void (manyTill anySingle end <* eol)

pClass :: Parser AST
pClass = do
  void (keyword "class") <?> "Class"
  name <- takeWhile1P Nothing (\x -> x `notElem` [':', '(']) <?> "Class name"
  (skipToEol ":" <|> skipToEol "") <?> "End of class"
  Class name <$> pTryExpr <* eof

pFunction :: Parser AST
pFunction = do
  void (keyword "def") <?> "Function"
  name <- takeWhile1P Nothing (/= '(') <?> "Function name"
  skipToEol ":" <?> "End of function"
  Fun name <$> pTryExpr <* eof

pNone :: Parser AST
pNone = pure None

pTryExpr :: Parser AST
pTryExpr = choice (map (try . (space *>)) [pClass, pFunction, pNone]) <* eof

pExpr :: Parser AST
pExpr = pClass <|> pFunction

parseAst :: String -> Text -> Either ParserError AST
parseAst file input = case runParser pExpr file input of
  Right out -> Right out
  Left err -> Left $ ParserError (T.pack $ errorBundlePretty err)

parseInput :: Text -> Either ParserError AST
parseInput = parseAst "file"

parseFile :: String -> Text -> Either ParserError AST
parseFile = parseAst
