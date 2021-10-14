module CFG.Parsers.Python (parseFile, parseInput) where

import CFG.Parsers.Parser (Parser, ParserError (..))
import CFG.Types (AST (Class, Fun))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "\"\"\"" "\"\"\"")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

skipToEol :: Parser Text -> Parser ()
skipToEol end = void (manyTill anySingle end <* eol)

pDefine :: (Text -> AST) -> Text -> String -> Parser AST
pDefine cls sym kind = do
  void (try $ symbol sym) <?> kind
  name <- takeWhile1P Nothing (/= '(') <?> kind <> " name"
  skipToEol ":" <?> "End of " <> kind
  return $ cls name

pClass :: Parser AST
pClass = pDefine Class "class " "class"

pFunction :: Parser AST
pFunction = pDefine Fun "def " "function"

pExpr :: Parser AST
pExpr = pClass <|> pFunction

-- pExpr :: FilePath -> Text
parseAst :: String -> Text -> Either ParserError AST
parseAst file input = case runParser pExpr file input of
  Right out -> Right out
  Left err -> Left $ ParserError (T.pack $ errorBundlePretty err)

parseInput :: Text -> Either ParserError AST
parseInput = parseAst "file"

parseFile :: String -> Text -> Either ParserError AST
parseFile = parseAst
