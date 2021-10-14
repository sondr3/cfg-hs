{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module CFG.Parsers.Python where

import CFG.Types (AST (Class, Fun))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype ParserError = Unimplemented Text deriving newtype (Show, Eq, Ord)

instance ShowErrorComponent ParserError where
  showErrorComponent (Unimplemented text) = T.unpack text <> " is not implemented"

type Parser = Parsec ParserError Text

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

pFunction :: Parser AST
pFunction = pDefine Fun "def " "function"

pClass :: Parser AST
pClass = pDefine Class "class " "class"
