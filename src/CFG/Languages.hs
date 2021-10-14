{-# LANGUAGE DerivingStrategies #-}

module CFG.Languages (Language, parseLanguage, parseLanguageFile) where

import CFG.Parsers.Parser (ParserError)
import qualified CFG.Parsers.Python as PY
import CFG.Types (AST)
import Data.Text (Text)

data Language = Python | Java deriving stock (Show, Eq)

parseLanguage :: Language -> Text -> Either ParserError AST
parseLanguage Python input = PY.parseInput input
parseLanguage Java input = undefined

parseLanguageFile :: Language -> String -> Text -> Either ParserError AST
parseLanguageFile Python file input = PY.parseFile file input
parseLanguageFile Java file input = undefined
