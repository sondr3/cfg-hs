{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module CFG.Parsers.Parser (ParserError (..), Parser) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (Parsec, ShowErrorComponent (showErrorComponent))

newtype ParserError = ParserError Text deriving newtype (Show, Eq, Ord)

instance ShowErrorComponent ParserError where
  showErrorComponent (ParserError text) = "Could not parse " <> T.unpack text

type Parser = Parsec ParserError Text
