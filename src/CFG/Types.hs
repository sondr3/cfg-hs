{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

module CFG.Types (AST (..)) where

import Data.Text (Text)
import Data.Typeable (Typeable)

data AST
  = Fun Text AST
  | Class Text AST
  | None
  deriving stock (Show, Typeable)
