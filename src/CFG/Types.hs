{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CFG.Types (AST (..), Name (..), Type (..)) where

import Data.Text (Text)
import Data.Typeable (Typeable)

newtype Name = Name Text deriving newtype (Show)

newtype Type = Type Text deriving newtype (Show)

data AST
  = Object Name Type [AST]
  | Ignored
  deriving stock (Show, Typeable)
