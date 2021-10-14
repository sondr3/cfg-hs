{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CFG.Types (AST (..)) where

import Data.Text (Text)
import Data.Typeable (Typeable)

data AST
  = Fun Text
  | Class Text
  deriving stock (Show, Typeable)