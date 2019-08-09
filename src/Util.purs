module Hearthcharm.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Foldable as F
import Effect.Aff (Error, error, throwError)


orThrow :: forall t114 t117 t118. F.Foldable t114 => Applicative t117 => MonadThrow Error t117 => String -> t114 t118 -> t117 t118
orThrow e m = 
  F.foldl (\_ v -> pure v) (throwError (error e)) m
