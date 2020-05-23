{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ausweis.Dependent
  ( main
  ) where

import Data.Constraint (Dict(Dict))

import qualified Data.Text.IO as T

import Ausweis.Common
import Ausweis.Common.Types
import Ausweis.Dependent.Types


printAusweis :: GoalType -> IO ()
printAusweis gt = withSomeSGoalType (toSGoalType gt) $ \(sg :: SGoalType g) ->
  case dict @ClassInfo sg of
    Dict -> T.putStrLn =<< ("\nВаш пропуск: " <>) . ausweis @g sg <$> getInfo

main :: IO ()
main = do
  printAusweis =<< getGoalType
