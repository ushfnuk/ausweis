{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ausweis.Dependent
  ( main
  ) where

import Data.Constraint (Dict(Dict))

import qualified Data.Text.IO as T
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import Ausweis.Common
import Ausweis.Common.Types
import Ausweis.Dependent.Types


printAusweis :: GoalType -> IO ()
printAusweis gt = withSomeSGoalType (toSGoalType gt) $ \(sg :: SGoalType g) ->
    case dict @ClassInfo sg of
      Dict -> T.putStrLn =<< ausweis @g sg <$> getInfo

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  printAusweis =<< getGoalType
