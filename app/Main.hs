module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import qualified Ausweis.Dependent as AD
import qualified Ausweis.NonDependent as AN

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- without dependent types
  -- AN.main

  -- with dependent types
  AD.main
