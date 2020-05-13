module Main where

import qualified Ausweis.Dependent as AD
import qualified Ausweis.NonDependent as AN

main :: IO ()
main = do
  putStrLn "Без зависимых типов:"
  AN.main

  putStrLn "\nС зависимыми типами:"
  AD.main
