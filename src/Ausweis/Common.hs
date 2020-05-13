{-# LANGUAGE OverloadedStrings #-}

module Ausweis.Common
  (
  -- * Utils
    startWith
  , separator
  , withAsterisks
  , common
  -- * User interface
  , getCompany
  , getMedicalOrganization
  , getDestination
  , getGoalType
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Ausweis.Common.Types

startWith :: Text
startWith = "Пропуск"
{-# INLINE startWith #-}


separator :: Text
separator = "*"
{-# INLINE separator #-}


withAsterisks :: [Text] -> Text
withAsterisks = T.intercalate separator
{-# INLINE withAsterisks #-}


common :: Common
common = Common Foreigner (Serie "ES") (DocumentId "879214") NoCar NoTroika NoStrelka
-- common = Common Russian (Serie "4510") (DocumentId "582874") NoCar NoTroika NoStrelka
{-# INLINE common #-}


getCompany
  :: (Common -> Enn -> Name -> b)
  -> IO b
getCompany f = do
  T.putStr "Введите ИНН вашей организации: "
  enn <- Enn <$> T.getLine

  putStr "Введите название вашей организации: "
  name <- Name <$> T.getLine
  pure $ f common enn name
{-# INLINE getCompany #-}


getMedicalOrganization
  :: (Common -> BirthDate -> Name -> b)
  -> IO b
getMedicalOrganization f = do
  let getPart = T.getLine <* putStr "."
  putStr "Введите дату своего рождения (дд/мм/гггг): "
  birth <- BirthDate <$> getPart <*> getPart <*> T.getLine

  putStr "Введите название медицинской организации для посещения: "
  name <- Name <$> T.getLine
  pure $ f common birth name
{-# INLINE getMedicalOrganization #-}


getDestination
  :: (Common -> Goal -> Address -> b)
  -> IO b
getDestination f = do
  putStr "Введите цель поездки: "
  goal <- Goal <$> T.getLine

  putStr "Введите адрес поездки: "
  address <- Address <$> T.getLine
  pure $ f common goal address
{-# INLINE getDestination #-}


getGoalType :: IO GoalType
getGoalType = do
  T.putStrLn "Введите причину поездки:"
  T.putStrLn "1) Работа"
  T.putStrLn "2) Посещение медицинского учреждения"
  T.putStrLn "3) Другое"

  T.putStr "Номер варианта (1-3): "
  toEnum . pred <$> readLn
{-# INLINE getGoalType #-}
