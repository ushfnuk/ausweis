{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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

import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Haskeline
  ( runInputT
  , defaultSettings
  , getInputLine
  , InputT
  , MonadException
  , Settings
  )
import Control.Monad.IO.Class (MonadIO(liftIO))

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


settings :: MonadIO m => Settings m
settings = defaultSettings


getCompany
  :: (Common -> Enn -> Name -> b)
  -> IO b
getCompany f = runInputT settings $ do
  f common <$> getEnn <*> getName
  where
    getEnn =
      getWizard
        Enn
        "Введите ИНН вашей организации: "
        "ИНН должно быть числом длиной в 9 цифр\n"
    getName =
      getWizard
        Name
        "Введите название вашей организации: "
        "Название организации не может быть нулевой длины\n"
{-# INLINE getCompany #-}


getMedicalOrganization
  :: (Common -> BirthDate -> Name -> b)
  -> IO b
getMedicalOrganization f = runInputT settings $ do
    day <- getField
      "Введите день своего рождения (дд): "
      "День рождения должен быть в формате: дд"
    month <- getField
      "Введите месяц своего рождения (мм): "
      "Месяц рождения должен быть в формате: мм"
    year <- getField
      "Введите год рождения (гггг): "
      "Год рождения должен быть в формате: гггг"

    f common (BirthDate day month year) <$> getName
  where
    getField msg err = do
      mDay <- getInputLine msg
      case mDay of
        Nothing -> wrongField
        Just "" -> wrongField
        Just field -> pure $ T.pack field
      where
        wrongField = do
          liftIO $ T.putStrLn $ "\n" <> err <> "\n"
          getField msg err

    getName =
      getWizard
        Name
        "Введите название медицинской организации для посещения: "
        "Название медицинской организации не может быть нулевой длины\n"
{-# INLINE getMedicalOrganization #-}


getDestination
  :: (Common -> Goal -> Address -> b)
  -> IO b
getDestination f = runInputT settings $ do
  f common <$> getGoal <*> getAddress
  where
    getGoal =
      getWizard
        Goal
        "Введите цель поездки: "
        "Цель поездки должна быть не более 20 символов"
    getAddress =
      getWizard
        Address
        "Введите адрес поездки: "
        "Адрес поездки не должен быть пустым"
{-# INLINE getDestination #-}


getWizard
  :: MonadException m
  => (Text -> b)
  -> String
  -> Text
  -> InputT m b
getWizard cons iMsg eMsg = do
  getInputLine iMsg >>= \case
    Nothing -> wrongField
    Just "" -> wrongField
    Just field -> pure . cons $ T.pack field
  where
    wrongField = do
      liftIO $ T.putStrLn $ "\n" <> eMsg <> "\n"
      getWizard cons iMsg eMsg
{-# INLINE getWizard #-}


getGoalType :: IO GoalType
getGoalType = do
  T.putStrLn "Введите причину поездки:"
  T.putStrLn " 1) Работа"
  T.putStrLn " 2) Посещение медицинского учреждения"
  T.putStrLn " 3) Другое"

  runInputT settings $ do
    minput <- getInputLine "Выберите один из вариантов ответа (1-3): "
    case validation =<< readMaybe =<< minput of
      Nothing -> liftIO $ do
        T.putStrLn "\nУкажите число от 1 до 3!\n"
        getGoalType
      Just n -> pure . toEnum . pred $ n
    where
      validation n | n > 3 || n < 1 = Nothing
                   | otherwise      = Just n
{-# INLINE getGoalType #-}
