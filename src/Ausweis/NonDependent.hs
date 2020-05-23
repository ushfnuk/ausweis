{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ausweis.NonDependent
  ( main
  ) where

import Data.Coerce (coerce)

import Data.Text (Text)
import qualified Data.Text.IO as T

import Ausweis.Common
import Ausweis.Common.Types
import Ausweis.NonDependent.Types


ausweis
  :: GoalType
  -> Info
  -> Text

-- Work
-- Пропуск*код цели*код документа*серия документа*номер документа* номер машины (если используется)* номер карты Тройка (если используется)*номер карты Стрелка (если используется)*ИНН организации *краткое наименование организации
ausweis gt@Work (Company Common{..} enn name) =
  withAsterisks
    [ startWith
    , toText gt
    , toText documentType
    , coerce serie
    , coerce documentId
    , toText carNumber
    , toText troika
    , toText strelka
    , coerce enn
    , coerce name
    ]

-- Medical
-- Пропуск*код цели* код документа*серия документа*номер документа*дата рождения (через точку)*номер машины (если используется)*номер карты Тройка (если используется)*номер карты Стрелка (если используется)*краткое наименование медицинской организации
ausweis gt@Medical (MedicalOrganization Common{..} birth name) =
  withAsterisks
    [ startWith
    , toText gt
    , toText documentType
    , toText birth
    , coerce serie
    , coerce documentId
    , toText carNumber
    , toText troika
    , toText strelka
    , coerce name
    ]

-- Other
-- Пропуск*код цели*код документа*серия паспорта*номер паспорта*номер машины (если используется)*номер карты Тройка (если используется)*номер карты Стрелка (если используется)*цель выхода (максимум 20 символов)*адрес пункта назначения.
ausweis gt@Other (Destination Common{..} goal address) =
  withAsterisks
    [ startWith
    , toText gt
    , toText documentType
    , coerce serie
    , coerce documentId
    , toText carNumber
    , toText troika
    , toText strelka
    , coerce goal
    , coerce address
    ]
ausweis _ _ = "Тип цели не соответствует введенным данным. Во всём винить разработчика."


main :: IO ()
main = do
  goalType <- getGoalType
  case goalType of
    Work -> do
      let company = getCompany Company
      T.putStrLn =<< ausweis goalType <$> company
    Medical -> do
      let medical = getMedicalOrganization MedicalOrganization
      T.putStrLn =<< ausweis goalType <$> medical
    Other -> do
      let destination = getDestination Destination
      T.putStrLn =<< ausweis goalType <$> destination
