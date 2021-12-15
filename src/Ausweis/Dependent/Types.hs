{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Ausweis.Dependent.Types
  (
  -- * Singletons
    SGoalType(..)
  , SomeSGoalType(..)
  , fromSGoalType
  , toSGoalType
  , withSomeSGoalType
  , ClassInfo(..)
  , dict
  -- * Domain
  , Company(..)
  , MedicalOrganization(..)
  , Destination(..)
  ) where

import Data.Coerce (coerce)
import Data.Constraint (Dict(..))
import Data.Kind (Type)

import Data.Text (Text)

import Ausweis.Common
import Ausweis.Common.Types

data SGoalType (g :: GoalType) where
  SWork    :: SGoalType 'Work
  SMedical :: SGoalType 'Medical
  SOther   :: SGoalType 'Other

fromSGoalType :: SGoalType g -> GoalType
fromSGoalType SWork    = Work
fromSGoalType SMedical = Medical
fromSGoalType SOther   = Other

data SomeSGoalType where
  SomeSGoalType :: SGoalType g -> SomeSGoalType

withSomeSGoalType
  :: GoalType
  -> (forall (g :: GoalType). SGoalType g -> r)
  -> r
withSomeSGoalType g f = case toSGoalType g of SomeSGoalType s -> f s

toSGoalType :: GoalType -> SomeSGoalType
toSGoalType Work    = SomeSGoalType SWork
toSGoalType Medical = SomeSGoalType SMedical
toSGoalType Other   = SomeSGoalType SOther


class ClassInfo (g :: GoalType) where
  type Info g = (r :: Type) | r -> g
  getInfo :: IO (Info g)
  ausweis :: SGoalType g -> Info g -> Text

-- Work
-- Пропуск*код цели*код документа*серия документа*номер документа* номер машины (если используется)* номер карты Тройка (если используется)*номер карты Стрелка (если используется)*ИНН организации *краткое наименование организации

instance ClassInfo 'Work where
  type Info 'Work = Company
  getInfo = getCompany Company

  ausweis sg (Company Common{..} enn name) =
    withAsterisks
      [ startWith
      , toText $ fromSGoalType sg
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

instance ClassInfo 'Medical where
  type Info 'Medical = MedicalOrganization
  getInfo = getMedicalOrganization MedicalOrganization

  ausweis sg (MedicalOrganization Common{..} birth name) =
    withAsterisks
      [ startWith
      , toText $ fromSGoalType sg
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

instance ClassInfo 'Other where
  type Info 'Other = Destination
  getInfo = getDestination Destination

  ausweis sg (Destination Common{..} goal address) =
    withAsterisks
      [ startWith
      , toText $ fromSGoalType sg
      , toText documentType
      , coerce serie
      , coerce documentId
      , toText carNumber
      , toText troika
      , toText strelka
      , coerce goal
      , coerce address
      ]

dict
  :: ( c 'Work
     , c 'Medical
     , c 'Other
     )
  => SGoalType b
  -> Dict (c b)
dict SWork    = Dict
dict SMedical = Dict
dict SOther   = Dict


data Company = Company !Common !Enn !Name
data MedicalOrganization = MedicalOrganization !Common !BirthDate !Name
data Destination = Destination !Common !Goal !Address
