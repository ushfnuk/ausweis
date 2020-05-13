{-# LANGUAGE OverloadedStrings #-}

module Ausweis.Common.Types
  (
  -- * Convert types
    ToText(..)
  -- * Common types
  , GoalType(..)
  , DocumentType(..)
  , Serie(..)
  , DocumentId(..)
  , BirthDate(..)
  , CarNumber(..)
  , Troika(..)
  , Strelka(..)
  , Common(..)
  -- * Medical organization
  , Enn(..)
  , Address(..)
  -- * Company
  , Goal(..)
  , Name(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T


class ToText a where
  toText :: a -> Text


data GoalType
  = Work
  | Medical
  | Other
  deriving Enum

instance ToText GoalType where
  toText = T.pack . show . succ . fromEnum


data DocumentType
  = Russian
  | Foreigner
  | OtherId
  deriving Enum

instance ToText DocumentType where
  toText = T.pack . show . succ . fromEnum


newtype Serie = Serie Text
newtype DocumentId = DocumentId Text


data BirthDate = BirthDate !Text !Text !Text

instance ToText BirthDate where
  toText (BirthDate d m y) = T.intercalate "." [d, m, y]


data CarNumber = NoCar | CarNumber !Text

instance ToText CarNumber where
  toText NoCar = ""
  toText (CarNumber t) = t


data Troika = NoTroika | Troika !Text

instance ToText Troika where
  toText NoTroika = ""
  toText (Troika t) = t


data Strelka = NoStrelka | Strelka !Text

instance ToText Strelka where
  toText NoStrelka = ""
  toText (Strelka t) = t

newtype Enn = Enn Text
newtype Address = Address Text

newtype Goal = Goal Text
newtype Name = Name Text

data Common
  = Common
  { documentType :: !DocumentType
  , serie :: !Serie
  , documentId :: !DocumentId
  , carNumber :: !CarNumber
  , troika :: !Troika
  , strelka :: !Strelka
  }
