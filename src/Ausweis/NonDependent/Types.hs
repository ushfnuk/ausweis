module Ausweis.NonDependent.Types
  ( Info(..)
  ) where

import Ausweis.Common.Types

data Info
  = Company Common Enn Name
  | MedicalOrganization Common BirthDate Name
  | Destination Common Goal Address
