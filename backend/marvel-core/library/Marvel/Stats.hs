module Marvel.Stats where

import Marvel.Prelude

newtype Atk = Atk { unAtk :: Natural }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Thw = Thw { unThw :: Natural }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Def = Def { unDef :: Natural }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Sch = Sch { unSch :: Natural }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Rec = Rec Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

