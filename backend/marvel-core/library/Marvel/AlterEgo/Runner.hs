{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.AlterEgo.Runner
  ( module X
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Types as X
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Message
import Marvel.Question

instance HasAbilities AlterEgo where
  getAbilities (AlterEgo a) = getAbilities a <> basicAbilities
    where basicAbilities = [ability a 200 Basic NoCriteria ExhaustCost Recover]

instance RunMessage AlterEgo where
  runMessage msg (AlterEgo a) = AlterEgo <$> runMessage msg a

