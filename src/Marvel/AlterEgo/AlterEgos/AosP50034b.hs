{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.AlterEgo.AlterEgos.AosP50034b where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Obligations

aosP50034b :: AlterEgoCard AosP50034b
aosP50034b = alterEgo AosP50034b Cards.aosP50034b (HP $ Static 10) (HandSize 6) (Rec 4) [Obligations.aosP50059]

newtype AosP50034b = AosP50034b (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50034b where
  getAbilities (AosP50034b attrs) =
    [ability attrs 1 Action (OwnsThis <> UpgradeExists (UpgradeControlledBy You <> UpgradeWithCard (CardWithTitle "Assault"))) NoCost (runAbility attrs 1), ability attrs 2 Setup OwnsThis NoCost (runAbility attrs 2)]

instance RunMessage AosP50034b where
  runMessage msg card@(AosP50034b attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> card <$ changeSuit you True
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      putCardFromSetAsideIntoPlay you "50035a"
      pure card
    _ -> AosP50034b <$> runMessage msg attrs
