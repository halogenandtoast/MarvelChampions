module Marvel.Ally.Allies.WonderManSimonWilliams where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost

-- | To attack, a card is discarded from hand as well.
wonderManSimonWilliams :: AllyCard WonderManSimonWilliams
wonderManSimonWilliams =
  allyWith
    WonderManSimonWilliams
    Cards.wonderManSimonWilliams
    (Thw 1, 1)
    (Atk 3, 1)
    (HP 3)
    \a -> a {allyAdditionalAttackCost = DiscardHandCardCost 1}

instance HasAbilities WonderManSimonWilliams where
  getAbilities _ = []

newtype WonderManSimonWilliams = WonderManSimonWilliams (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage WonderManSimonWilliams where
  runMessage msg (WonderManSimonWilliams attrs) = WonderManSimonWilliams <$> runMessage msg attrs
