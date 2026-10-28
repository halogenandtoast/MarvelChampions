module Marvel.Ally.Allies.MartinexTNaga (
  martinexTNaga,
  MartinexTNaga (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Id
import Marvel.Id
import Marvel.Matchers
import Marvel.Trait (Trait (Guardian))

martinexTNaga :: AllyCard MartinexTNaga
martinexTNaga = ally MartinexTNaga Cards.martinexTNaga (Thw 1, 1) (Atk 1, 1) (HP 4)

newtype MartinexTNaga = MartinexTNaga (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | In hand, his cost is 1 less if his player's identity has the Guardian trait, in the
-- form it is in.
instance HasModifiersFor MartinexTNaga where
  getModifiersFor _ (CardIdRef cid) (MartinexTNaga a) | cid == CardId (unAllyId $ allyId a) = do
    guardian <- identityMatches (IdentityWithTrait Guardian) (allyController a)
    pure [ResourceCostReduction 1 | guardian]
  getModifiersFor _ _ _ = pure []

instance RunMessage MartinexTNaga where
  runMessage msg (MartinexTNaga attrs) = MartinexTNaga <$> runMessage msg attrs
