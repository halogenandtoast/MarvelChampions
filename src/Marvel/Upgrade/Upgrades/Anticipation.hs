module Marvel.Upgrade.Upgrades.Anticipation (anticipation, Anticipation (..)) where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

anticipation :: UpgradeCard Anticipation
anticipation = upgrade Anticipation Cards.anticipation

newtype Anticipation = Anticipation (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Anticipation where
  getAbilities a@(Anticipation attrs) =
    [ limitedWindowAbility a 1
        (EngagesMinion When (IdentityWithId $ upgradeController attrs) AnyMinion)
        HeroInterrupt
        (OwnsThis <> CharacterExists (IdentityCharacters You <> CharacterCanBeReadied))
        (DiscardCost $ toTarget attrs) (runAbility a 1)
    ]

instance RunMessage Anticipation where
  runMessage msg u@(Anticipation attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> ready you >> pure u
    _ -> Anticipation <$> runMessage msg attrs
