module Marvel.Upgrade.Upgrades.OrganicSteel where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

organicSteel :: UpgradeCard OrganicSteel
organicSteel = upgradeWith OrganicSteel Cards.organicSteel $ (usesL .~ 2) . (discardIfNoUsesL .~ True)

newtype OrganicSteel = OrganicSteel (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities OrganicSteel where
  getAbilities (OrganicSteel a) =
    [limitedWindowAbility a 1 (W.IdentityDiscardsTough W.After $ IdentityWithId $ upgradeController a)
      HeroResponse (OwnsThis <> CharacterExists (CharacterWithId (IdentityCharacter $ upgradeController a) <> CharacterCanGainTough)) (ExhaustCost <> UseCost) (runAbility a 1)]

instance RunMessage OrganicSteel where
  runMessage msg e@(OrganicSteel attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> e <$ becomeTough you
    _ -> OrganicSteel <$> runMessage msg attrs
