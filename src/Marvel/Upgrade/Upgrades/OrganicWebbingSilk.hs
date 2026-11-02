module Marvel.Upgrade.Upgrades.OrganicWebbingSilk where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Upgrade.Types

organicWebbingSilk :: UpgradeCard OrganicWebbingSilk
organicWebbingSilk = upgrade OrganicWebbingSilk Cards.organicWebbingSilk
newtype OrganicWebbingSilk = OrganicWebbingSilk (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage OrganicWebbingSilk where
  runMessage msg u@(OrganicWebbingSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseTuckedCard you (\card -> discardTuckedEncounterCard you card >> ready you >> lastingModifiers attrs (IdentityEntity $ IdentityWithId you) [TraitModifier T.Aerial] True)
      pure u
    _ -> OrganicWebbingSilk <$> runMessage msg attrs

instance HasAbilities OrganicWebbingSilk where
  getAbilities u@(OrganicWebbingSilk _attrs) = [ability u 1 HeroAction (OwnsThis <> HasTuckedEncounterCard) ExhaustCost (runAbility u 1)]

instance HasModifiersFor OrganicWebbingSilk where
  getModifiersFor _ (IdentityRef you) (OrganicWebbingSilk attrs) | you == upgradeController attrs = pure [ThwartModifier 1]
  getModifiersFor _ _ _ = pure []
