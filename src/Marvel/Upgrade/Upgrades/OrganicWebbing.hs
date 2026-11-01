module Marvel.Upgrade.Upgrades.OrganicWebbing where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Matchers qualified as Matchers
import Marvel.Query ()
import Marvel.Question
import Marvel.Trait
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

organicWebbing :: UpgradeCard OrganicWebbing
organicWebbing = upgrade OrganicWebbing Cards.organicWebbing

newtype OrganicWebbing = OrganicWebbing (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor OrganicWebbing where
  getModifiersFor _ (IdentityRef you) (OrganicWebbing attrs)
    | you == upgradeController attrs = do
      heroForm <- identityMatches HeroIdentity you
      pure [ThwartModifier 1 | heroForm]
  getModifiersFor _ _ _ = pure []

instance HasAbilities OrganicWebbing where
  getAbilities a = [ability a 1 HeroAction (OwnsThis <> AnyCriteria [SelfMatches Matchers.ExhaustedIdentity, NotCriteria (SelfMatches $ IdentityWithTrait Aerial)])
    (ExhaustCost <> IdentityCountersCost 1) (runAbility a 1)]

instance RunMessage OrganicWebbing where
  runMessage msg u@(OrganicWebbing attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      ready you
      createEffect Cards.organicWebbing attrs (IdentityEntity $ IdentityWithId you)
      pure u
    _ -> OrganicWebbing <$> runMessage msg attrs

newtype OrganicWebbingEffect = OrganicWebbingEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

organicWebbingEffect :: CardEffect OrganicWebbingEffect
organicWebbingEffect = effectWith OrganicWebbingEffect Cards.organicWebbing $
  (modifiersL .~ [TraitModifier Aerial]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage OrganicWebbingEffect where
  runMessage msg (OrganicWebbingEffect attrs) = OrganicWebbingEffect <$> runMessage msg attrs
