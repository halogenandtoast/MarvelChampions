module Marvel.Support.Supports.GuardiansOfTheGalaxy (guardiansOfTheGalaxy, GuardiansOfTheGalaxy (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait
import Marvel.Window qualified as W

guardiansOfTheGalaxy :: SupportCard GuardiansOfTheGalaxy
guardiansOfTheGalaxy = support GuardiansOfTheGalaxy Cards.guardiansOfTheGalaxy

newtype GuardiansOfTheGalaxy = GuardiansOfTheGalaxy (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GuardiansOfTheGalaxy where
  getAbilities a@(GuardiansOfTheGalaxy attrs) =
    [limitedWindowAbility a 1 (W.PlaysUpgradeOnAlly W.After self) Response
      (OwnsThis <> SelfMatches (IdentityWithTrait Guardian)
        <> NotCriteria (AllyExists $ AllyControlledBy self <> NotAlly (AllyWithTrait Guardian)))
      NoCost (runAbility a 1)]
   where self = IdentityWithId $ supportController attrs

instance RunMessage GuardiansOfTheGalaxy where
  runMessage msg a@(GuardiansOfTheGalaxy attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> a <$ drawCards ident 1
    _ -> GuardiansOfTheGalaxy <$> runMessage msg attrs
