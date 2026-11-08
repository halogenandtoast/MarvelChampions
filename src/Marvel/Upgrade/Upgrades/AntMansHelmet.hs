module Marvel.Upgrade.Upgrades.AntMansHelmet (
  antMansHelmet,
  AntMansHelmet (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

antMansHelmet :: UpgradeCard AntMansHelmet
antMansHelmet = upgrade AntMansHelmet Cards.antMansHelmet

newtype AntMansHelmet = AntMansHelmet (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After you change to Giant hero form, heal 2 damage from your hero.
-- Hero Response: After you change to Tiny hero form, draw 1 card.
instance HasAbilities AntMansHelmet where
  getAbilities (AntMansHelmet a) =
    [ limitedWindowAbility a 1 (changesTo Giant) HeroResponse (OwnsThis <> SelfMatches healableIdentity) NoCost $
        Heal (IdentityCharacter you) 2
    , limitedWindowAbility a 2 (changesTo Tiny) HeroResponse OwnsThis NoCost $ runAbility a 2
    ]
   where
    you = upgradeController a
    changesTo t = IdentityChangedToForm After (IdentityWithId you <> IdentityWithTrait t)

instance RunMessage AntMansHelmet where
  runMessage msg u@(AntMansHelmet attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      drawCards you 1
      pure u
    _ -> AntMansHelmet <$> runMessage msg attrs
