{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50009 where

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
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

aosP50009 :: SupportCard AosP50009
aosP50009 = supportWith AosP50009 Cards.aosP50009 ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype AosP50009 = AosP50009 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50009 where
  getAbilities (AosP50009 attrs) =
    [ability attrs 1 Action (OwnsThis <> AnyCriteria [EnemyExists DamageableEnemy, SchemeExists ThwartableScheme, CharacterExists (IdentityCharacters healableIdentity)]) (ExhaustCost <> UseCost) (runAbility attrs 1)]

instance RunMessage AosP50009 where
  runMessage msg card@(AosP50009 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList DamageableEnemy
      schemes <- selectList ThwartableScheme
      players <- selectList healableIdentity
      chooseOne you $
        [labeled "$label.dealDamage" (chooseDamage you attrs (toDamage 5 FromAbility) DamageableEnemy) | notNull enemies]
        <> [labeled "$label.removeThreat" (removeThreat you attrs 4 ThwartableScheme) | notNull schemes]
        <> [labeled "$label.heal" (chooseOne you [targeting p (heal p 3) | p <- players]) | notNull players]
      pure card
    _ -> AosP50009 <$> runMessage msg attrs
