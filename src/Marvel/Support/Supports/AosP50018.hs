{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50018 where

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

aosP50018 :: SupportCard AosP50018
aosP50018 = supportWith AosP50018 Cards.aosP50018 ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype AosP50018 = AosP50018 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50018 where
  getAbilities (AosP50018 attrs) =
    [ability attrs 1 Action (OwnsThis) (ExhaustCost <> UseCost) (runAbility attrs 1)]

instance RunMessage AosP50018 where
  runMessage msg card@(AosP50018 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      chooseOne you [targeting p do
        withTheVillain you $ \villain -> do
          minions <- selectList (MinionEngagedWith $ IdentityWithId p)
          let enemies = EnemyVillainId villain : map EnemyMinionId minions
          for_ enemies removeAllTough
          dealDamageToEach enemies attrs (toDamage 3 FromAbility)
        | p <- players]
      pure card
    _ -> AosP50018 <$> runMessage msg attrs
