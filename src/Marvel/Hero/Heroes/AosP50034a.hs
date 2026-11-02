{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Hero.Heroes.AosP50034a where

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
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats

aosP50034a :: HeroCard AosP50034a
aosP50034a = hero AosP50034a Cards.aosP50034a (HP $ Static 10) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

newtype AosP50034a = AosP50034a (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50034a where
  getAbilities (AosP50034a attrs) =
    [ limitedWindowAbility attrs 1 (W.MakesBasicThwart W.After $ IdentityWithId $ heroIdentityId attrs) Response OwnsThis NoCost (runAbility attrs 1)
    , limitedWindowAbility attrs 2 (W.IdentityAttacked W.When (IdentityWithId $ heroIdentityId attrs) AnyEnemy) ForcedInterrupt (OwnsThis <> UpgradeExists (UpgradeControlledBy You <> UpgradeWithCard (CardWithTitle "Stealth"))) NoCost (runAbility attrs 2)]

instance RunMessage AosP50034a where
  runMessage msg card@(AosP50034a attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> card <$ placeSuitThreat you 1
    RanAbility you (isTarget attrs -> True) 2 _ _ -> card <$ changeSuit you False
    _ -> AosP50034a <$> runMessage msg attrs
