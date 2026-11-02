{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Hero.Heroes.AosP50001a where

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

aosP50001a :: HeroCard AosP50001a
aosP50001a = hero AosP50001a Cards.aosP50001a (HP $ Static 9) (HandSize 5) (Thw 2) (Atk 1) (Def 2)

newtype AosP50001a = AosP50001a (Attrs Hero)
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50001a where
  getAbilities (AosP50001a attrs) =
    [limitedAbility attrs 1 (PerRound 1) Action (OwnsThis <> MovableSupportCounter T.Shield) NoCost (runAbility attrs 1)]

instance HasModifiersFor AosP50001a where
  getModifiersFor _ (AllyRef aid) (AosP50001a attrs) = do
    yours <- selectAny (AllyWithId aid <> AllyControlledBy (IdentityWithId $ heroIdentityId attrs))
    pure [TraitModifier T.Shield | yours]
  getModifiersFor _ _ _ = pure []

instance RunMessage AosP50001a where
  runMessage msg card@(AosP50001a attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      supports <- selectList (SupportMatches [SupportWithTrait T.Shield, SupportWithAnyUses])
      chooseOne you [targeting sid do
        others <- filter (/= sid) <$> selectList (SupportWithTrait T.Shield)
        chooseOne you [targeting other (removeCounters sid 1 >> placeCounters other 1) | other <- others]
        | sid <- supports]
      pure card
    _ -> AosP50001a <$> runMessage msg attrs
