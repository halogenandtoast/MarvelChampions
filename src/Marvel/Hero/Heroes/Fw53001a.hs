{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Hero.Heroes.Fw53001a where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Hero.Types
import Marvel.Hero.Runner ()

import Marvel.Phase
fw53001a :: HeroCard Fw53001a
fw53001a = hero Fw53001a Cards.fw53001a (HP $ Static 10) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

newtype Fw53001a = Fw53001a (Attrs Hero)
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53001a where
  getAbilities a@(Fw53001a attrs) =
    [label "$label.eagleEyed" $ limitedWindowAbility a 1 (W.PlaysCard W.After (IdentityWithId $ heroIdentityId attrs) $ CardWithTrait T.Aerial) Response (IsSelf) (NoCost) (runAbility a 1)]

instance HasModifiersFor Fw53001a where
  getModifiersFor _ GameRef (Fw53001a _) = do
    phase <- getPhase
    pure [CanLookAtTopOfEncounterDeck | phase == PlayerPhase]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw53001a where
  runMessage msg a@(Fw53001a attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      discardTopOfEncounterDeckFor 1 attrs
      pure a
    _ -> Fw53001a <$> runMessage msg attrs
