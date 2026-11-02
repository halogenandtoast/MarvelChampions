{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.SideScheme.SideSchemes.Fw54029 where
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
import Marvel.SideScheme.Types

fw54029 :: SideSchemeCard Fw54029
fw54029 = sideScheme Fw54029 Cards.fw54029 (PerPlayer 3)

newtype Fw54029 = Fw54029 (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Fw54029 where
  getModifiersFor _ (MinionRef mid) _ = do
    hydra <- minionMatches (MinionWithTrait T.Hydra) mid
    pure [m | hydra, m <- [AttackModifier 1, HitPointModifier 2]]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw54029 where
  runMessage msg a@(Fw54029 attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      player <- getDefeatedBy attrs
      searchAndRevealEncounterCard player (CardWithType MinionType <> CardWithTrait T.Hydra)
      pure a
    _ -> Fw54029 <$> runMessage msg attrs
