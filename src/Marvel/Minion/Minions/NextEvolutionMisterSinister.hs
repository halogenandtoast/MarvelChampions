module Marvel.Minion.Minions.NextEvolutionMisterSinister where

import Marvel.Minion.Runner
import Marvel.Effect.Types
import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.NextEvolution.Sinister

import Marvel.Card
import Marvel.Entity
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype MisterSinisterMinion = MisterSinisterMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40145 :: MinionCard MisterSinisterMinion
ne40145 = minion MisterSinisterMinion Cards.ne40145 (Sch 1) (Atk 1) (HP 5)

instance RunMessage MisterSinisterMinion where
  runMessage msg card@(MisterSinisterMinion a) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      sinister >>= traverse_ (\vid -> when (enemy == EnemyVillainId vid) do
        n <- superpowers vid
        boostActivationStats vid n)
      pure card
    _ -> MisterSinisterMinion <$> runMessage msg a

instance HasModifiersFor MisterSinisterMinion where
  getModifiersFor _ target (MisterSinisterMinion a) | target == toRef a = do
    n <- maybe (pure 0) superpowers =<< sinister
    pure [AttackModifier n, SchemeModifier n]
  getModifiersFor _ _ _ = pure []

newtype SinisterSoldierBoostEffect = SinisterSoldierBoostEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

ne40145BoostEffect :: CardEffect SinisterSoldierBoostEffect
ne40145BoostEffect = effect SinisterSoldierBoostEffect Cards.ne40145

instance RunMessage SinisterSoldierBoostEffect where
  runMessage msg card@(SinisterSoldierBoostEffect a) = runQueueT $ case msg of
    CardSpecific source "sinisterBoostAmount" (fromJSON -> Success n)
      | source == refocus (effectSource a), null (effectModifiers a) ->
          pure $ SinisterSoldierBoostEffect a {effectModifiers = [AttackModifier n, SchemeModifier n]}
    CheckWindows windows | any (\case W.Window W.After (W.EnemyActivation enemy _) -> enemyRef enemy == refocus (effectSource a); _ -> False) windows -> card <$ disableEffect (effectId a)
    _ -> SinisterSoldierBoostEffect <$> runMessage msg a
