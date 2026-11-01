module Marvel.Ally.Allies.DominoPsylocke where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

dominoPsylocke :: AllyCard DominoPsylocke
dominoPsylocke = ally DominoPsylocke Cards.dominoPsylocke (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype DominoPsylocke = DominoPsylocke (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DominoPsylocke where
  getAbilities (DominoPsylocke a) =
    [limitedWindowAbility a 1 (W.WindowOneOf [W.AllyAttacked W.After (AllyWithId $ allyId a) AnyEnemy, W.AllyThwarted W.After (AllyWithId $ allyId a) AnyScheme])
      Response (OwnsThis <> ExtendedCardExists (InHandOf (IdentityWithId $ allyController a) $ BasicCardMatches AnyCard)) NoCost (runAbility a 1)]

instance RunMessage DominoPsylocke where
  runMessage msg e@(DominoPsylocke a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      cards <- selectList (InHandOf (IdentityWithId you) $ BasicCardMatches AnyCard)
      chooseOne you [targeting card $ cardSpecific you "swapHandWithDeckTop" card | card <- cards]
      pure e
    _ -> DominoPsylocke <$> runMessage msg a
