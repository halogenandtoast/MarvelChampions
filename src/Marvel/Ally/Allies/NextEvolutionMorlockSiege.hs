module Marvel.Ally.Allies.NextEvolutionMorlockSiege where

import Marvel.Ally.Runner
import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Card
import Marvel.Entity
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Types
import Marvel.Ability
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Window qualified as W
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype MorlockSiegeAlly = MorlockSiegeAlly (Attrs Ally)
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40079 :: AllyCard MorlockSiegeAlly
ne40079 = ally encounterMorlock Cards.ne40079 (Thw 1, 1) (Atk 1, 1) (HP 5)

encounterMorlock :: Attrs Ally -> MorlockSiegeAlly
encounterMorlock a = MorlockSiegeAlly a {allyOriginalCard = Just $ EncounterCard $
  MkEncounterCard (CardId $ unAllyId $ allyId a) Cards.ne40079}

instance HasModifiersFor MorlockSiegeAlly where
  getModifiersFor _ target (MorlockSiegeAlly a) = pure
    [modifier | target == toRef a, modifier <- [NotCountedAgainstAllyLimit, CannotLeavePlayByCardAbilities]]

instance HasAbilities MorlockSiegeAlly where
  getAbilities (MorlockSiegeAlly a) =
    [windowAbility a 1 (W.EnemyWouldAttack AnyEnemy $ IdentityWithId $ allyController a)
      ForcedInterrupt NoCost (runAbility a 1)]

instance RunMessage MorlockSiegeAlly where
  runMessage msg e@(MorlockSiegeAlly a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [enemy | W.EnemyAttack enemy you <- windows, you == allyController a] $ \enemy ->
        redirectEnemyAttackToAlly enemy (allyController a) (allyId a)
      pure e
    _ -> MorlockSiegeAlly <$> runMessage msg a
