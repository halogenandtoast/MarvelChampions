{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.Fw53002 where
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
import Marvel.Ally.Types
import Marvel.Ally.Runner ()

import Marvel.FalconWinter.Helpers
fw53002 :: AllyCard Fw53002
fw53002 = ally Fw53002 Cards.fw53002 (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Fw53002 = Fw53002 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53002 where
  getAbilities a@(Fw53002 attrs) =
    [ability a 1 HeroAction (OwnsThis <> EncounterTopHasEffect <> AnyCriteria [EnemyExists (EnemyCanTakeDamageFrom $ toSource attrs), SchemeExists ThwartableScheme]) (ExhaustCost <> ReturnAllyToHandCost (AllyWithId $ allyId attrs) <> DiscardTopEncounterCardCost) (runAbility a 1)]

instance RunMessage Fw53002 where
  runMessage msg a@(Fw53002 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      countFalconIcons attrs "redwing" you [card | EncounterCard card <- paymentEncounterDiscards payment]
      pure a
    WithBoostIcons (isTarget attrs -> True) value counts -> a <$ resolveFalconIcons attrs value counts
    CardSpecific (isTarget attrs -> True) "redwing" (fromJSON -> Success (you, n :: Natural)) -> do
      when (n > 0) $ do
        enemies <- selectList $ EnemyCanTakeDamageFrom $ toSource attrs
        schemes <- selectList ThwartableScheme
        chooseOne you $
          [labeled "$label.dealDamage" (chooseDamage you attrs (toDamage n FromAbility) AnyEnemy) | notNull enemies]
          <> [labeled "$label.removeThreat" (removeThreat you attrs n ThwartableScheme) | notNull schemes]
      pure a
    _ -> Fw53002 <$> runMessage msg attrs
