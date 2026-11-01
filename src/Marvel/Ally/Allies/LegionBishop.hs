module Marvel.Ally.Allies.LegionBishop where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Question
import Marvel.Resource
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

legionBishop :: AllyCard LegionBishop
legionBishop = ally LegionBishop Cards.legionBishop (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype LegionBishop = LegionBishop (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LegionBishop where
  getAbilities a@(LegionBishop attrs) = [limitedWindowAbility a 1
    (W.WindowOneOf [W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy,
      W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme,
      W.AllyDefends (AllyWithId $ allyId attrs)])
    Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage LegionBishop where
  runMessage msg a@(LegionBishop attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ discardFromDeck you 1 attrs
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      let icons = concatMap (printedResources . getCardDef) cards
      when (Energy `elem` icons) $ chooseDamage (allyController attrs) attrs (toDamage 2 FromAbility) AnyEnemy
      when (Mental `elem` icons) $ removeThreat (allyController attrs) attrs 2 ThwartableScheme
      when (Physical `elem` icons) $ heal (allyId attrs) 2
      pure a
    _ -> LegionBishop <$> runMessage msg attrs
