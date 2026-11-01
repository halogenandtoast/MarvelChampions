module Marvel.Ally.Allies.PeteWisdom where

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

peteWisdom :: AllyCard PeteWisdom
peteWisdom = ally PeteWisdom Cards.peteWisdom (Thw 3, 2) (Atk 2, 1) (HP 3)

newtype PeteWisdom = PeteWisdom (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PeteWisdom where
  getAbilities (PeteWisdom a) =
    [limitedWindowAbility a 1 (W.TreacheryResolvedBy $ IdentityWithId $ allyController a) Response OwnsThis NoCost (runAbility a 1) | allyDamage a > 0]

instance RunMessage PeteWisdom where
  runMessage msg e@(PeteWisdom a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ heal (allyId a) 1
    _ -> PeteWisdom <$> runMessage msg a
