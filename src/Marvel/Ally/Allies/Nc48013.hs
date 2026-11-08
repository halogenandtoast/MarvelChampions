module Marvel.Ally.Allies.Nc48013 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Ally.Runner
import Marvel.Ally.Cards qualified as Cards

nc48013 :: AllyCard Nc48013
nc48013 = ally Nc48013 Cards.nc48013 (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Nc48013 = Nc48013 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Nc48013 where
  getAbilities a = [limitedWindowAbility a 1 (W.BoostCardWithIconsFlipped W.When AttackingEnemy) Interrupt
    OwnsThis (DealDamageThisCost 1) (runAbility a 1)]

instance RunMessage Nc48013 where
  runMessage msg a@(Nc48013 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [card | W.FlippedBoostCard card _ <- windows] $ \card -> cancelBoostIconsOnCard card
      pure a
    _ -> Nc48013 <$> runMessage msg attrs
