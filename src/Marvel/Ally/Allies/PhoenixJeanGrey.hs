module Marvel.Ally.Allies.PhoenixJeanGrey where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.AlterEgo.AlterEgos.ScottSummers (cyclopsCard)

phoenixJeanGrey :: AllyCard PhoenixJeanGrey
phoenixJeanGrey = ally PhoenixJeanGrey Cards.phoenixJeanGrey (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype PhoenixJeanGrey = PhoenixJeanGrey (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PhoenixJeanGrey where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage PhoenixJeanGrey where
  runMessage msg a@(PhoenixJeanGrey attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches cyclopsCard)
      unless (null cards) $ chooseOne you [targeting card (addToHand you card) | card <- cards]
      pure a
    _ -> PhoenixJeanGrey <$> runMessage msg attrs
