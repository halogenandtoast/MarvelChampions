module Marvel.Ally.Allies.MonetStCroix where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers (MinionMatcher(MinionCanBeDefeated, MinionWithFewerRemainingHitPointsThanAlly))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

monetStCroix :: AllyCard MonetStCroix
monetStCroix = ally MonetStCroix Cards.monetStCroix (Thw 2, 1) (Atk 3, 2) (HP 4)

newtype MonetStCroix = MonetStCroix (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MonetStCroix where
  getAbilities a@(MonetStCroix attrs) = [limitedWindowAbility a 1 W.EntersPlayThis Response
    (OwnsThis <> MinionExists (MinionCanBeDefeated <> MinionWithFewerRemainingHitPointsThanAlly (allyId attrs))) NoCost (runAbility a 1)]

instance RunMessage MonetStCroix where
  runMessage msg e@(MonetStCroix attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      eligible <- selectList (MinionCanBeDefeated <> MinionWithFewerRemainingHitPointsThanAlly (allyId attrs))
      chooseOne you [targeting mid $ defeatMinion mid | mid <- eligible]
      pure e
    _ -> MonetStCroix <$> runMessage msg attrs
