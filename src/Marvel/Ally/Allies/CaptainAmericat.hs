module Marvel.Ally.Allies.CaptainAmericat where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Projection
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

captainAmericat :: AllyCard CaptainAmericat
captainAmericat = ally CaptainAmericat Cards.captainAmericat (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype CaptainAmericat = CaptainAmericat (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CaptainAmericat where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage CaptainAmericat where
  runMessage msg a@(CaptainAmericat attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      placeCounters you 1
      cards <- projectMap PlayerIdentityDiscard unDiscard you
      let signature c = let code = unCardCode (toCardCode c) in code >= "30002" && code <= "30011"
      let eligible = filter signature cards
      unless (null eligible) $ chooseOne you [targeting c (shuffleIntoDeck you [c]) | c <- eligible]
      pure a
    _ -> CaptainAmericat <$> runMessage msg attrs
