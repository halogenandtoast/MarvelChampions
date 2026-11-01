module Marvel.Ally.Allies.MsMarvelKamalaKhanNova where

import Marvel.Prelude
import Data.List (find)
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Code ()
import Marvel.Card.Def ()
import Marvel.Card.Id ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Id (EventId (..))
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Matchers
import Marvel.Message.Lifted ()
import Marvel.Projection
import Marvel.Question ()
import Marvel.Window qualified as W

msMarvelKamalaKhanNova :: AllyCard MsMarvelKamalaKhanNova
msMarvelKamalaKhanNova = ally MsMarvelKamalaKhanNova Cards.msMarvelKamalaKhanNova (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype MsMarvelKamalaKhanNova = MsMarvelKamalaKhanNova (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MsMarvelKamalaKhanNova where
  getAbilities a@(MsMarvelKamalaKhanNova attrs) =
    [ limitedWindowAbility a 1 (W.PlaysEvent W.After (IdentityWithId $ allyController attrs) AnyCard)
        HeroResponse (OwnsThis <> PlayedEventInDiscard) (ExhaustCost <> DamageThisCost 1) (runAbility a 1)
    ]

instance RunMessage MsMarvelKamalaKhanNova where
  runMessage msg a@(MsMarvelKamalaKhanNova attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      discarded <- projectMap PlayerIdentityDiscard unDiscard you
      for_ [eid | W.EventPlayed _ eid _ <- windows] \eid ->
        for_ (find ((== CardId (unEventId eid)) . pcCardId) discarded) (addToHand you)
      pure a
    _ -> MsMarvelKamalaKhanNova <$> runMessage msg attrs
