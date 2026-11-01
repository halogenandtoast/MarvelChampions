module Marvel.Minion.Minions.Nc48035 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source (getModifiers)
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
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Minion.Runner
import Marvel.Minion.Cards qualified as Cards

nc48035 :: MinionCard Nc48035
nc48035 = minion Nc48035 Cards.nc48035 (Sch 0) (Atk 1) (HP 5)

newtype Nc48035 = Nc48035 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48035 where
  runMessage msg a@(Nc48035 attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      already <- selectAny (IdentityWithId you <> ConfusedIdentity)
      giveStatus you ConfusedStatus
      when already $ do
        cards <- filterM (fmap (\ms -> not $ any (`elem` ms) [CannotBeDiscarded, CannotLeavePlay, CannotLeavePlayByCardAbilities]) . getModifiers) =<< selectList (SupportControlledBy $ IdentityWithId you)
        chooseOne you [targeting card (removeFromPlay card) | card <- cards]
      pure a
    _ -> Nc48035 <$> runMessage msg attrs
