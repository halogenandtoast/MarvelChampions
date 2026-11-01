module Marvel.Minion.Minions.Nc48037 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
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

nc48037 :: MinionCard Nc48037
nc48037 = minion Nc48037 Cards.nc48037 (Sch 0) (Atk 2) (HP 6)

newtype Nc48037 = Nc48037 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48037 where
  runMessage msg a@(Nc48037 attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      already <- selectAny (IdentityWithId you <> StunnedIdentity)
      giveStatus you StunnedStatus
      when already $ do
        cards <- selectList (UpgradeThatCanBeDiscarded <> UpgradeControlledBy (IdentityWithId you))
        chooseOne you [targeting card (removeFromPlay card) | card <- cards]
      pure a
    _ -> Nc48037 <$> runMessage msg attrs
