module Marvel.Minion.Minions.Nc48034 where

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
import Marvel.SideScheme.Cards qualified as Schemes
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

nc48034 :: MinionCard Nc48034
nc48034 = minion Nc48034 Cards.nc48034 (Sch 0) (Atk 1) (HP 4)

newtype Nc48034 = Nc48034 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48034 where
  runMessage msg a@(Nc48034 attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      gang <- selectAny $ SideSchemeIs Schemes.nc48033
      if gang then dealEncounterCard you else searchAndRevealEncounterCard you (CardWithTitle "The Crazy Gang")
      pure a
    _ -> Nc48034 <$> runMessage msg attrs
