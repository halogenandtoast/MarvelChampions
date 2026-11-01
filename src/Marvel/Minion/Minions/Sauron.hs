module Marvel.Minion.Minions.Sauron where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Minion.Runner

sauron :: MinionCard Sauron
sauron = minion Sauron Cards.sauron (Sch 2) (Atk 2) (HP 6)

newtype Sauron = Sauron (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Sauron where
  runMessage msg a@(Sauron attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> a <$ searchAndRevealEncounterCard you (CardIs Cards.lifeDrain)
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      heal enemy 3
      enemyBecomesTough enemy
      pure a
    _ -> Sauron <$> runMessage msg attrs
