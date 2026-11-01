module Marvel.Minion.Minions.HellfirePawnMagneto where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity ()
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref ()
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

hellfirePawnMagneto :: MinionCard HellfirePawnMagneto
hellfirePawnMagneto = minion HellfirePawnMagneto Cards.hellfirePawnMagneto (Sch 1) (Atk 2) (HP 3)

newtype HellfirePawnMagneto = HellfirePawnMagneto (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HellfirePawnMagneto where
  runMessage msg e@(HellfirePawnMagneto attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      putBoostIntoPlay attrs you
      pure e
    _ -> HellfirePawnMagneto <$> runMessage msg attrs
