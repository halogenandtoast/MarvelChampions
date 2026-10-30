module Marvel.Minion.Minions.Prowler where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Matchers

prowler :: MinionCard Prowler
prowler = minion Prowler Cards.prowler (Sch 1) (Atk 2) (HP 5)

newtype Prowler = Prowler (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Prowler where
  runMessage msg m@(Prowler attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      alterEgo <- identityMatches AlterEgoIdentity you
      when alterEgo $ enemyBecomesTough mid
      pure m
    _ -> Prowler <$> runMessage msg attrs
