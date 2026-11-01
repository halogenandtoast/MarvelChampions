module Marvel.Minion.Minions.PyroIceman where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Attack (Attack (..))
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

pyroIceman :: MinionCard PyroIceman
pyroIceman = minion PyroIceman Cards.pyroIceman (Sch 1) (Atk 3) (HP 4)

newtype PyroIceman = PyroIceman (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PyroIceman where
  runMessage msg (PyroIceman attrs) = runQueueT $ case msg of
    MinionMessage mid (MinionBeginAttack _) | mid == minionId attrs -> do
      updated <- runMessage msg attrs
      pure $ PyroIceman $ updated {minionAttacking = fmap (\attack -> attack {attackIndirect = True}) $ minionAttacking updated}
    _ -> PyroIceman <$> runMessage msg attrs
