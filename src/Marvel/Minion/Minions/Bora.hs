module Marvel.Minion.Minions.Bora where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Status
import Marvel.Trait (Trait (Inheritor, WebWarrior))
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

bora :: MinionCard Bora
bora = minion Bora Cards.bora (Sch 3) (Atk 1) (HP 5)

newtype Bora = Bora (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Bora where
  getModifiersFor _ (MinionRef mid) _ = do
    inheritor <- selectAny (MinionWithId mid <> MinionWithTrait Inheritor)
    pure $ if inheritor then [AccelerationIconModifier 1] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Bora where
  runMessage msg m@(Bora attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      webWarrior <- selectAny (CharacterWithTrait WebWarrior)
      when webWarrior do
        schemes <- selectList AnyScheme
        for_ schemes (`placeThreat` 1)
      Bora <$> runMessage msg attrs
    _ -> Bora <$> runMessage msg attrs
