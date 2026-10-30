module Marvel.Minion.Minions.Jennix where

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

jennix :: MinionCard Jennix
jennix = minion Jennix Cards.jennix (Sch 2) (Atk 2) (HP 6)

newtype Jennix = Jennix (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Jennix where
  getModifiersFor _ (MinionRef mid) _ = do
    inheritor <- selectAny (MinionWithId mid <> MinionWithTrait Inheritor)
    pure $ if inheritor then [KeywordModifier Guard] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Jennix where
  runMessage msg m@(Jennix attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      webWarrior <- selectAny (CharacterWithTrait WebWarrior)
      when webWarrior do
        enemyBecomesTough (minionId attrs)
      Jennix <$> runMessage msg attrs
    _ -> Jennix <$> runMessage msg attrs
