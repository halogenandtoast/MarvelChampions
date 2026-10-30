module Marvel.Minion.Minions.Brix where

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

brix :: MinionCard Brix
brix = minion Brix Cards.brix (Sch 1) (Atk 2) (HP 5)

newtype Brix = Brix (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Brix where
  getModifiersFor _ (MinionRef mid) _ = do
    inheritor <- selectAny (MinionWithId mid <> MinionWithTrait Inheritor)
    pure $ if inheritor then [KeywordModifier Patrol] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Brix where
  runMessage msg m@(Brix attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      webWarrior <- selectAny (CharacterWithTrait WebWarrior)
      when webWarrior do
        schemes <- selectList EncounterMainScheme
        for_ schemes (`placeThreat` 2)
      Brix <$> runMessage msg attrs
    _ -> Brix <$> runMessage msg attrs
