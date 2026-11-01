module Marvel.Minion.Minions.Solus where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Status ()
import Marvel.Trait (Trait (Inheritor, WebWarrior))
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

solus :: MinionCard Solus
solus = minion Solus Cards.solus (Sch 2) (Atk 3) (HP 7)

newtype Solus = Solus (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Solus where
  getModifiersFor _ (MinionRef mid) _ = do
    inheritor <- selectAny (MinionWithId mid <> MinionWithTrait Inheritor)
    pure $ if inheritor then [KeywordModifier Villainous] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Solus where
  runMessage msg (Solus attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion _) | mid == minionId attrs -> do
      webWarrior <- selectAny (CharacterWithTrait WebWarrior)
      when webWarrior do
        dealBoost (minionId attrs)
      Solus <$> runMessage msg attrs
    _ -> Solus <$> runMessage msg attrs
