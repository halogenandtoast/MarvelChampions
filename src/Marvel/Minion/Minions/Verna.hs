module Marvel.Minion.Minions.Verna where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage
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

verna :: MinionCard Verna
verna = minion Verna Cards.verna (Sch 1) (Atk 1) (HP 6)

newtype Verna = Verna (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Verna where
  getModifiersFor _ (MinionRef mid) _ = do
    inheritor <- selectAny (MinionWithId mid <> MinionWithTrait Inheritor)
    pure $ if inheritor then [KeywordModifier (Retaliate 1)] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Verna where
  runMessage msg (Verna attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      webWarrior <- selectAny (CharacterWithTrait WebWarrior)
      when webWarrior do
        characters <- selectList (CharacterControlledBy $ IdentityWithId you)
        for_ characters \case
          IdentityCharacter ident -> damageIdentity ident attrs (toDamage 1 FromAbility)
          character -> damageCharacter character attrs (toDamage 1 FromAbility)
      Verna <$> runMessage msg attrs
    _ -> Verna <$> runMessage msg attrs
