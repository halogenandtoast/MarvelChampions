module Marvel.Minion.Minions.Morlun where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Status ()
import Marvel.Trait (Trait (Inheritor, WebWarrior))
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

morlun :: MinionCard Morlun
morlun = minion Morlun Cards.morlun (Sch 2) (Atk 2) (HP 5)

newtype Morlun = Morlun (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Morlun where
  getModifiersFor _ (MinionRef mid) _ = do
    inheritor <- selectAny (MinionWithId mid <> MinionWithTrait Inheritor)
    pure $ if inheritor then [AttackModifier 1] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Morlun where
  runMessage msg (Morlun attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      webWarrior <- selectAny (CharacterWithTrait WebWarrior)
      when webWarrior do
        damageIdentity you attrs (toDamage 2 FromAbility)
      Morlun <$> runMessage msg attrs
    _ -> Morlun <$> runMessage msg attrs
