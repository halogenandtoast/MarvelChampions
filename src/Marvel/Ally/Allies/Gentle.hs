module Marvel.Ally.Allies.Gentle where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Entity ()
import Marvel.Id
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.Window qualified as W

gentle :: AllyCard Gentle
gentle = ally (Gentle . (`With` False)) Cards.gentle (Thw 1, 1) (Atk 3, 1) (HP 3)

newtype Gentle = Gentle (Attrs Ally `With` Bool)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAlly Gentle where
  toAllyAttrs (Gentle (a `With` _)) = a
instance IsRef Gentle where
  toRef = toRef . toAllyAttrs

instance HasModifiersFor Gentle where
  getModifiersFor _ (AllyRef aid) (Gentle (a `With` villain))
    | aid == allyId a = pure [AttackConsequentialDamageModifier 1 | villain]
  getModifiersFor _ _ _ = pure []

instance RunMessage Gentle where
  runMessage msg (Gentle (attrs `With` villain)) = runQueueT $ case msg of
    CheckWindows ws | or [aid == allyId attrs | W.Window W.When (W.AllyAttack aid (EnemyVillainId _)) <- ws] ->
      Gentle . (`With` True) <$> runMessage msg attrs
    AllyMessage aid action | aid == allyId attrs, (case action of AllyAttacked -> True; AllyAttackConsequential -> True; AllyAttackConsequentialAgainst _ -> True; _ -> False) -> do
      attrs' <- runMessage msg attrs
      pure $ Gentle (attrs' `With` False)
    _ -> Gentle . (`With` villain) <$> runMessage msg attrs
