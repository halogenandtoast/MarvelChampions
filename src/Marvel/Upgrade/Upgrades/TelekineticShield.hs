module Marvel.Upgrade.Upgrades.TelekineticShield where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

telekineticShield :: UpgradeCard TelekineticShield
telekineticShield = upgrade TelekineticShield Cards.telekineticShield

newtype TelekineticShield = TelekineticShield (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade TelekineticShield where
  upgradeTargets _ = Just . map characterRef <$> selectList (CharacterControlledBy AnyIdentity)

instance HasAbilities TelekineticShield where
  getAbilities (TelekineticShield a) =
    [limitedWindowAbility a 1 matcher ForcedInterrupt OwnsThis NoCost (runAbility a 1) | Just matcher <- [hostWindow]]
   where
    hostWindow = case (upgradeAttachedIdentity a, upgradeAttachedAlly a) of
      (Just you, _) -> Just $ W.CharacterWouldTakeAttackDamage (CharacterWithId $ IdentityCharacter you)
      (_, Just aid) -> Just $ W.CharacterWouldTakeAttackDamage (CharacterWithId $ AllyCharacter aid)
      _ -> Nothing

instance RunMessage TelekineticShield where
  runMessage msg e@(TelekineticShield a) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade (Just (IdentityRef host))) | uid == upgradeId a -> do
      upgradeStep uid (UpgradeControlledByPlayer host)
      upgradeStep uid (UpgradeAttachedToIdentity host)
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      let host = (IdentityRef <$> upgradeAttachedIdentity a) <|> (AllyRef <$> upgradeAttachedAlly a)
      amounts <- for host pendingAttackDamage
      let n = fromMaybe 0 amounts
      for_ (upgradeAttachedIdentity a) (\you -> preventDamage you n)
      for_ (upgradeAttachedAlly a) (\aid -> preventAllyDamage aid n)
      when (upgradeUses a + n >= 5) $ removeFromPlay a
      pure $ TelekineticShield a {upgradeUses = upgradeUses a + n}
    _ -> TelekineticShield <$> runMessage msg a
