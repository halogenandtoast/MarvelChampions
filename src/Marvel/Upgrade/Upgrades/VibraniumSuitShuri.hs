module Marvel.Upgrade.Upgrades.VibraniumSuitShuri where
import Marvel.Prelude
import Marvel.Query
import Marvel.Id
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.BlackPantherShuri.Helpers ()
import Marvel.Count

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Upgrade.Types

vibraniumSuitShuri :: UpgradeCard VibraniumSuitShuri
vibraniumSuitShuri = upgrade VibraniumSuitShuri Cards.vibraniumSuitShuri
newtype VibraniumSuitShuri = VibraniumSuitShuri (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage VibraniumSuitShuri where
  runMessage msg u@(VibraniumSuitShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> attackGuard you u do
      damage <- selectCount SustainedDamage (IdentityWithId you)
      enemies <- selectList AttackableEnemy
      healable <- characterMatches CharacterCanBeHealed (IdentityCharacter you)
      when (damage > 0 && healable) $ chooseOne you [targeting (enemyRef enemy) (heal you 1 >> dealDamage enemy attrs (toDamage 1 $ FromPlayerAttack you)) | enemy <- enemies]
      cardSpecific attrs "tough" you
      pure u
    CardSpecific (isTarget attrs -> True) "tough" (fromJSON -> Success (you :: IdentityId)) -> do
      possible <- characterMatches CharacterCanGainTough (IdentityCharacter you)
      when possible $ chooseOne you [labeled "$label.done" (pure ()), targeting attrs (discardUpgrade (upgradeId attrs) >> becomeTough you)]
      pure u
    _ -> VibraniumSuitShuri <$> runMessage msg attrs

instance HasAbilities VibraniumSuitShuri where
  getAbilities _ = []

instance IsUpgrade VibraniumSuitShuri where
  toUpgradeAttrs (VibraniumSuitShuri attrs) = attrs
  upgradeSpecialCanResolve (VibraniumSuitShuri attrs) = do
    let you = upgradeController attrs
    move <- (&&) <$> characterMatches CharacterCanBeHealed (IdentityCharacter you) <*> selectAny AttackableEnemy
    tough <- characterMatches CharacterCanGainTough (IdentityCharacter you)
    pure $ move || tough
