module Marvel.Treachery.Treacheries.WeaponMaster (
  weaponMaster,
  WeaponMaster (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Trait
import Marvel.Ref (IsRef)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

weaponMaster :: TreacheryCard WeaponMaster
weaponMaster = treachery WeaponMaster Cards.weaponMaster

newtype WeaponMaster = WeaponMaster (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-ego: the villain schemes; hero: the villain attacks you. If they have a Weapon
-- attachment, this card gains surge.
instance RunMessage WeaponMaster where
  runMessage msg t@(WeaponMaster attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      if hero then currentVillainAttacks you else currentVillainSchemes you
      cardSpecific attrs "checkWeapon" ()
      pure t
    CardSpecific (isTarget attrs -> True) "checkWeapon" _ -> do
      villain <- selectOne ActiveVillain
      armed <- maybe (pure False) (\vid -> selectAny $ AttachmentOnEnemy (EnemyVillainId vid) <> AttachmentWithTrait Weapon) villain
      pure . WeaponMaster $ attrs & surgeL .~ armed
    _ -> WeaponMaster <$> runMessage msg attrs
