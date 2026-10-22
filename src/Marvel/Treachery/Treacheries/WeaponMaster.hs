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
      villain <- selectOne ActiveVillain
      case villain of
        Nothing -> pure t
        Just vid -> do
          hero <- identityMatches HeroIdentity you
          armed <- selectAny $ AttachmentOnEnemy (EnemyVillainId vid) <> AttachmentWithTrait Weapon
          if hero then enemyAttacks vid you else villainSchemes vid
          pure . WeaponMaster $ attrs & surgeL .~ armed
    _ -> WeaponMaster <$> runMessage msg attrs
