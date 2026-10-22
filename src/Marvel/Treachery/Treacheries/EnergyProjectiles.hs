module Marvel.Treachery.Treacheries.EnergyProjectiles (
  energyProjectiles,
  EnergyProjectiles (..),
) where

import Marvel.Prelude

import Control.Monad (replicateM_)
import Data.Maybe (maybeToList)
import Data.Aeson (Result (..), fromJSON)
import Marvel.Attack
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Data.List (nub)
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDefeated))
import Marvel.Projection (projectP)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Status
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

energyProjectiles :: TreacheryCard EnergyProjectiles
energyProjectiles = treachery EnergyProjectiles Cards.energyProjectiles

newtype EnergyProjectiles = EnergyProjectiles (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: deal 1 damage to each friendly character you control.
-- [star] Boost: deal 1 damage to the defending character.
instance RunMessage EnergyProjectiles where
  runMessage msg t@(EnergyProjectiles attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      damageAllCharacters attrs (toDamage 1 FromTreachery) (CharacterControlledBy $ IdentityWithId you)
      pure t
    Boost (RevealedAsBoost target (EnemyVillainId v)) | isTarget attrs target -> do
      getVillainAttack v >>= traverse_ \a ->
        when (attackDefended a) $ damageCharacter (attackCharacter a) attrs (toDamage 1 FromTreachery)
      pure t
    _ -> EnergyProjectiles <$> runMessage msg attrs
