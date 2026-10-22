module Marvel.Treachery.Treacheries.ChaosInThePrison (
  chaosInThePrison,
  ChaosInThePrison (..),
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

chaosInThePrison :: TreacheryCard ChaosInThePrison
chaosInThePrison = treachery ChaosInThePrison Cards.chaosInThePrison

newtype ChaosInThePrison = ChaosInThePrison (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: choose to either discard an upgrade you control or place 1 threat on
-- the active villain's side scheme for each upgrade you control. If you control none, this
-- card gains surge.
-- [star] Boost: if this attack is undefended, discard an upgrade you control.
instance RunMessage ChaosInThePrison where
  runMessage msg t@(ChaosInThePrison attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
      scheme <- selectOne (SignatureSchemeOf ActiveVillain)
      if null upgrades
        then pure . ChaosInThePrison $ attrs & surgeL .~ True
        else do
          chooseOne you $
            labeled "$label.discardAnUpgradeYouControl" (chooseOne you [targeting u (discardUpgrade u) | u <- upgrades])
              : [ labeled "$label.placeThreatForEachUpgrade" (placeThreat s (fromIntegral $ length upgrades))
                | s <- maybeToList scheme
                ]
          pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      undefended <- selectAny (EnemyWithId enemy <> UndefendedEnemy)
      you <- getBoostedPlayer enemy
      upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
      when (undefended && notNull upgrades) $
        chooseOne you [targeting u (discardUpgrade u) | u <- upgrades]
      pure t
    _ -> ChaosInThePrison <$> runMessage msg attrs
