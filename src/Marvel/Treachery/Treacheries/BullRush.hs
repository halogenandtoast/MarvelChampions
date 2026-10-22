module Marvel.Treachery.Treacheries.BullRush (
  bullRush,
  BullRush (..),
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

bullRush :: TreacheryCard BullRush
bullRush = treachery (BullRush . (`With` Meta Nothing False 0)) Cards.bullRush

data Meta = Meta {bulldozer :: Maybe VillainId, scheming :: Bool, dealt :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BullRush = BullRush (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery BullRush where
  toTreacheryAttrs (BullRush (attrs `With` _)) = attrs

-- | When Revealed (Alter-Ego): Bulldozer schemes; discard the top card of your deck for
-- each threat placed by this activation. (Hero): Bulldozer attacks you; discard the top
-- card of your deck for each damage dealt by this attack.
instance RunMessage BullRush where
  runMessage msg t@(BullRush (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity you
      villainNamed "Bulldozer" >>= \case
        Nothing -> pure t
        Just v -> do
          if isHero then enemyAttacks v you else villainSchemes v
          cardSpecific attrs "discard" you
          pure $ BullRush (attrs `With` meta {bulldozer = Just v})
    -- the threat his scheming places, as it is placed
    VillainMessage v VillainSchemed | bulldozer meta == Just v -> pure $ BullRush (attrs `With` meta {scheming = True})
    SideSchemeMessage _ (SideSchemePlaceThreat n) | scheming meta -> placed n
    MainSchemeMessage _ (MainSchemePlaceThreat n) | scheming meta -> placed n
    (attackDamageDealt -> Just (EnemyVillainId v, _, n)) | bulldozer meta == Just v ->
      pure $ BullRush (attrs `With` meta {dealt = dealt meta + n})
    CardSpecific (isTarget attrs -> True) "discard" (fromJSON -> Success you) -> do
      when (dealt meta > 0) $ discardTopOfDeck you (dealt meta)
      pure $ BullRush (attrs `With` meta {dealt = 0, bulldozer = Nothing})
    _ -> BullRush . (`With` meta) <$> runMessage msg attrs
   where
    placed n = pure $ BullRush (attrs `With` meta {scheming = False, dealt = dealt meta + n})
