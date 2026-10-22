module Marvel.Treachery.Treacheries.Pummel (
  pummel,
  Pummel (..),
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

pummel :: TreacheryCard Pummel
pummel = treachery (Pummel . (`With` Meta Nothing False)) Cards.pummel

data Meta = Meta {piledriver :: Maybe VillainId, applied :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Pummel = Pummel (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery Pummel where
  toTreacheryAttrs (Pummel (attrs `With` _)) = attrs

-- | When Revealed (Alter-Ego): Piledriver schemes; if he is tough, he gets +2 SCH for this
-- activation. (Hero): Piledriver attacks you; if he is tough, he gets +2 ATK for this
-- activation. The bonus is added as his boost cards are turned up.
instance RunMessage Pummel where
  runMessage msg t@(Pummel (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity you
      villainNamed "Piledriver" >>= \case
        Nothing -> pure t
        Just v -> do
          tough <- selectAny (VillainWithId v <> VillainWithToughStatus)
          if isHero then enemyAttacks v you else villainSchemes v
          pure $ Pummel (attrs `With` meta {piledriver = if tough then Just v else Nothing})
    VillainMessage v VillainFlipBoostCards | piledriver meta == Just v && not (applied meta) -> do
      addBoost v 2
      pure $ Pummel (attrs `With` meta {applied = True})
    _ -> Pummel . (`With` meta) <$> runMessage msg attrs
