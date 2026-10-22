module Marvel.Treachery.Treacheries.InfinitePower (
  infinitePower,
  InfinitePower (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

infinitePower :: TreacheryCard InfinitePower
infinitePower = treachery InfinitePower Cards.infinitePower

newtype InfinitePower = InfinitePower (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Give Red Skull a tough status card; then he schemes (alter-ego) or attacks you (hero).
instance RunMessage InfinitePower where
  runMessage msg t@(InfinitePower attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      villain <- selectJust ActiveVillain
      isHero <- identityMatches HeroIdentity you
      enemyBecomesTough villain
      if isHero then enemyAttacks villain you else villainSchemes villain
      pure t
    _ -> InfinitePower <$> runMessage msg attrs
