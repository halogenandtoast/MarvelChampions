module Marvel.Treachery.Treacheries.MindRay (
  mindRay,
  MindRay (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

mindRay :: TreacheryCard MindRay
mindRay = treachery MindRay Cards.mindRay

newtype MindRay = MindRay (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Alter-ego: Zola schemes, and you are confused. Hero: Zola attacks you, and you are
-- stunned.
instance RunMessage MindRay where
  runMessage msg t@(MindRay attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | treacheryId attrs == ident -> do
      villain <- selectJust ActiveVillain
      isHero <- identityMatches HeroIdentity you
      if isHero
        then enemyAttacks villain you >> stunIdentity you
        else villainSchemes villain >> confuseIdentity you
      pure t
    _ -> MindRay <$> runMessage msg attrs
