module Marvel.Treachery.Treacheries.GetWrecked (
  getWrecked,
  GetWrecked (..),
) where

import Marvel.Prelude

import Control.Monad ()
import Data.Maybe ()
import Data.Aeson ()
import Marvel.Attack ()
import Marvel.Card
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Data.List ()
import Marvel.Id ()
import Marvel.Identity.Types ()
import Marvel.Projection ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query ()
import Marvel.Ref
import Marvel.Status ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

getWrecked :: TreacheryCard GetWrecked
getWrecked = treachery GetWrecked Cards.getWrecked

newtype GetWrecked = GetWrecked (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed (Alter-Ego): the villain whose side scheme has the most threat schemes.
-- (Hero): the villain whose side scheme has the least threat attacks you.
instance RunMessage GetWrecked where
  runMessage msg t@(GetWrecked attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity you
      if isHero
        then withVillainBySideSchemeThreat LeastThreat (`enemyAttacks` you)
        else withVillainBySideSchemeThreat MostThreat villainSchemes
      pure t
    _ -> GetWrecked <$> runMessage msg attrs
