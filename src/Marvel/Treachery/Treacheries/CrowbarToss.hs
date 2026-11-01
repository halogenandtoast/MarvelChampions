module Marvel.Treachery.Treacheries.CrowbarToss (
  crowbarToss,
  CrowbarToss (..),
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

crowbarToss :: TreacheryCard CrowbarToss
crowbarToss = treachery CrowbarToss Cards.crowbarToss

newtype CrowbarToss = CrowbarToss (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed (Alter-Ego): Wrecker schemes. (Hero): Wrecker attacks you. Then, move
-- the active counter to the villain whose side scheme has the least threat.
instance RunMessage CrowbarToss where
  runMessage msg t@(CrowbarToss attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity you
      villainNamed "Wrecker" >>= traverse_ \w -> if isHero then enemyAttacks w you else villainSchemes w
      -- once the activation is over
      cardSpecific attrs "move" ()
      pure t
    CardSpecific (isTarget attrs -> True) "move" _ -> t <$ moveActiveCounter LeastThreat
    _ -> CrowbarToss <$> runMessage msg attrs
