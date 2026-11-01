module Marvel.Treachery.Treacheries.EscapePlan (
  escapePlan,
  EscapePlan (..),
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
import Marvel.Query
import Marvel.Ref
import Marvel.Status ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

escapePlan :: TreacheryCard EscapePlan
escapePlan = treachery EscapePlan Cards.escapePlan

newtype EscapePlan = EscapePlan (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: you are confused. If you are already confused, Piledriver schemes.
-- [star] Boost: give the active villain a tough status card. If they already have one,
-- place 2 threat on their scheme.
instance RunMessage EscapePlan where
  runMessage msg t@(EscapePlan attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      confused <- identityMatches ConfusedIdentity you
      if confused
        then traverse_ villainSchemes =<< villainNamed "Piledriver"
        else confuseIdentity you
      pure t
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      selectOne ActiveVillain >>= traverse_ \v -> do
        tough <- selectAny (VillainWithId v <> VillainWithToughStatus)
        if tough
          then traverse_ (`placeThreat` 2) =<< signatureSchemeOf v
          else enemyBecomesTough v
      pure t
    _ -> EscapePlan <$> runMessage msg attrs
