module Marvel.Treachery.Treacheries.YonRoggsTreason
  ( yonRoggsTreason
  , YonRoggsTreason(..)
  ) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field(..))
import Marvel.Message
import Marvel.Projection
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

yonRoggsTreason :: TreacheryCard YonRoggsTreason
yonRoggsTreason = treachery YonRoggsTreason Cards.yonRoggsTreason

newtype YonRoggsTreason = YonRoggsTreason (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage YonRoggsTreason where
  runMessage msg t@(YonRoggsTreason attrs) = case msg of
    TreacheryMessage ident msg' | ident == treacheryId attrs -> case msg' of
      RevealTreachery identityId -> do
        energyResources <- projectMap PlayerIdentityHand unHand identityId
        chooseOneAtATime identityId
          $ [ TargetLabel (CardIdTarget $ toCardId c) [DiscardCard $ PlayerCard c]
            | c <- energyResources
            ]
        pure t
      _ -> YonRoggsTreason <$> runMessage msg attrs
    _ -> YonRoggsTreason <$> runMessage msg attrs
