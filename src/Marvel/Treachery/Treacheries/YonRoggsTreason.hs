module Marvel.Treachery.Treacheries.YonRoggsTreason (
  yonRoggsTreason,
  YonRoggsTreason (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (..))
import Marvel.Message
import Marvel.Projection
import Marvel.Queue
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

yonRoggsTreason :: TreacheryCard YonRoggsTreason
yonRoggsTreason = treachery YonRoggsTreason Cards.yonRoggsTreason

newtype YonRoggsTreason = YonRoggsTreason (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

isEnergy :: (HasCardDef a) => a -> Bool
isEnergy = any (== Energy) . printedResources . getCardDef

instance RunMessage YonRoggsTreason where
  runMessage msg (YonRoggsTreason attrs) = case msg of
    TreacheryMessage ident msg' | ident == treacheryId attrs -> case msg' of
      RevealTreachery identityId -> do
        energyResources <- filter isEnergy <$> projectMap PlayerIdentityHand unHand identityId
        pushAll [DiscardedCard $ PlayerCard c | c <- energyResources]
        pure . YonRoggsTreason $ attrs & surgeL .~ null energyResources
      _ -> YonRoggsTreason <$> runMessage msg attrs
    _ -> YonRoggsTreason <$> runMessage msg attrs
