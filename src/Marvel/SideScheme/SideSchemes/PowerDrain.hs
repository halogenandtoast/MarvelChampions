module Marvel.SideScheme.SideSchemes.PowerDrain (
  powerDrain,
  PowerDrain (..),
) where

import Marvel.Prelude

import Data.Aeson (Result (..))
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

powerDrain :: SideSchemeCard PowerDrain
powerDrain = sideScheme PowerDrain Cards.powerDrain (PerPlayer 2)

newtype PowerDrain = PowerDrain (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: discard 2 cards from the encounter deck; each player discards from
-- their hand 1 resource of any type for each boost icon on them, a card counting each
-- resource it prints, until that many are discarded or their hand has none.
instance RunMessage PowerDrain where
  runMessage msg s@(PowerDrain attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      discardTopOfEncounterDeckThen 2 (countBoostIcons attrs players)
      pure s
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success players) (sum . map snd -> n) -> do
      when (n > 0) $ for_ (players :: [IdentityId]) \p -> cardSpecific attrs "drain" (p, n)
      pure s
    CardSpecific (isTarget attrs -> True) "drain" (fromJSON -> Success (you :: IdentityId, n :: Natural)) -> do
      cards <- projectMap PlayerIdentityHand (filter (notNull . printedResources . getCardDef) . unHand) you
      unless (n == 0 || null cards) $
        chooseOnePrompted you "$label.chooseResourceToDiscard"
          [ targeting card do
              discardCard card
              cardSpecific attrs "drain" (you, subtractNatural (fromIntegral $ length $ printedResources $ getCardDef card) n)
          | card <- cards
          ]
      pure s
    _ -> PowerDrain <$> runMessage msg attrs
