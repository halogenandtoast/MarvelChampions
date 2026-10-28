-- | The Loki set's side schemes (The Mad Titan's Shadow, scenario 5): each Hinder
-- 1[per hero] (its keyword), and the same When Defeated.
module Marvel.SideScheme.SideSchemes.Loki (
  casketOfAncientWinters,
  CasketOfAncientWinters (..),
  warInAsgard,
  WarInAsgard (..),
  madnessOnMidgard,
  MadnessOnMidgard (..),
  openTheBifrost,
  OpenTheBifrost (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | When Defeated: reveal the top card of the Infinity Stone deck (the active player
-- revealing it). Swap Loki with a random set-aside Loki villain.
whenDefeated :: (ReverseQueue m) => Attrs SideScheme -> Message -> Maybe (m ())
whenDefeated attrs = \case
  SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> Just do
    revealTopInfinityStone =<< getActivePlayerId
    traverse_ swapVillain =<< selectOne ActiveVillain
  _ -> Nothing

casketOfAncientWinters :: SideSchemeCard CasketOfAncientWinters
casketOfAncientWinters = sideScheme CasketOfAncientWinters Cards.casketOfAncientWinters (Static 4)

newtype CasketOfAncientWinters = CasketOfAncientWinters (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage CasketOfAncientWinters where
  runMessage msg (CasketOfAncientWinters attrs) = runQueueT do
    sequence_ (whenDefeated attrs msg)
    CasketOfAncientWinters <$> runMessage msg attrs

warInAsgard :: SideSchemeCard WarInAsgard
warInAsgard = sideScheme WarInAsgard Cards.warInAsgard (Static 6)

newtype WarInAsgard = WarInAsgard (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage WarInAsgard where
  runMessage msg (WarInAsgard attrs) = runQueueT do
    sequence_ (whenDefeated attrs msg)
    WarInAsgard <$> runMessage msg attrs

madnessOnMidgard :: SideSchemeCard MadnessOnMidgard
madnessOnMidgard = sideScheme MadnessOnMidgard Cards.madnessOnMidgard (Static 5)

newtype MadnessOnMidgard = MadnessOnMidgard (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage MadnessOnMidgard where
  runMessage msg (MadnessOnMidgard attrs) = runQueueT do
    sequence_ (whenDefeated attrs msg)
    MadnessOnMidgard <$> runMessage msg attrs

openTheBifrost :: SideSchemeCard OpenTheBifrost
openTheBifrost = sideScheme OpenTheBifrost Cards.openTheBifrost (Static 3)

newtype OpenTheBifrost = OpenTheBifrost (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage OpenTheBifrost where
  runMessage msg (OpenTheBifrost attrs) = runQueueT do
    sequence_ (whenDefeated attrs msg)
    OpenTheBifrost <$> runMessage msg attrs
