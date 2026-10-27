-- | The Galaxy's Most Wanted's Challenge side schemes, each revealed by a scenario's
-- campaign Setup: the A side in a standard campaign, the B side in an expert one
-- ("docs/rules/campaign-galaxys-most-wanted.md"). Hinder and Victory 1 are their cards'.
module Marvel.SideScheme.SideSchemes.Challenge (
  badoonBlitz,
  badoonBlitzExpert,
  galleryOfSplendor,
  galleryOfSplendorExpert,
  thereIsNoEscape,
  thereIsNoEscapeExpert,
  guerrillaTactics,
  guerrillaTacticsExpert,
  kreeSupremacy,
  kreeSupremacyExpert,
  BadoonBlitz (..),
  GalleryOfSplendor (..),
  ThereIsNoEscape (..),
  GuerrillaTactics (..),
  KreeSupremacy (..),
) where

import Marvel.Prelude

import Data.Text qualified as T
import Marvel.Card
import Marvel.Damage
import Marvel.Deck
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (..))
import Marvel.Environment.Environments.NebulasShip (placeEvasionCounters)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import System.Random.Shuffle (shuffleM)

-- | The expert campaign's side.
isExpertSide :: Attrs SideScheme -> Bool
isExpertSide = ("b" `T.isSuffixOf`) . unCardCode . toCardCode

-- * Badoon Blitz (16178)

badoonBlitz :: SideSchemeCard BadoonBlitz
badoonBlitz = sideScheme BadoonBlitz Cards.badoonBlitz (Static 2)

badoonBlitzExpert :: SideSchemeCard BadoonBlitz
badoonBlitzExpert = sideScheme BadoonBlitz Cards.badoonBlitzExpert (Static 3)

newtype BadoonBlitz = BadoonBlitz (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: each player may draw 1 card (A); each player must choose and discard 1
-- card from their hand (B).
instance RunMessage BadoonBlitz where
  runMessage msg (BadoonBlitz attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \p ->
        if isExpertSide attrs
          then discardFromHand p 1
          else chooseOne p [labeled (countLabel "drawCards" 1) (drawCards p 1), labeled "$label.pass" (pure ())]
      BadoonBlitz <$> runMessage msg attrs
    _ -> BadoonBlitz <$> runMessage msg attrs

-- * Gallery of Splendor (16179)

galleryOfSplendor :: SideSchemeCard GalleryOfSplendor
galleryOfSplendor = sideScheme GalleryOfSplendor Cards.galleryOfSplendor (Static 3)

galleryOfSplendorExpert :: SideSchemeCard GalleryOfSplendor
galleryOfSplendorExpert = sideScheme GalleryOfSplendor Cards.galleryOfSplendorExpert (Static 4)

newtype GalleryOfSplendor = GalleryOfSplendor (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: the top card of each player's deck faceup into The Collection (A); 1
-- card at random from each player's hand (B).
instance RunMessage GalleryOfSplendor where
  runMessage msg (GalleryOfSplendor attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \p -> do
        card <-
          if isExpertSide attrs
            then headMay <$> (shuffleM =<< projectMap PlayerIdentityHand unHand p)
            else headMay <$> projectMap PlayerIdentityDeck unDeck p
        for_ card placeInCollection
      GalleryOfSplendor <$> runMessage msg attrs
    _ -> GalleryOfSplendor <$> runMessage msg attrs

-- * "There is No Escape" (16180)

thereIsNoEscape :: SideSchemeCard ThereIsNoEscape
thereIsNoEscape = sideScheme ThereIsNoEscape Cards.thereIsNoEscape (Static 4)

thereIsNoEscapeExpert :: SideSchemeCard ThereIsNoEscape
thereIsNoEscapeExpert = sideScheme ThereIsNoEscape Cards.thereIsNoEscapeExpert (Static 5)

newtype ThereIsNoEscape = ThereIsNoEscape (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: deal 1 damage to each player (A), 2 (B).
instance RunMessage ThereIsNoEscape where
  runMessage msg (ThereIsNoEscape attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \p -> damageIdentity p attrs (toDamage (if isExpertSide attrs then 2 else 1) FromAbility)
      ThereIsNoEscape <$> runMessage msg attrs
    _ -> ThereIsNoEscape <$> runMessage msg attrs

-- * Guerrilla Tactics (16181)

guerrillaTactics :: SideSchemeCard GuerrillaTactics
guerrillaTactics = sideScheme GuerrillaTactics Cards.guerrillaTactics (Static 5)

guerrillaTacticsExpert :: SideSchemeCard GuerrillaTactics
guerrillaTacticsExpert = sideScheme GuerrillaTactics Cards.guerrillaTacticsExpert (Static 6)

newtype GuerrillaTactics = GuerrillaTactics (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: place 2 evasion counters on Nebula's Ship (A), 3 (B).
instance RunMessage GuerrillaTactics where
  runMessage msg (GuerrillaTactics attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      placeEvasionCounters (if isExpertSide attrs then 3 else 2)
      GuerrillaTactics <$> runMessage msg attrs
    _ -> GuerrillaTactics <$> runMessage msg attrs

-- * Kree Supremacy (16182): no text but its keywords

kreeSupremacy :: SideSchemeCard KreeSupremacy
kreeSupremacy = sideScheme KreeSupremacy Cards.kreeSupremacy (Static 6)

kreeSupremacyExpert :: SideSchemeCard KreeSupremacy
kreeSupremacyExpert = sideScheme KreeSupremacy Cards.kreeSupremacyExpert (Static 7)

newtype KreeSupremacy = KreeSupremacy (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage KreeSupremacy where
  runMessage msg (KreeSupremacy attrs) = runQueueT $ KreeSupremacy <$> runMessage msg attrs
