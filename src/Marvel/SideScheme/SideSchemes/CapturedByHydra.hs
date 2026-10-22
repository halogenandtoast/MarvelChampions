module Marvel.SideScheme.SideSchemes.CapturedByHydra (
  capturedByHydra,
  CapturedByHydra (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait
import System.Random.Shuffle (shuffleM)

capturedByHydra :: SideSchemeCard CapturedByHydra
capturedByHydra = sideScheme CapturedByHydra Cards.capturedByHydra (Static 5)

newtype CapturedByHydra = CapturedByHydra (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | "When Revealed: Place 1 random set-aside Captive ally facedown beneath this scheme.
-- When this scheme is defeated, the player who defeated it takes that ally into their
-- hand and removes this scheme from the game." The ally is theirs from then on.
instance RunMessage CapturedByHydra where
  runMessage msg s@(CapturedByHydra attrs) = runQueueT $ case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        captives <- shuffleM . filter (cardMatch $ CardWithTrait Captive) =<< getSetAsidePlayerCards
        for_ (take 1 captives) \c -> removeFromGame (CardIdRef $ pcCardId c)
        pure . CapturedByHydra $ attrs & heldCardsL <>~ take 1 captives
      DefeatSideScheme -> do
        rescuer <- getDefeatedBy attrs
        for_ (sideSchemeHeldCards attrs) (takeControlOf rescuer)
        removeFromGame attrs
        pure s
      _ -> CapturedByHydra <$> runMessage msg attrs
    _ -> CapturedByHydra <$> runMessage msg attrs
