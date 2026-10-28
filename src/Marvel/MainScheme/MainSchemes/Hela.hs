-- | Hela's main scheme (The Mad Titan's Shadow, scenario 4): Odin's Torment, Odin held
-- on it faceup, attached, until Hall of Nastrond is defeated.
module Marvel.MainScheme.MainSchemes.Hela (
  odinsTorment,
  OdinsTorment (..),
) where

import Marvel.Prelude

import Data.Aeson (Result (..), fromJSON)
import Data.List (partition)
import Marvel.Ally.Cards qualified as Allies
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes

odinsTorment :: MainSchemeCard OdinsTorment
odinsTorment = mainScheme OdinsTorment Cards.odinsTorment (PerPlayer 18) (PerPlayer 1) (PerPlayer 1)

newtype OdinsTorment = OdinsTorment (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1A Setup: attach Odin to the main scheme, Captive side faceup. Reveal Gnipahellir
-- and Garm. Set Gjallerbru, Skurge, Hall of Nastrond, and Nidhogg aside, out of play.
-- Shuffle the encounter deck. 1B: if this scheme is completed, the players lose; its
-- Forced Interrupt as Hela would be defeated is Hela's to answer
-- ('Marvel.Villain.Villains.Hela'). Hall of Nastrond defeated, the first player
-- detaches Odin and takes control of him.
instance RunMessage OdinsTorment where
  runMessage msg ms@(OdinsTorment attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      cardId <- getRandom
      deck <- getEncounterDeck
      let named def = take 1 [c | c <- deck, getCardDef c == def]
      for_ (concatMap named [SideSchemes.gnipahellir, Minions.garm]) (revealEncounterCard first)
      setAside $ EncounterCard <$> concatMap named [SideSchemes.gjallerbru, Minions.skurge, SideSchemes.hallOfNastrond, Minions.nidhogg]
      shuffleEncounterDeck
      pure $ OdinsTorment attrs {mainSchemeHeldCards = [MkPlayerCard cardId Allies.odinCaptive Nothing Nothing]}
    CardSpecific (isTarget attrs -> True) "detachOdin" (fromJSON -> Success first) -> do
      let (odin, rest) = partition (sameCard Allies.odinCaptive . getCardDef) (mainSchemeHeldCards attrs)
      for_ odin \card -> putIntoPlay first card {pcOwner = Just first, pcController = Just first}
      pure $ OdinsTorment attrs {mainSchemeHeldCards = rest}
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> OdinsTorment <$> runMessage msg attrs
