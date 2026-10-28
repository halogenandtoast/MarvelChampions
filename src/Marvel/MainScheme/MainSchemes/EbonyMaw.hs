-- | Ebony Maw's main schemes (The Mad Titan's Shadow, scenario 1).
module Marvel.MainScheme.MainSchemes.EbonyMaw (
  attackOnKnowhere,
  AttackOnKnowhere (..),
  thePowerStoneScheme,
  ThePowerStoneScheme (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Environment.Environments.EbonyMawSpells (eachPlayerPutsSpellIntoPlay)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted
import Marvel.Ref

attackOnKnowhere :: MainSchemeCard AttackOnKnowhere
attackOnKnowhere = mainScheme AttackOnKnowhere Cards.attackOnKnowhere (PerPlayer 6) (PerPlayer 1) (PerPlayer 1)

newtype AttackOnKnowhere = AttackOnKnowhere (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1B When Revealed: each player discards cards from the top of the encounter deck
-- until they discard a Spell card and puts that card into play in their play area.
-- Shuffle the encounter discard pile into the encounter deck.
instance RunMessage AttackOnKnowhere where
  runMessage msg ms@(AttackOnKnowhere attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      eachPlayerPutsSpellIntoPlay
      -- the discard pile as the players left it
      cardSpecific (MainSchemeRef ident) "shuffleDiscardBack" ()
      pure ms
    CardSpecific (isTarget attrs -> True) "shuffleDiscardBack" _ -> do
      shuffleIntoEncounterDeck =<< getEncounterDiscard
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ nextMainScheme (mainSchemeId attrs)
    _ -> AttackOnKnowhere <$> runMessage msg attrs

thePowerStoneScheme :: MainSchemeCard ThePowerStoneScheme
thePowerStoneScheme = mainScheme ThePowerStoneScheme Cards.thePowerStoneScheme (PerPlayer 9) (PerPlayer 1) (PerPlayer 1)

newtype ThePowerStoneScheme = ThePowerStoneScheme (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 2A When Revealed: shuffle the encounter discard pile into the encounter deck. Each
-- player discards cards from the top of the encounter deck until they discard a Spell
-- card and puts that card into play in their play area. 2B completed: the players lose.
instance RunMessage ThePowerStoneScheme where
  runMessage msg ms@(ThePowerStoneScheme attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      shuffleIntoEncounterDeck =<< getEncounterDiscard
      eachPlayerPutsSpellIntoPlay
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> ThePowerStoneScheme <$> runMessage msg attrs
