module Marvel.MainScheme.MainSchemes.Breakout where

import Marvel.Prelude

import Data.List (partition)
import Marvel.Card
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref

breakout :: MainSchemeCard Breakout
breakout = mainScheme Breakout Cards.breakout (PerPlayer 6) (Static 0) (Static 1)

newtype Breakout = Breakout (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | 1A Setup: each villain's side scheme is put into play as his own, the active counter
-- placed on Wrecker; the villains' encounter decks are made, each from his own set (TWC
-- insert pp.3-4).
--
-- 1B [star] Forced Response: after step one of the villain phase, place 1 threat on each
-- side scheme, then move the active counter to the villain whose scheme has the most
-- threat (the first player choosing among those tied). If this stage is completed, the
-- players lose.
instance RunMessage Breakout where
  runMessage msg ms@(Breakout attrs) = runQueueT $ case msg of
    -- nemesis cards and obligations are not used (TWC insert p.4)
    SetupMainScheme -> do
      removePlayerObligationsFromGame
      first <- selectJust FirstPlayer
      villains <- selectList AnyVillain
      for_ villains \v -> do
        def <- getsGame (`abilityCardDef` VillainRef v)
        for_ (cdEncounterSet =<< def) \set -> do
          (schemes, deck) <- partition ((== SideSchemeType) . cdCardType . getCardDef) <$> gatherEncounterSet set
          for_ schemes \card -> do
            putEncounterCardIntoPlay first card
            setSignatureScheme v (SideSchemeId $ unCardId $ ecCardId card)
          createDeck (VillainDeckKey v) deck
        when (fmap cdName def == Just "Wrecker") $ setActiveVillain v
      pure ms
    AccelerateMainScheme -> do
      ms' <- Breakout <$> runMessage msg attrs
      forcedResponse (mainSchemeId attrs) do
        schemes <- selectList AnySideScheme
        for_ schemes \s -> placeThreat s 1
        -- once the threat is there
        cardSpecific attrs "moveActive" ()
      pure ms'
    CardSpecific (isTarget attrs -> True) "moveActive" _ -> do
      moveActiveCounter MostThreat
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> Breakout <$> runMessage msg attrs
