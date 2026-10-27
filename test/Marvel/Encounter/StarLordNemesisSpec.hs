module Marvel.Encounter.StarLordNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Phase (Phase (..))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

inVillainPhase :: GameAppT ()
inVillainPhase = withGame_ (phaseL .~ VillainPhase)

-- | Reveal Spartoi Cunning for the player, a Hydra Mercenary on top of the encounter
-- deck: whether it surged, dealing the player the Mercenary facedown.
surges :: IdentityId -> GameAppT Bool
surges p = do
  [next] <- stackEncounterDeck [Minions.hydraMercenary]
  _ <- revealNow p Treacheries.spartoiCunning
  pass
  elem (ecCardId next) . map ecCardId <$> dealtTo p

spec :: Spec
spec = describe "Star-Lord nemesis" do
  describe "Budding Crime Syndicate (17025)" do
    it "enters play with 2 threat and hinder 2 per player: 4 with one player" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        scheme <- revealNow p SideSchemes.buddingCrimeSyndicate
        pass
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4

    it "and 6 with two" do
      tg <- testGame ["spider-man", "black-panther"]
      let p = player1 tg
      inGame tg do
        scheme <- revealNow p SideSchemes.buddingCrimeSyndicate
        pass
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 6

  describe "Mister Knife (17026)" do
    it "the first treachery the engaged player reveals in the villain phase gains surge" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.misterKnife
        pass
        inVillainPhase
        surges p `shouldReturn` True

    it "the second does not" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.misterKnife
        pass
        inVillainPhase
        surges p `shouldReturn` True
        surges p `shouldReturn` False

    it "a treachery revealed before he was in play was the first" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        inVillainPhase
        surges p `shouldReturn` False
        _ <- addMinion p Minions.misterKnife
        pass
        surges p `shouldReturn` False

    it "not in the player phase" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.misterKnife
        pass
        surges p `shouldReturn` False

    it "another player's treachery neither surges nor counts for the engaged player" do
      tg <- testGame ["spider-man", "black-panther"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- addMinion p Minions.misterKnife
        pass
        inVillainPhase
        surges other `shouldReturn` False
        surges p `shouldReturn` True

    it "Shadow of the Past, which brings him out, gains nothing (FAQ)" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        inVillainPhase
        before <- length <$> scenarioEncounterDeckNow
        _ <- revealNow p Treacheries.shadowOfThePast
        pass
        getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMinions) `shouldReturn` ["17026"]
        -- the three Spartoi Cunning shuffled in, nothing revealed by a surge
        length <$> scenarioEncounterDeckNow `shouldReturn` (before + 3)
        -- and it was his player's first treachery this phase
        surges p `shouldReturn` False

  describe "Spartoi Cunning (17027)" do
    it "discards a card at random, deals 1 damage and places 1 threat on the main scheme" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        hand <- length <$> handOf p
        discard <- length <$> discardOf p
        threat <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.spartoiCunning
        pass
        length <$> handOf p `shouldReturn` (hand - 1)
        length <$> discardOf p `shouldReturn` (discard + 1)
        damageOf p `shouldReturn` 1
        mainSchemeThreatNow `shouldReturn` (threat + 1)
