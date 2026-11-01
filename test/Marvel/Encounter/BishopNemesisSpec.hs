module Marvel.Encounter.BishopNemesisSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability qualified as Ability
import TestImport

sideSchemeCodes :: GameAppT [CardCode]
sideSchemeCodes = getsGame $ map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> settleForced >> clearHand p

settleForced :: GameAppT ()
settleForced = do
  pass
  question <- asked
  case question >>= (headMay . filter forced . snd) of
    Just choice -> choose "resolve mandatory ability" (== choice) >> settleForced
    Nothing -> pure ()
 where
  forced = \case
    UseAbility ab -> abilityType ab `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse, Ability.WhenRevealed]
    _ -> False

spec :: Spec
spec = describe "Bishop nemesis" do
  describe "Fear the Future" do
    it "can exhaust Lucas Bishop to remove itself from the game" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- revealNow p Obligations.fearTheFuture
        choose "exhaust Lucas" (isLabel "$label.exhaustLucasBishopRemoveFearTheFuture")
        exhaustedNow p `shouldReturn` True
        codes <- map toCardCode <$> encounterDiscardNow
        "45025" `elem` codes `shouldBe` False

    it "discards all resource cards and preserves nonresource cards" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putInHand p "45010"
        _ <- putInHand p "01088"
        other <- putInHand p "45007"
        _ <- revealNow p Obligations.fearTheFuture
        choose "discard resources" (isLabel "$label.discardResourcesFearTheFuture")
        settleForced
        handOf p `shouldReturn` [other]

  describe "Bantam" do
    it "finds and reveals Portal Through Time when it is absent" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- revealNow p Minions.bantam
        settleForced
        codes <- sideSchemeCodes
        "45027" `elem` codes `shouldBe` True

    it "places two threat on Portal Through Time when it is already in play" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        asHero p
        portal <- sideSchemeOf <$> revealNow p Schemes.portalThroughTime
        before <- sideSchemeThreat portal
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance, Treacheries.advance] []
        _ <- revealNow p Minions.bantam
        settleForced
        sideSchemeThreat portal `shouldReturn` before + 2

  it "Trevor Fitzroy has quickstrike and creates the portal after defeating an ally" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      defender <- putIntoPlay p "01002"
      _ <- revealNow p Minions.trevorFitzroy
      choose "Black Cat defends quickstrike" \case
        AllyDefend aid _ -> aid == allyIdOf defender
        _ -> False
      settleForced
      codes <- sideSchemeCodes
      "45027" `elem` codes `shouldBe` True

  describe "Temporal Trickery" do
    it "forces discarding a card with the most printed icons and places that much threat on every scheme" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        asHero p
        side <- sideSchemeOf <$> revealNow p Schemes.bombScare
        ordinary <- putInHand p "45007"
        doubled <- putInHand p "45010"
        before <- mainSchemeThreatNow
        sideBefore <- sideSchemeThreat side
        _ <- revealNow p Treacheries.temporalTrickery
        offers (isTargetChoice ordinary) `shouldReturn` False
        choose "double resource" (isTargetChoice doubled)
        settleForced
        mainSchemeThreatNow `shouldReturn` before + 2
        sideSchemeThreat side `shouldReturn` sideBefore + 2

    it "does nothing with an empty hand and does not ask an empty question" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        asHero p
        before <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.temporalTrickery
        settleForced
        mainSchemeThreatNow `shouldReturn` before

  it "Portal Through Time gives only the first Temporal reveal each phase surge" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- revealNow p Schemes.portalThroughTime
      first <- putInHand p "45007"
      second <- putInHand p "45008"
      _ <- setEncounterDeck [Minions.hydraMercenary, Minions.hydraSoldier] []
      _ <- revealNow p Treacheries.temporalTrickery
      settleForced
      choose "first discard" (isTargetChoice first)
      settleForced
      map toCardCode <$> dealtTo p `shouldReturn` ["01101"]
      runNow [IdentityMessage p RevealEncounterCards]
      settleForced
      codes <- getsGame $ map (toCardCode . getCardDef) . HashMap.elems . gameMinions
      "01101" `elem` codes `shouldBe` True
      _ <- revealNow p Treacheries.temporalTrickery
      choose "second discard" (isTargetChoice second)
      settleForced
      map (toCardCode . getCardDef) <$> scenarioEncounterDeckNow `shouldReturn` ["01182"]
      dealtTo p `shouldReturn` []

  it "Fear the Future surges when its discard option finds no resource cards" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- setEncounterDeck [Minions.hydraMercenary, Minions.hydraSoldier] []
      _ <- revealNow p Obligations.fearTheFuture
      choose "discard no resources" (isLabel "$label.discardResourcesFearTheFuture")
      settleForced
      map (toCardCode . getCardDef) <$> scenarioEncounterDeckNow `shouldReturn` ["01182"]
