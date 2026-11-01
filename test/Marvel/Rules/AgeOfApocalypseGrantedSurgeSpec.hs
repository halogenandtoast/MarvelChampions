module Marvel.Rules.AgeOfApocalypseGrantedSurgeSpec (spec) where

import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

spec :: Spec
spec = describe "Age of Apocalypse granted surge" do
  it "Portal Through Time grants a Temporal treachery one facedown encounter card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Schemes.portalThroughTime
      pass
      ([next], _) <- setEncounterDeck [Treacheries.advance] []
      temporal <- encounterCard Treacheries.temporalTrickery
      resolve [RevealEncounterCard p temporal]
      dealtTo p `shouldReturn` [next]

  it "Enhanced Spider-Sense cancels the Temporal treachery and its already granted surge" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      clearHand p
      sense <- putInHand p "01004"
      payment <- putInHand p "01088"
      portal <- sideSchemeOf <$> revealNow p Schemes.portalThroughTime
      pass
      ([next], _) <- setEncounterDeck [Treacheries.advance] []
      before <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.temporalTrickery
      choose "grant surge before cancelling" (isAbilityOf portal)
      playWith sense [payment]
      pass
      mainSchemeThreatNow `shouldReturn` before
      dealtTo p `shouldReturn` []
      getEncounterDeck `shouldReturn` [next]

  it "Stepford Cuckoos cancels granted surge while revealing exactly its own replacement" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      cuckoos <- putIntoPlay p "45049"
      portal <- sideSchemeOf <$> revealNow p Schemes.portalThroughTime
      pass
      ([replacement, boost, remaining], _) <- setEncounterDeck
        [Treacheries.advance, Treacheries.advance, Treacheries.advance] []
      original <- revealNow p Treacheries.temporalTrickery
      choose "grant surge before cancelling" (isAbilityOf portal)
      choose "cancel the Temporal treachery" (isAbilityOfCard cuckoos)
      pass
      dealtTo p `shouldReturn` []
      getEncounterDeck `shouldReturn` [remaining]
      discard <- getEncounterDiscard
      discard `shouldSatisfy` elem original
      discard `shouldSatisfy` elem replacement
      discard `shouldSatisfy` elem boost
      [support] <- supportsWith "45049"
      supportUses support `shouldBe` 2
