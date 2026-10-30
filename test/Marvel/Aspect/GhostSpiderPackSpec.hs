module Marvel.Aspect.GhostSpiderPackSpec (spec) where

import Marvel.Hero.GhostSpiderSpec (ghostSpider, asGhost)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

spec :: Spec
spec = describe "Ghost-Spider protection pack" do
  describe "Jump Flip" do
    it "prevents two damage and removes two main-scheme threat when paid with energy" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        placeMainThreat 4
        event <- putInHand p "27014"
        energy <- putInHand p "01088"
        runNow $ damageIdentity p GameRef (toDamage 4 FromAbility)
        playWith event [energy]
        chooseMainScheme
        pass
        damageOf p `shouldReturn` 2
        mainSchemeThreatNow `shouldReturn` 2

    it "does not remove threat when paid without energy" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        placeMainThreat 4
        event <- putInHand p "27014"
        strength <- putInHand p "01090"
        runNow $ damageIdentity p GameRef (toDamage 4 FromAbility)
        playWith event [strength]
        pass
        damageOf p `shouldReturn` 2
        mainSchemeThreatNow `shouldReturn` 4

  describe "What Doesn't Kill Me" do
    it "pays for healing two damage and readies the hero using a physical resource" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        setDamage p 3
        resolve [IdentityMessage p ExhaustedIdentity]
        event <- putInHand p "27016"
        strength <- putInHand p "01090"
        playWith event [strength]
        damageOf p `shouldReturn` 1
        exhaustedNow p `shouldReturn` False

    it "cannot be played without enough damage, without a physical resource, or while ready" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27016"
        _ <- putInHand p "01089"
        setDamage p 3
        offers (isPlay event) `shouldReturn` False
        resolve [IdentityMessage p ExhaustedIdentity]
        offers (isPlay event) `shouldReturn` False
        _ <- putInHand p "01090"
        setDamage p 1
        offers (isPlay event) `shouldReturn` False
        setDamage p 2
        offers (isPlay event) `shouldReturn` True

  describe "Return the Favor" do
    it "reveals and fully resolves a discarded treachery before attacking for five" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        george <- putIntoPlay p "27007"
        event <- putInHand p "27015"
        _ <- setEncounterDeck [Minions.shocker, Treacheries.caughtOffGuard, Minions.hydraMercenary] []
        playWith event []
        villainDamageNow `shouldReturn` 0
        choose "resolve Caught Off Guard first" (isTargetChoice $ supportIdOf george)
        villainDamageNow `shouldReturn` 5
        encounterDiscardCodes >>= (`shouldSatisfy` (\cards -> "01103" `elem` cards && "01188" `elem` cards))

    it "does not attack when no treachery can be revealed to pay its cost" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27015"
        _ <- setEncounterDeck [Minions.shocker, Minions.hydraMercenary] []
        playWith event []
        villainDamageNow `shouldReturn` 0
        handOf p >>= (`shouldSatisfy` elem event)
