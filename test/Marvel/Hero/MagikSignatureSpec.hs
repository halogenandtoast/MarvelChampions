module Marvel.Hero.MagikSignatureSpec (spec) where

import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.SideScheme.Cards qualified as SideSchemes
import Data.HashMap.Strict qualified as HashMap
import Marvel.Villain.Types (villainConfused, villainStunned)
import TestImport

asMagik :: IdentityId -> GameAppT ()
asMagik p = changeForm p >> pass >> clearHand p

paying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
paying p code codes = do
  card <- putInHand p code
  payment <- traverse (putInHand p) codes
  playWith card payment
  pure card

spec :: Spec
spec = describe "Magik signature" do
  it "Scrying draws, discards and returns the chosen three cards" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      [drawn, discarded, returned] <- stackDeck p ["01086", "01087", "01093"]
      _ <- paying p "45036" []
      choose "draw the chosen card" (isTargetChoice drawn)
      choose "discard the chosen card" (isTargetChoice discarded)
      handOf p >>= (`shouldSatisfy` elem drawn)
      discardOf p >>= (`shouldSatisfy` elem discarded)
      headMay . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` Just returned

  it "Scrying does as much as possible with fewer than three cards" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      [drawn] <- stackDeck p ["01086"]
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs {playerIdentityDeck = Deck [drawn], playerIdentityDiscard = Discard []}
      _ <- paying p "45036" []
      choose "draw the only available card" (isTargetChoice drawn)
      handOf p >>= (`shouldSatisfy` elem drawn)

  it "Stepping Disc readies Magik and retrieves a different Magik card" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      [signature, anotherDisc, basic] <- stackDiscard p ["45039", "45037", "01086"]
      resolve [IdentityMessage p ExhaustedIdentity]
      _ <- paying p "45037" ["01086"]
      offers (isTargetChoice anotherDisc) `shouldReturn` False
      offers (isTargetChoice basic) `shouldReturn` False
      choose "return a Magik signature" (isTargetChoice signature)
      isExhaustedPlayer p `shouldReturn` False
      headMay . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` Just signature

  for_ [("01089", True), ("45031", True), ("01088", False)] \(topCode, confuses) ->
    it ("Exorcism removes four threat; top " <> show topCode <> " determines confusion") do
      tg <- testGame ["magik"]
      inGame tg do
        let p = player1 tg
        asMagik p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        _ <- stackDeck p [topCode]
        _ <- paying p "45038" ["01088"]
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 4
        villainConfused . toAttrs <$> getVillain `shouldReturn` confuses

  for_ [("01090", True), ("45031", True), ("01089", False)] \(topCode, stuns) ->
    it ("Soul Strike deals four damage; top " <> show topCode <> " determines stun") do
      tg <- testGame ["magik"]
      inGame tg do
        let p = player1 tg
        asMagik p
        _ <- stackDeck p [topCode]
        _ <- paying p "45039" ["01088"]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4
        villainStunned . toAttrs <$> getVillain `shouldReturn` stuns

  it "playing Soul Strike from the deck checks the newly exposed top card" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      [strike, _] <- stackDeck p ["45039", "01090"]
      payment <- putInHand p "01086"
      playWith strike [payment]
      chooseVillain
      pass
      villainStunned . toAttrs <$> getVillain `shouldReturn` True

  for_ [("01088", 3), ("45031", 3), ("01089", 0)] \(topCode, retaliatory) ->
    it ("Magic Barrier prevents three and top " <> show topCode <> " determines its damage") do
      tg <- testGame ["magik"]
      inGame tg do
        let p = player1 tg
        asMagik p
        _ <- stackDeck p [topCode]
        _ <- stackEncounterDeck [Treacheries.advance {cdBoostIcons = []}]
        barrier <- putInHand p "45040"
        payment <- putInHand p "01086"
        v <- villainIdNow
        runNow [VillainMessage v (VillainAttacks p)]
        choose "Magic Barrier" (isPlay barrier)
        payWith [payment]
        pass
        villainDamageNow `shouldReturn` retaliatory
        damageOf p `shouldReturn` 0

  it "Magic Barrier prevents damage and counterattacks when the enemy targets an ally" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      ally <- putIntoPlay p "45048"
      _ <- stackDeck p ["01088"]
      _ <- stackEncounterDeck [Treacheries.advance {cdBoostIcons = []}]
      barrier <- putInHand p "45040"
      payment <- putInHand p "01086"
      v <- villainIdNow
      runNow [VillainMessage v (VillainAttacksAlly p $ allyIdOf ally)]
      choose "defend the ally with Magic Barrier" (isPlay barrier)
      payWith [payment]
      pass
      villainDamageNow `shouldReturn` 3
      allyDamage <$> allyIn "45048" `shouldReturn` 0
      damageOf p `shouldReturn` 0

  it "Exorcism finishes after Specter of Death eliminates Magik without testing her removed deck" do
    tg <- testGame ["magik", "bishop"]
    inGame tg do
      let p = player1 tg
          survivor = player2 tg
      asMagik p
      encounter <- revealNow p SideSchemes.theSpecterOfDeath
      pass
      let sid = sideSchemeOf encounter
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved (toSource p) 2]
      maximumHp <- hitPointsOf p
      setDamage p (maximumHp - 1)
      _ <- stackDeck p ["01089"]
      _ <- paying p "45038" ["01088"]
      choose "defeat Specter of Death" (isTargetChoice $ SchemeSideSchemeId sid)
      pass
      players `shouldReturn` [survivor]
      getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` False
      villainConfused . toAttrs <$> getVillain `shouldReturn` False
