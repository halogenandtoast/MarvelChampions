module Marvel.Hero.BishopSignatureSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n c = isAbilityOfCard card c && case c of
  UseAbility a -> abilityIndex a == n
  _ -> False

spec :: Spec
spec = describe "Bishop signature" do
  describe "Super-Charged" do
    it "adds a counter per printed resource icon and spends itself to boost a single basic attack" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        asHero p
        charged <- putIntoPlay p "45006"
        resource <- putInHand p "45010"
        choose "charge" (cardAbility charged 1)
        choose "Stored Energy" (isTargetChoice resource)
        pass
        upgradeUses . testHead <$> upgradesWith "45006" `shouldReturn` 2
        choose "attack" (isHeroAttack p)
        choose "spend Super-Charged" (cardAbility charged 2)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 6
        length <$> upgradesWith "45006" `shouldReturn` 0

    it "caps its attack bonus at eight even with more than four counters" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        asHero p
        charged <- putIntoPlay p "45006"
        for_ [1 .. 3 :: Int] $ \_ -> do
          resource <- putInHand p "45010"
          choose "charge Super-Charged" (cardAbility charged 1)
          choose "discard Stored Energy" (isTargetChoice resource)
          pass
        choose "attack" (isHeroAttack p)
        choose "spend Super-Charged" (cardAbility charged 2)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 10

  describe "Concussive Blast" do
    it "deals six and readies Bishop when a resource card pays for it" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        asHero p
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- putInHand p "45007"
        resource <- putInHand p "45010"
        extra <- putInHand p "45008"
        playWith card [resource, extra]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 6
        exhaustedNow p `shouldReturn` False

    it "does not ready Bishop merely because nonresource cards generated resource icons" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        asHero p
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- putInHand p "45007"
        paid <- traverse (putInHand p) ["45008", "45008", "45008"]
        playWith card paid
        chooseVillain
        pass
        exhaustedNow p `shouldReturn` True

  describe "Command Authority" do
    for_ [(True, ["45010"]), (False, ["45007", "45007"])] \(bonus, codes) ->
      it ("removes three threat and draws only when a resource card paid: " <> show bonus) do
        tg <- testGame ["bishop"]
        inGame tg do
          let p = player1 tg
          asHero p
          placeMainThreat 5
          before <- mainSchemeThreatNow
          card <- putInHand p "45008"
          payment <- traverse (putInHand p) codes
          playWith card payment
          chooseMainScheme
          pass
          mainSchemeThreatNow `shouldReturn` before - 3
          length <$> handOf p `shouldReturn` if bonus then 1 else 0

  describe "Energy Conversion" do
    it "shuffles every resource discard back, keeps other discards, and caps this attack's damage" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        asHero p
        resource <- putInDiscard p "45010"
        other <- putInDiscard p "45007"
        card <- putInHand p "45009"
        m <- addMinion p Minions.laufey
        setDamage p 0
        runNow [MinionMessage m $ MinionAttacks p]
        choose "Energy Conversion" (isPlay card)
        pass
        cards <- discardOf p
        resource `elem` cards `shouldBe` False
        other `elem` cards `shouldBe` True
        damageOf p `shouldReturn` 3

    it "does not cap a later attack" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        asHero p
        card <- putInHand p "45009"
        m <- addMinion p Minions.laufey
        setDamage p 0
        runNow [MinionMessage m $ MinionAttacks p]
        choose "Energy Conversion" (isPlay card)
        pass
        damageOf p `shouldReturn` 3
        runNow [MinionMessage m $ MinionAttacks p]
        pass
        damageOf p `shouldReturn` 7

  it "Stored Energy generates two resources, one physical and one energy" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      rifle <- putInHand p "45004"
      energy <- putInHand p "45010"
      playWith rifle [energy]
      length <$> upgradesWith "45004" `shouldReturn` 1

  it "a borrowed Concussive Blast readies Bishop rather than the hero who plays it" do
    tg <- testGame ["rogue", "bishop"]
    inGame tg do
      let p = player1 tg
          bishop = player2 tg
      asHero p
      resolve [IdentityMessage p ExhaustedIdentity, IdentityMessage bishop ExhaustedIdentity]
      card <- putInHand p "45007"
      stored <- putInHand p "45010"
      extra <- putInHand p "45008"
      playWith card [stored, extra]
      chooseVillain
      pass
      exhaustedNow p `shouldReturn` True
      exhaustedNow bishop `shouldReturn` False
