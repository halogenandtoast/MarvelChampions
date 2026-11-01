module Marvel.Aspect.BishopPackSpec (spec) where

import Marvel.Ally.Types qualified as Ally
import Marvel.Event.Events.SuitUp (allyAttachment)
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n c = isAbilityOfCard card c && case c of
  UseAbility a -> abilityIndex a == n
  _ -> False

spec :: Spec
spec = describe "Bishop pack" do
  it "Cable draws after thwarting and defeating a side scheme" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      cable <- putIntoPlay p "45011"
      side <- sideSchemeOf <$> revealNow p Schemes.breakinAndTakin
      resolve [SideSchemeMessage side $ SideSchemeThreatRemoved GameRef 1]
      attrs <- allyIn "45011"
      choose "Cable thwarts" (isAllyThwart attrs)
      choose "side scheme" (isTargetChoice side)
      choose "Cable response" (cardAbility cable 1)
      pass
      length <$> handOf p `shouldReturn` 1

  it "X-23 readies after defeating an enemy with an attack" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putIntoPlay p "45012"
      m <- addMinion p Minions.hydraMercenary
      attrs <- allyIn "45012"
      choose "X-23 attacks" (isAllyAttack attrs)
      choose "mercenary" (isTargetChoice m)
      choose "X-23 readies" (cardAbility card 1)
      pass
      allyExhausted <$> allyIn "45012" `shouldReturn` False
      allyDamage <$> allyIn "45012" `shouldReturn` 1

  it "Marrow deals two damage when entering play" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "45021"
      pay <- putInHand p "01088"
      playWith card [pay]
      choose "Marrow response" (cardAbility card 1)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2

  describe "Legion" do
    for_ [("01088", 2), ("01072", 0)] \(top, damage) ->
      it ("resolves printed energy, while wild does not substitute: " <> show top) do
        tg <- testGame ["bishop"]
        inGame tg do
          let p = player1 tg
          asHero p
          card <- putIntoPlay p "45020"
          _ <- stackDeck p [top]
          attrs <- allyIn "45020"
          choose "Legion attacks" (isAllyAttack attrs)
          chooseVillain
          choose "Legion response" (cardAbility card 1)
          when (damage > 0) chooseVillain
          pass
          villainDamageNow `shouldReturn` 1 + damage

  it "Advanced Suit attaches to an X-Men ally, then heals per discarded printed icon" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      malcolm <- putIntoPlay p "45002"
      suit <- putInHand p "45014"
      playWith suit []
      choose "attach to Malcolm" (isTargetChoice $ allyIdOf malcolm)
      resolve [AllyMessage (allyIdOf malcolm) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      m <- addMinion p Minions.hydraMercenary
      resolve [MinionMessage m $ MinionDamaged GameRef $ toDamage 1 FromAbility]
      resource <- putInHand p "01088"
      attrs <- allyIn "45002"
      choose "Malcolm attacks" (isAllyAttack attrs)
      choose "mercenary" (isTargetChoice m)
      choose "Advanced Suit" (cardAbility suit 1)
      choose "discard double energy" (isTargetChoice resource)
      pass
      allyDamage <$> allyIn "45002" `shouldReturn` 1

  it "Sidekick attaches only to an identity-specific ally and heals it after basic recovery" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      clearHand p
      malcolm <- putIntoPlay p "45002"
      cable <- putIntoPlay p "45011"
      sidekick <- putInHand p "45015"
      pay <- putInHand p "45007"
      choose "play Sidekick" (isPlay sidekick)
      offers (isTargetChoice $ allyIdOf cable) `shouldReturn` False
      choose "Malcolm" (isTargetChoice $ allyIdOf malcolm)
      payWith [pay]
      hp <- Ally.getModifiedHitPoints =<< allyIn "45002"
      hp `shouldBe` 5
      resolve [AllyMessage (allyIdOf malcolm) $ AllyDamaged GameRef $ toDamage 3 FromAbility]
      setDamage p 1
      choose "recover" \case UseAbility a -> Recover `elem` abilityChoices a; _ -> False
      choose "Sidekick heals" (cardAbility sidekick 1)
      pass
      allyDamage <$> allyIn "45002" `shouldReturn` 1

  describe "Suit Up" do
    it "finds an ally and an independently attachable upgrade with an empty board" do
      tg <- testGame ["bishop"]
      inGame tg do
        let p = player1 tg
        clearHand p
        ally <- putInDiscard p "45002"
        upgrade <- putInDiscard p "04015"
        card <- putInHand p "45017"
        pay <- putInHand p "01088"
        playWith card [pay]
        choose "Malcolm" (isTargetChoice ally)
        choose "Sky Cycle, independently of Malcolm's traits" (isTargetChoice upgrade)
        pass
        cards <- handOf p
        ally `elem` cards `shouldBe` True
        upgrade `elem` cards `shouldBe` True

    for_ [("03025", True), ("04015", True), ("45014", True), ("45015", True), ("45004", False)] \(code, valid) ->
      it ("classifies printed attachment eligibility: " <> show code) do
        tg <- testGame ["bishop"]
        inGame tg do
          c <- putInHand (player1 tg) code
          allyAttachment c `shouldBe` valid

  it "Side-by-Side pays by readying the sidekick, then readies Bishop and heals both" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      malcolm <- putIntoPlay p "45002"
      sidekick <- putInHand p "45015"
      pay <- putInHand p "45007"
      choose "Sidekick" (isPlay sidekick)
      choose "Malcolm" (isTargetChoice $ allyIdOf malcolm)
      payWith [pay]
      resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf malcolm) ExhaustedAlly,
        AllyMessage (allyIdOf malcolm) $ AllyDamaged GameRef $ toDamage 2 FromAbility]
      setDamage p 2
      card <- putInHand p "45016"
      payment <- putInHand p "01088"
      playWith card [payment]
      choose "sidekick ready cost" (isTargetChoice $ allyIdOf malcolm)
      choose "heal both" (isLabel "$label.sideBySideHeal")
      pass
      exhaustedNow p `shouldReturn` False
      allyExhausted <$> allyIn "45002" `shouldReturn` False
      damageOf p `shouldReturn` 1
      allyDamage <$> allyIn "45002" `shouldReturn` 1

  it "Legion removes threat after a mental discard" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putIntoPlay p "45020"
      _ <- stackDeck p ["01089"]
      placeMainThreat 5
      before <- mainSchemeThreatNow
      attrs <- allyIn "45020"
      choose "Legion attacks" (isAllyAttack attrs)
      chooseVillain
      choose "Legion response" (cardAbility card 1)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 2

  it "Legion heals after a physical discard before paying consequential damage" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putIntoPlay p "45020"
      resolve [AllyMessage (allyIdOf card) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      _ <- stackDeck p ["01090"]
      attrs <- allyIn "45020"
      choose "Legion attacks" (isAllyAttack attrs)
      chooseVillain
      choose "Legion response" (cardAbility card 1)
      pass
      allyDamage <$> allyIn "45020" `shouldReturn` 1

  it "Legion responds after defending as well as attacking and thwarting" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putIntoPlay p "45020"
      _ <- stackDeck p ["01088"]
      minion <- addMinion p Minions.hydraMercenary
      runNow [MinionMessage minion $ MinionAttacks p]
      choose "Legion defends" \case AllyDefend aid _ -> aid == allyIdOf card; _ -> False
      choose "Legion response" (cardAbility card 1)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2

  it "Side-by-Side cannot be played when its sidekick is already ready" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      malcolm <- putIntoPlay p "45002"
      sidekick <- putInHand p "45015"
      pay <- putInHand p "45007"
      playOn sidekick (allyIdOf malcolm) [pay]
      card <- putInHand p "45016"
      _ <- putInHand p "01088"
      offers (isPlay card) `shouldReturn` False

  it "Side-by-Side's other choice boosts both characters this phase" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      malcolm <- putIntoPlay p "45002"
      sidekick <- putInHand p "45015"
      pay <- putInHand p "45007"
      playOn sidekick (allyIdOf malcolm) [pay]
      resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf malcolm) ExhaustedAlly]
      card <- putInHand p "45016"
      payment <- putInHand p "01088"
      playWith card [payment]
      choose "ready sidekick" (isTargetChoice $ allyIdOf malcolm)
      choose "boost" (isLabel "$label.sideBySideBoost")
      pass
      attrs <- allyIn "45002"
      Ally.getModifiedAttack attrs `shouldReturn` 3
      Ally.getModifiedThwart attrs `shouldReturn` 2
      choose "Bishop attacks" (isHeroAttack p)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3

  it "Suit Up finds the campaign's Desperate Measures as an ally attachment" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      clearHand p
      ally <- putInDiscard p "45002"
      upgrade <- putInDiscard p "45176"
      card <- putInHand p "45017"
      payment <- putInHand p "01088"
      playWith card [payment]
      choose "Malcolm" (isTargetChoice ally)
      choose "Desperate Measures" (isTargetChoice upgrade)
      pass
      cards <- handOf p
      upgrade `elem` cards `shouldBe` True
