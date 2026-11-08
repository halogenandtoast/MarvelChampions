module Marvel.Aspect.ShieldPackSpec (spec) where
import Marvel.Phase
import Marvel.AgentsOfShield.PlayerHelpers
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import TestImport

onCard :: PlayerCard -> Natural -> Choice -> Bool
onCard c n choice = isAbilityOfCard c choice && case choice of UseAbility ab -> abilityIndex ab == n; _ -> False

spec :: Spec
spec = describe "SHIELD player cards" do
  it "On the Double respects the combined six-cost budget" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      staff <- putIntoPlay p "50008"
      iliad <- putIntoPlay p "50009"
      runNow [SupportMessage (supportIdOf staff) ExhaustedSupport,SupportMessage (supportIdOf iliad) ExhaustedSupport]
      event <- putInHand p "50004"
      playWith event []
      choose "ready staff" (isTargetChoice $ supportIdOf staff)
      offers (isTargetChoice $ supportIdOf iliad) `shouldReturn` False
      choose "done" (isLabel "$label.done")
      supports <- supportsWith "50008"
      map supportExhausted supports `shouldBe` [False]
      ships <- supportsWith "50009"
      map supportExhausted ships `shouldBe` [True]

  it "Reinforcements can add a fourth counter but cannot choose the same support twice" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      iliad <- putIntoPlay p "50009"
      event <- putInHand p "50005"
      resource <- putInHand p "01088"
      playWith event [resource]
      choose "Iliad" (isTargetChoice $ supportIdOf iliad)
      offers (isTargetChoice $ supportIdOf iliad) `shouldReturn` False
      choose "done" (isLabel "$label.done")
      getCountersOn (supportIdOf iliad) `shouldReturn` 4

  it "All-Points Bulletin repeats once per controlled SHIELD support" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      _ <- putIntoPlay p "50008"
      event <- putInHand p "50003"
      resource <- putInHand p "01088"
      playWith event [resource]
      choose "damage" (isLabel "$label.dealDamage")
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 1

  for_ [("50017",2),("50018",3),("50019",3),("50020",2)] $ \(code,uses) ->
    it (show code <> " enters with its printed all-purpose counters") do
      tg <- testGame ["maria-hill"]
      inGame tg do
        card <- putIntoPlay (player1 tg) code
        getCountersOn (supportIdOf card) `shouldReturn` uses

  it "the Bellerophon strips tough before its three nonattack damage" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      ship <- putIntoPlay p "50018"
      v <- villainIdNow
      runNow [GiveStatus (VillainRef v) ToughStatus]
      choose "Bellerophon" (onCard ship 1)
      choose "player" (isTargetChoice p)
      pass
      villainDamageNow `shouldReturn` 3
      getCountersOn (supportIdOf ship) `shouldReturn` 2

  it "Safe House 221 can add suit threat in alter ego" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      house <- putIntoPlay p "50041"
      choose "safe house" (onCard house 1)
      choose "gather intel" (isLabel "$label.addThreat")
      pass
      suitThreat p `shouldReturn` 1

  it "SHIELD Director cannot act twice without readying" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "50008"
      director <- putIntoPlay p "50011"
      choose "Director" (onCard director 1)
      pass
      offers (onCard director 1) `shouldReturn` False

  it "EM Shield offers no interrupt for nonattack damage" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      shield <- putIntoPlay p "50042"
      L.runQueueT (L.capture $ L.damageIdentity p GameRef $ toDamage 2 FromAbility) >>= start
      offers (onCard shield 1) `shouldReturn` False
      pass
      damageOf p `shouldReturn` 2

  it "Circe puts a chosen ally from hand into play without paying its cost" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      clearHand p
      ship <- putIntoPlay p "50017"
      ally <- putInHand p "01083"
      choose "Circe" (onCard ship 1)
      choose "player" (isTargetChoice p)
      choose "ally" (isTargetChoice ally)
      pass
      length <$> alliesWith "01083" `shouldReturn` 1
      getCountersOn (supportIdOf ship) `shouldReturn` 1
      handOf p `shouldReturn` []

  it "Douglass removes two threat from the main scheme and spends one counter" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      ship <- putIntoPlay p "50019"
      placeMainThreat 5
      choose "Douglass" (onCard ship 1)
      pass
      mainSchemeThreatNow `shouldReturn` 3
      getCountersOn (supportIdOf ship) `shouldReturn` 2

  it "Victoria Hand readies an exhausted SHIELD support on entry" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      staff <- putIntoPlay p "50008"
      runNow [SupportMessage (supportIdOf staff) ExhaustedSupport]
      clearHand p
      victoria <- putInHand p "50012"
      r1 <- putInHand p "01088"
      r2 <- putInHand p "01089"
      playWith victoria [r1,r2]
      choose "Victoria's response" (onCard victoria 1)
      choose "staff" (isTargetChoice $ supportIdOf staff)
      pass
      map supportExhausted <$> supportsWith "50008" `shouldReturn` [False]

  it "Nick Fury's basic attack can replenish a SHIELD support" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      staff <- putIntoPlay p "50008"
      nick <- putIntoPlay p "50002"
      ally <- allyIn "50002"
      choose "Nick attacks" (isAllyAttack ally)
      chooseVillain
      choose "Nick's response" (onCard nick 1)
      choose "staff" (isTargetChoice $ supportIdOf staff)
      pass
      getCountersOn (supportIdOf staff) `shouldReturn` 4
      villainDamageNow `shouldReturn` 2

  it "Leo Fitz searches the deck for a Tech card in alter ego" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      leo <- putIntoPlay p "50056"
      clearHand p
      cards <- stackDeck p ["50044"]
      choose "Leo" (onCard leo 1)
      choose "watch" (isTargetChoice $ testHead cards)
      pass
      map toCardCode <$> handOf p `shouldReturn` ["50044"]

  it "Nick Fury Sr removes three threat when he enters play" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      clearHand p
      placeMainThreat 5
      nick <- putInHand p "50054"
      r1 <- putInHand p "01088"
      r2 <- putInHand p "01089"
      playWith nick [r1,r2]
      choose "Nick Sr entry" (onCard nick 1)
      choose "remove threat" (isLabel "$label.removeThreat")
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 2

  it "Nick Fury Sr can draw two cards instead" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- stackDeck p ["01088", "01089"]
      nick <- putInHand p "50054"
      r1 <- putInHand p "01088"
      r2 <- putInHand p "01089"
      playWith nick [r1,r2]
      choose "Nick Sr entry" (onCard nick 1)
      choose "draw" (isLabel "$label.drawCards")
      pass
      length <$> handOf p `shouldReturn` 2

  it "Nick Fury Sr leaves at the end of the round" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "50054"
      runNow [EndRound]
      length <$> alliesWith "50054" `shouldReturn` 0

  it "Pericles gives separate statuses to a hero and an ally" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      ally <- putIntoPlay p "01083"
      ship <- putIntoPlay p "50020"
      choose "Pericles" (onCard ship 1)
      choose "Maria" (isTargetChoice p)
      choose "confuse" (isLabel "$label.confuse")
      choose "ally" (isTargetChoice $ allyIdOf ally)
      choose "stun" (isLabel "$label.stun")
      pass
      isConfused p `shouldReturn` True
      getCountersOn (supportIdOf ship) `shouldReturn` 1

  it "Jemma Simmons costs only one resource for a SHIELD identity" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      clearHand p
      jemma <- putInHand p "50055"
      resource <- putInHand p "01090"
      playWith jemma [resource]
      pass
      length <$> supportsWith "50055" `shouldReturn` 1

  it "Leo Fitz costs only one resource for a SHIELD identity" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      clearHand p
      leo <- putInHand p "50056"
      resource <- putInHand p "01090"
      playWith leo [resource]
      pass
      length <$> supportsWith "50056" `shouldReturn` 1

  it "Prism Dust confuses and damages an entering minion, then is discarded" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      dust <- putIntoPlay p "50052"
      minion <- revealNow p Minions.hydraSoldier
      choose "Prism Dust" (onCard dust 1)
      pass
      minionDamageOf (minionOf minion) `shouldReturn` Just 2
      selectAny (EnemyWithId (EnemyMinionId $ minionOf minion) <> ConfusedEnemy) `shouldReturn` True
      map toCardCode <$> discardOf p `shouldReturn` ["50052"]

  it "Prism Dust cannot respond while Nick is in alter ego" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      dust <- putIntoPlay p "50052"
      _ <- revealNow p Minions.hydraSoldier
      offers (onCard dust 1) `shouldReturn` False
      pass
      map toCardCode <$> discardOf p `shouldReturn` []

  it "Melinda May can discard the encounter card inspected after attacking" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      melinda <- putIntoPlay p "50023"
      ally <- allyIn "50023"
      (cards, _) <- setEncounterDeck [Minions.hydraSoldier,Treacheries.advance] []
      choose "Melinda attacks" (isAllyAttack ally)
      chooseVillain
      choose "inspect" (onCard melinda 1)
      choose "discard" (isLabel "$label.discard")
      pass
      encounterDiscard `shouldReturn` [testHead cards]

  it "Melinda May may leave the inspected card on top of the encounter deck" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      melinda <- putIntoPlay p "50023"
      ally <- allyIn "50023"
      _ <- setEncounterDeck [Minions.hydraSoldier] []
      choose "Melinda attacks" (isAllyAttack ally)
      chooseVillain
      choose "inspect" (onCard melinda 1)
      choose "leave it" (isLabel "$label.done")
      pass
      encounterDiscard `shouldReturn` []

  it "Practiced Plan returns a discarded Preparation to hand" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      plan <- putIntoPlay p "50058"
      dust <- putIntoPlay p "50052"
      _ <- revealNow p Minions.hydraSoldier
      choose "Prism Dust" (onCard dust 1)
      choose "Practiced Plan" (onCard plan 1)
      pass
      map toCardCode <$> handOf p `shouldReturn` ["50052"]
      map toCardCode <$> discardOf p `shouldReturn` ["50058"]

  it "Secret Agent moves one scheme threat onto the suit after a Preparation resolves" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      agent <- putIntoPlay p "50046"
      dust <- putIntoPlay p "50052"
      placeMainThreat 4
      _ <- revealNow p Minions.hydraSoldier
      choose "Prism Dust" (onCard dust 1)
      choose "Secret Agent" (onCard agent 1)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 3
      suitThreat p `shouldReturn` 1

  it "Informant makes a minion remove threat instead of placing it" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      informant <- putIntoPlay p "50050"
      mid <- addMinion p Minions.hydraSoldier
      placeMainThreat 4
      start [MinionMessage mid (MinionSchemesAgainst p)]
      choose "Informant" (onCard informant 1)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 3
      map toCardCode <$> discardOf p `shouldReturn` ["50050"]

  it "Special Funding adds its counter after the purchased SHIELD support enters" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      clearHand p
      staff <- putInHand p "50008"
      funding <- putInHand p "50007"
      playWith staff [funding]
      choose "fund support" (isLabel "$label.specialFunding")
      pass
      getCountersOn (supportIdOf staff) `shouldReturn` 4

  it "The Hard Call damages every enemy by the discarded support's printed cost" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      _ <- putIntoPlay p "50008"
      mid <- addMinion p Minions.hydraSoldier
      clearHand p
      event <- putInHand p "50006"
      resource <- putInHand p "01088"
      choose "play Hard Call" (isPlay event)
      payWith [resource]
      pass
      villainDamageNow `shouldReturn` 1
      minionDamageOf mid `shouldReturn` Just 1

  it "Super Spies cannot be played without its second Team-Up character" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      _ <- putIntoPlay p "50008"
      event <- putInHand p "50024"
      offers (isPlay event) `shouldReturn` False

  it "Super Spies places three counters and may choose the same support repeatedly" do
    tg <- testGame ["maria-hill", "nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      L.runQueueT (L.capture $ L.changeToHeroForm $ player2 tg) >>= runNow
      pass
      staff <- putIntoPlay p "50008"
      event <- putInHand p "50024"
      playWith event []
      choose "counter one" (isTargetChoice $ supportIdOf staff)
      choose "counter two" (isTargetChoice $ supportIdOf staff)
      choose "counter three" (isTargetChoice $ supportIdOf staff)
      pass
      getCountersOn (supportIdOf staff) `shouldReturn` 6

  it "Support Staff generates a resource and exhausts while spending one counter" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      staff <- putIntoPlay p "50008"
      clearHand p
      director <- putInHand p "50011"
      resource <- putInHand p "01090"
      choose "play Director" (isPlay director)
      choose "staff resource" (onCard staff 1)
      payWith [resource]
      pass
      getCountersOn (supportIdOf staff) `shouldReturn` 2
      map supportExhausted <$> supportsWith "50008" `shouldReturn` [True]

  it "Jemma generates a mental resource for a Tech upgrade" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      jemma <- putIntoPlay p "50055"
      clearHand p
      shield <- putInHand p "50042"
      resource <- putInHand p "01090"
      choose "play shield" (isPlay shield)
      choose "Jemma resource" (onCard jemma 1)
      payWith [resource]
      pass
      map supportExhausted <$> supportsWith "50055" `shouldReturn` [True]

  it "Grant Ward deals his ATK as damage and leaves the game if his price is declined" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      grant <- putIntoPlay p "50022"
      clearHand p
      _ <- revealNow p Treacheries.falseAlarm
      choose "Grant Ward response" (onCard grant 1)
      choose "accept betrayal" (isLabel "$label.aosGrantWardDamage")
      pass
      damageOf p `shouldReturn` 2
      length <$> alliesWith "50022" `shouldReturn` 0
      map toCardCode <$> discardOf p `shouldReturn` []

  it "Agents of SHIELD cancels a revealed card and reveals its replacement" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      team <- putIntoPlay p "50015"
      _ <- setEncounterDeck [Minions.hydraSoldier,Treacheries.advance] []
      original <- revealNow p Treacheries.falseAlarm
      choose "Agents of SHIELD" (onCard team 1)
      pass
      isConfused p `shouldReturn` False
      length <$> selectList AnyMinion `shouldReturn` 1
      encounterDiscard >>= (`shouldSatisfy` elem original)

  it "Agents of SHIELD cannot answer if a controlled ally lacks SHIELD" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      team <- putIntoPlay p "50015"
      _ <- putIntoPlay p "01050"
      _ <- revealNow p Treacheries.falseAlarm
      offers (onCard team 1) `shouldReturn` False
      pass

  it "Intelligence can swap a dealt encounter card with the top of the encounter deck" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      intel <- putIntoPlay p "50051"
      (cards, _) <- setEncounterDeck [Treacheries.falseAlarm, Minions.hydraSoldier] []
      L.runQueueT (L.capture $ L.dealEncounterCard p) >>= start
      choose "Intelligence" (onCard intel 1)
      choose "dealt card" (isTargetChoice $ testHead cards)
      choose "replacement" (isTargetChoice $ testHead $ testTail cards)
      choose "done" (isLabel "$label.done")
      pass
      playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` [testHead $ testTail cards]

  it "Front Organization replaces an encounter effect discarding another controlled card" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      front <- putIntoPlay p "50028"
      staff <- putIntoPlay p "50008"
      _ <- revealNow p Treacheries.caughtOffGuard
      choose "discard staff" (isTargetChoice $ supportIdOf staff)
      choose "Front Organization" (onCard front 1)
      pass
      length <$> supportsWith "50008" `shouldReturn` 1
      length <$> supportsWith "50028" `shouldReturn` 0

  it "Organizational Support exhausts a shared-trait support for its printed resource" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      staff <- putIntoPlay p "50008"
      clearHand p
      director <- putInHand p "50011"
      resource <- putInHand p "50014"
      choose "play Director" (isPlay director)
      choose "pay Organizational Support" (isPayWith resource)
      choose "staff's printed icon" (isTargetChoice $ supportIdOf staff)
      offers (isTargetChoice $ supportIdOf staff) `shouldReturn` False
      choose "done" (isLabel "$label.done")
      payWith []
      pass
      map supportExhausted <$> supportsWith "50008" `shouldReturn` [True]
      getCountersOn (supportIdOf staff) `shouldReturn` 3

  it "Slingshot enters under another player's control and returns to her owner's hand at phase end" do
    tg <- testGame ["maria-hill", "nick-fury"]
    inGame tg do
      let p = player1 tg
      clearHand p
      sling <- putInHand p "50013"
      energy <- putInHand p "01088"
      choose "Slingshot action" (isAbilityOf $ CardIdRef $ pcCardId sling)
      payWith [energy]
      choose "Nick controls Slingshot" (isTargetChoice $ player2 tg)
      pass
      ally <- allyIn "50013"
      allyController ally `shouldBe` player2 tg
      runNow [EndPhase PlayerPhase]
      pass
      handOf p >>= (`shouldSatisfy` any ((== pcCardId sling) . pcCardId))
      length <$> alliesWith "50013" `shouldReturn` 0

  it "Douglass removes two threat from every scheme while a crisis icon is in play" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      ship <- putIntoPlay p "50019"
      crisis <- revealNow p Schemes.defenseNetwork
      pass
      placeMainThreat 5
      before <- sideSchemeThreat (sideSchemeOf crisis)
      choose "Douglass ignores crisis" (onCard ship 1)
      pass
      mainSchemeThreatNow `shouldReturn` 3
      sideSchemeThreat (sideSchemeOf crisis) `shouldReturn` subtractNatural 2 before
      getCountersOn (supportIdOf ship) `shouldReturn` 2

  it "Show of Empathy redirects Secret Agent's moved threat to a minion instead of the suit" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      agent <- putIntoPlay p "50046"
      scheme <- putIntoPlay p "51017"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      dust <- putIntoPlay p "50052"
      card <- revealNow p Minions.hydraSoldier
      let mid = minionOf card
      choose "Prism Dust" (onCard dust 1)
      choose "Secret Agent" (onCard agent 1)
      choose "move from Show of Empathy" (isTargetChoice sid)
      choose "Show of Empathy redirects" (isAbilityOf sid)
      choose "put threat on minion" (isTargetChoice mid)
      pass
      suitThreat p `shouldReturn` 0
      getCharacterThreat (MinionCharacter mid) `shouldReturn` 1
      sideSchemeThreat sid `shouldReturn` 5

  it "Slingshot's Energy action is unavailable while she is already in play" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      clearHand p
      sling <- putIntoPlay p "50013"
      _ <- putInHand p "01088"
      offers (onCard sling 1) `shouldReturn` False
      offers (isAbilityOf $ CardIdRef $ pcCardId sling) `shouldReturn` False

  it "Slingshot's Energy action is unavailable from the discard pile" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      clearHand p
      sling <- putInDiscard p "50013"
      _ <- putInHand p "01088"
      offers (isAbilityOf $ CardIdRef $ pcCardId sling) `shouldReturn` False

  it "SHIELD Director can add a counter to another player's SHIELD support" do
    tg <- testGame ["maria-hill", "nick-fury"]
    inGame tg do
      let p = player1 tg
      director <- putIntoPlay p "50011"
      staff <- putIntoPlay (player2 tg) "50008"
      choose "SHIELD Director" (onCard director 1)
      choose "other player's Support Staff" (isTargetChoice $ supportIdOf staff)
      pass
      getCountersOn (supportIdOf staff) `shouldReturn` 4

  it "Grant Ward can be retained by paying a mental resource after revealing a treachery" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      grant <- putIntoPlay p "50022"
      clearHand p
      resource <- putInHand p "01089"
      _ <- revealNow p Treacheries.falseAlarm
      choose "Grant Ward response" (onCard grant 1)
      choose "pay mental" (isLabel "$label.aosGrantWardPay")
      payWith [resource]
      pass
      damageOf p `shouldReturn` 0
      length <$> alliesWith "50022" `shouldReturn` 1

  it "Nick Fury Sr can give a SHIELD identity tough on entry" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      clearHand p
      nick <- putInHand p "50054"
      r1 <- putInHand p "01088"
      r2 <- putInHand p "01089"
      playWith nick [r1,r2]
      choose "Nick Sr entry" (onCard nick 1)
      choose "give tough" (isLabel "$label.giveTough")
      choose "Nick Fury identity" (isTargetChoice p)
      pass
      playerIdentityTough <$> playerAttrs p `shouldReturn` True

  it "Jemma cannot generate a resource for a card without Tech" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      jemma <- putIntoPlay p "50055"
      clearHand p
      staff <- putInHand p "50008"
      resource <- putInHand p "01090"
      choose "play support" (isPlay staff)
      offers (onCard jemma 1) `shouldReturn` False
      payWith [resource]
      pass

  it "Leo Fitz cannot use his search in hero form" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      leo <- putIntoPlay p "50056"
      changeForm p
      pass
      offers (onCard leo 1) `shouldReturn` False

  it "Front Organization cannot replace an ally's defeat by an enemy attack" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      front <- putIntoPlay p "50028"
      ally <- putIntoPlay p "52014"
      mid <- addMinion p Minions.hydraSoldier
      withGame_ $ phaseL .~ VillainPhase
      L.runQueueT (L.capture $ L.enemyAttacks mid p) >>= start
      choose "Spider-Byte defends" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
      offers (onCard front 1) `shouldReturn` False
      pass
      selectAny (AllyWithId $ allyIdOf ally) `shouldReturn` False
      length <$> supportsWith "50028" `shouldReturn` 1
      damageOf p `shouldReturn` 0

  it "Front Organization cannot replace an ally's defeat by consequential damage" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      front <- putIntoPlay p "50028"
      ally <- putIntoPlay p "52014"
      L.runQueueT (L.capture $ L.damageCharacter (allyIdOf ally) GameRef $ toDamage 1 FromAbility) >>= runNow
      attrs <- allyIn "52014"
      choose "Spider-Byte uses a basic attack" (isAllyAttack attrs)
      chooseVillain
      offers (onCard front 1) `shouldReturn` False
      pass
      selectAny (AllyWithId $ allyIdOf ally) `shouldReturn` False
      length <$> supportsWith "50028" `shouldReturn` 1
      villainDamageNow `shouldReturn` 1

  it "Front Organization cannot replace attached-upgrade cleanup after an encounter discards its host" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      front <- putIntoPlay p "50028"
      ally <- putIntoPlay p "52014"
      clearHand p
      upgrade <- putInHand p "01074"
      resource <- putInHand p "01090"
      playOn upgrade (allyIdOf ally) [resource]
      pass
      _ <- revealNow p Treacheries.energyBlast
      choose "Energy Blast discards Spider-Byte" (isTargetChoice $ allyIdOf ally)
      offers (onCard front 1) `shouldReturn` True
      choose "allow direct encounter discard" (isLabel "$label.noResponse")
      offers (onCard front 1) `shouldReturn` False
      pass
      selectAny (AllyWithId $ allyIdOf ally) `shouldReturn` False
      selectAny (UpgradeWithId $ upgradeIdOf upgrade) `shouldReturn` False
      length <$> supportsWith "50028" `shouldReturn` 1

  it "Front Organization cannot replace a support discarded as The Hard Call's player cost" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      front <- putIntoPlay p "50028"
      _ <- putIntoPlay p "50008"
      clearHand p
      event <- putInHand p "50006"
      resource <- putInHand p "01088"
      playWith event [resource]
      offers (onCard front 1) `shouldReturn` False
      pass
      length <$> supportsWith "50008" `shouldReturn` 0
      length <$> supportsWith "50028" `shouldReturn` 1
      villainDamageNow `shouldReturn` 1
