module Marvel.Aspect.MutantGenesisPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier (Modifier (KeywordModifier))
import Marvel.Keyword (Keyword (Patrol))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Ally.Types qualified as Ally
import Marvel.Window qualified as W
import Marvel.Villain.Types (villainConfused)
import TestImport

allyWithCode :: CardCode -> GameAppT AllyId
allyWithCode code = getsGame $ \g -> fromMaybe (error "ally missing") $ headMay [aid | (aid, a) <- HashMap.toList (gameAllies g), toCardCode (getCardDef a) == code]

allyHitPointsNow :: AllyId -> GameAppT Natural
allyHitPointsNow aid = getsGame (toAttrs . fromMaybe (error "ally missing") . HashMap.lookup aid . gameAllies) >>= Ally.getModifiedHitPoints

spec :: Spec
spec = describe "Mutant Genesis player cards" do
  it "Protective Training adds three hit points to an X-Men ally" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      _ <- putIntoPlay p "32002"
      aid <- allyWithCode "32002"
      card <- putInHand p "32013"
      pay <- putInHand p "01090"
      playOn card aid [pay]
      allyHitPointsNow aid `shouldReturn` 6

  it "Attack Training adds two hit points and one attack to an X-Men ally" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      _ <- putIntoPlay p "32032"
      aid <- allyWithCode "32032"
      card <- putInHand p "32043"
      pay <- putInHand p "01090"
      playOn card aid [pay]
      allyHitPointsNow aid `shouldReturn` 4
      a <- getsGame (toAttrs . fromMaybe (error "ally missing") . HashMap.lookup aid . gameAllies)
      Ally.getModifiedAttack a `shouldReturn` 2

  it "X-Mansion heals a Mutant alter-ego" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      setDamage p 3
      mansion <- putIntoPlay p "32049"
      choose "X-Mansion" (isAbilityOfCard mansion)
      choose "Kitty" (isTargetChoice p)
      damageOf p `shouldReturn` 2

  it "Polaris gives an X-Men character a tough status after entering" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      polaris <- putInHand p "32012"
      a <- putInHand p "01090"
      b <- putInHand p "01089"
      playWith polaris [a,b]
      choose "Polaris" (isAbilityOfCard polaris)
      choose "Shadowcat" (isTargetChoice p)
      (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True

  it "Professor X can confuse the villain on entering" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "32019"
      a <- putInHand p "01090"
      b <- putInHand p "01089"
      playWith card [a,b]
      vid <- villainIdNow
      choose "Professor X forced response" (isAbilityOfCard card)
      choose "confuse villain" (isTargetChoice vid)
      v <- getVillain
      villainConfused (toAttrs v) `shouldBe` True

  it "Colossus costs one less while the payer is a Mutant" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "32048"
      two <- putInHand p "01090"
      one <- putInHand p "32051"
      playWith card [two,one]
      aid <- allyWithCode "32048"
      a <- getsGame (toAttrs . fromMaybe (error "ally missing") . HashMap.lookup aid . gameAllies)
      allyTough a `shouldBe` True
      (length <$> handOf p) `shouldReturn` 0

  it "The X-Jet exhausts to pay for an X-Men hero's event" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      jet <- putIntoPlay p "32020"
      card <- putInHand p "32039"
      placeMainThreat 3
      choose "Airwalk" (isPlay card)
      choose "The X-Jet resource" (isAbilityOfCard jet)
      whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
      sid <- mainSchemeIdNow
      choose "main scheme" (isTargetChoice sid)
      offers (isAbilityOfCard jet) `shouldReturn` False

  it "Powerful Punch deals four damage while declaring a defense without basic DEF" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      card <- putInHand p "32014"
      two <- putInHand p "01090"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      playWith card [two]
      pass
      villainDamageNow `shouldReturn` 4
      damageOf p `shouldReturn` 2

  it "Defensive Energy draws one card when spent on a Defense event" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      card <- putInHand p "32014"
      wild <- putInHand p "32018"
      one <- putInHand p "32033"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      playWith card [wild,one]
      choose "Defensive Energy" (isLabel "$label.defensiveEnergy")
      (length <$> handOf p) `shouldReturn` 1

  for_ ["32021", "32050"] \code ->
    it ("Shadow and Steel " <> show code <> " cannot interrupt without Colossus") do
      tg <- testGame ["shadowcat"]
      inGame tg do
        let p = player1 tg
        changeForm p
        pass
        clearHand p
        card <- putInHand p code
        _ <- putInHand p "01090"
        vid <- villainIdNow
        start [VillainMessage vid (VillainAttacksWithoutBoost p)]
        offers (isPlay card) `shouldReturn` False

  it "Shadow and Steel can interrupt with Shadowcat and the Colossus ally in play" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      _ <- putIntoPlay p "32048"
      card <- putInHand p "32050"
      two <- putInHand p "01090"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      offers (isPlay card) `shouldReturn` True
      playWith card [two]
      pass
      damageOf p `shouldReturn` 0
      villainDamageNow `shouldReturn` 4

  it "Shadow and Steel requires its team-up and prevents the enemy attack's damage" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      _ <- putIntoPlay p "32002"
      card <- putInHand p "32021"
      two <- putInHand p "01090"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      playWith card [two]
      pass
      damageOf p `shouldReturn` 0
      villainDamageNow `shouldReturn` 4

  it "Nightcrawler returns to hand to prevent damage to an X-Men hero" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      nightcrawler <- putIntoPlay p "32011"
      energy <- putInHand p "01088"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      choose "take attack" isNoDefender
      choose "Nightcrawler" (isAbilityOfCard nightcrawler)
      payWith [energy]
      damageOf p `shouldReturn` 0
      cards <- handOf p
      map toCardCode cards `shouldSatisfy` elem "32011"

  it "Magik spends a mental resource to shuffle an engaged non-Elite minion into the encounter deck" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      mid <- addMinion p Minions.hydraSoldier
      card <- putInHand p "32042"
      two <- putInHand p "01090"
      one <- putInHand p "32033"
      mental <- putInHand p "01089"
      playWith card [two,one]
      choose "Magik" (isAbilityOfCard card)
      payWith [mental]
      choose "non-Elite minion" (isTargetChoice mid)
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False

  it "Aggressive Energy increases the attack event's damage by one" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      card <- putInHand p "32037"
      energy <- putInHand p "32047"
      one <- putInHand p "32033"
      playWith card [energy,one]
      choose "Aggressive Energy" (isLabel "$label.aggressiveEnergy")
      chooseVillain
      villainDamageNow `shouldReturn` 4

  it "Mutant Protectors puts an X-Men ally from hand into play as the defender" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      card <- putInHand p "32017"
      nightcrawler <- putInHand p "32011"
      one <- putInHand p "32033"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      playWith card [one]
      choose "X-Men ally" (isTargetChoice nightcrawler)
      pass
      damageOf p `shouldReturn` 0
      cards <- handOf p
      map toCardCode cards `shouldSatisfy` notElem "32011"

  it "Team Strike may exhaust only the hero and deals that hero's ATK" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      card <- putInHand p "32045"
      one <- putInHand p "32033"
      playWith card [one]
      villainDamageNow `shouldReturn` 2
      (.playerIdentityExhausted) <$> playerAttrs p `shouldReturn` True

  it "Gatekeeper gives its attached minion two extra hit points and patrol" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      mid <- addMinion p Minions.hydraSoldier
      card <- putInHand p "32044"
      playOn card mid []
      a <- getsGame (toAttrs . fromMaybe (error "minion missing") . HashMap.lookup mid . gameMinions)
      Minion.getModifiedHitPoints a `shouldReturn` 6
      modifiers <- getModifiers (MinionRef mid)
      modifiers `shouldSatisfy` elem (KeywordModifier Patrol)

  it "Wolverine heals one damage after his controller's turn begins" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putIntoPlay p "32041"
      aid <- allyWithCode "32041"
      runNow [AllyMessage aid (AllyDamaged (toSource p) $ toDamage 2 FromAbility)]
      start [IdentityMessage p BeginTurn]
      choose "Wolverine" (isAbilityOfCard card)
      a <- getsGame (toAttrs . fromMaybe (error "ally missing") . HashMap.lookup aid . gameAllies)
      allyDamage a `shouldBe` 1

  it "Training upgrades share a maximum of one per ally across both aspects" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      _ <- putIntoPlay p "32032"
      aid <- allyWithCode "32032"
      protective <- putInHand p "32013"
      one <- putInHand p "32033"
      playOn protective aid [one]
      attack <- putInHand p "32043"
      _ <- putInHand p "01090"
      offers (isPlay attack) `shouldReturn` False

  it "Defensive Energy does not respond when spent on a thwart event" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      placeMainThreat 4
      card <- putInHand p "32039"
      energy <- putInHand p "32018"
      playWith card [energy]
      offers (isLabel "$label.defensiveEnergy") `shouldReturn` False
      sid <- mainSchemeIdNow
      choose "main scheme" (isTargetChoice sid)
      (length <$> handOf p) `shouldReturn` 0

  it "Professor X discards himself at the end of the round" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      professor <- putIntoPlay p "32019"
      aid <- allyWithCode "32019"
      start [EndRound]
      choose "Professor X leaves" (isAbilityOfCard professor)
      getsGame (HashMap.member aid . gameAllies) `shouldReturn` False

  it "Gatekeeper removes four threat from the main scheme when its host is defeated" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      mid <- addMinion p Minions.hydraSoldier
      gate <- putInHand p "32044"
      playOn gate mid []
      placeMainThreat 5
      before <- mainSchemeThreatNow
      strike <- putInHand p "32038"
      two <- putInHand p "01090"
      one <- putInHand p "32033"
      playWith strike [two,one]
      choose "Gatekeeper host" (isTargetChoice mid)
      choose "Gatekeeper interrupt" (isAbilityOfCard gate)
      sid <- mainSchemeIdNow
      choose "main scheme" (isTargetChoice sid)
      mainSchemeThreatNow `shouldReturn` (before - 4)

  it "Nightcrawler prevents only one chosen character's packet from simultaneous attack damage" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      nightcrawler <- putIntoPlay p "32011"
      _ <- putIntoPlay p "32041"
      wolverine <- allyWithCode "32041"
      energy <- putInHand p "01088"
      vid <- villainIdNow
      let damage = toDamage 1 FromAttack
      start
        [ CheckWindows [W.Window W.When (W.IdentityTakeDamage p damage), W.Window W.When (W.AllyTakeDamage wolverine damage)]
        , Simultaneously [IdentityMessage p (IdentityDamaged (VillainRef vid) damage), AllyMessage wolverine (AllyDamaged (VillainRef vid) damage)]
        ]
      choose "Nightcrawler" (isAbilityOfCard nightcrawler)
      payWith [energy]
      choose "protect Shadowcat only" (isTargetChoice p)
      damageOf p `shouldReturn` 0
      a <- getsGame (toAttrs . fromMaybe (error "Wolverine missing") . HashMap.lookup wolverine . gameAllies)
      allyDamage a `shouldBe` 1

  it "Team Strike divides one hero-only attack among multiple enemies without multiplying damage" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      mid <- addMinion p Minions.weaponsRunner
      card <- putInHand p "32045"
      one <- putInHand p "32033"
      playWith card [one]
      chooseVillain
      choose "one damage to minion" (isTargetChoice mid)
      villainDamageNow `shouldReturn` 1
      dealt <- getsGame $ maybe 0 (minionDamage . toAttrs) . HashMap.lookup mid . gameMinions
      dealt `shouldBe` 1
