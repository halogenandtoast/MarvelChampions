module Marvel.Aspect.SilkPackSpec (spec) where
import TestImport
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Minion.Cards qualified as Minions
import Marvel.Silk.Cards qualified as Cards
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import Marvel.Matchers
import Marvel.Projection

spec :: Spec
spec = describe "Silk pack" do
  it "Spider-Byte costs three without any Tech cards" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "52014"
      _ <- putInHand p "01088"
      offers (isPlay card) `shouldReturn` False
      _ <- putInHand p "01089"
      offers (isPlay card) `shouldReturn` True

  it "Dr. Sinclair heals the triggering alter-ego by REC and can remove a status" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      doctor <- putIntoPlay p "52017"
      payment <- putInHand p "01089"
      setDamage p 6
      resolve [IdentityMessage p IdentityStunned]
      choose "Dr. Sinclair" (isAbilityOfCard doctor)
      payWith [payment]
      choose "remove stunned" (isLabel "$label.stunnedStatus")
      pass
      damageOf p `shouldReturn` 3
      project PlayerIdentityStunned p `shouldReturn` False

  it "Stun Gun starts with two charge counters" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      gun <- putIntoPlay p "52020"
      getCountersOn (upgradeIdOf gun) `shouldReturn` 2

  it "Ready for a Fight cannot be played without the required physical resource" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "52019"
      _ <- putInHand p "01089"
      offers (isPlay card) `shouldReturn` False
      _ <- putInHand p "01090"
      offers (isPlay card) `shouldReturn` True

  it "Quick Quip requires a Web-Warrior character for its damage cost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      card <- putInHand p "52034"
      _ <- putInHand p "01089"
      offers (isPlay card) `shouldReturn` False

  it "Scarlet Spider takes damage in place of another Web-Warrior character" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      spider <- putIntoPlay p "52013"
      L.runQueueT (L.capture $ L.damageIdentity p GameRef $ toDamage 2 FromAbility) >>= runNow
      choose "Scarlet Spider" (isAbilityOfCard spider)
      pass
      damageOf p `shouldReturn` 0
      getCharacterRemainingHitPoints (AllyCharacter $ allyIdOf spider) `shouldReturn` 3

  it "Stun Gun may spend both counters to stun the villain and is discarded" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      gun <- putIntoPlay p "52020"
      choose "spend both charges" (\case UseAbility ab -> abilitySource ab == UpgradeRef (upgradeIdOf gun) && abilityIndex ab == 2; _ -> False)
      chooseVillain
      pass
      v <- villainIdNow
      enemyMatches StunnedEnemy (EnemyVillainId v) `shouldReturn` True
      selectAny (UpgradeWithId $ upgradeIdOf gun) `shouldReturn` False

  it "Stop Hitting Yourself deals the DEF used for the completed attack" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      card <- putInHand p "52016"
      payment <- putInHand p "01089"
      mid <- addMinion p Cards.morlunSilk
      runNow [MinionMessage mid (MinionAttacks p)]
      choose "Silk defends" isDefend
      playWith card [payment]
      pass
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 2

  it "Quick Quip deals its additional cost to Silk and confuses an enemy" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      card <- putInHand p "52034"
      payment <- putInHand p "01089"
      playWith card [payment]
      choose "Silk takes the cost" (isTargetChoice p)
      chooseVillain
      pass
      damageOf p `shouldReturn` 1
      v <- villainIdNow
      enemyMatches ConfusedEnemy (EnemyVillainId v) `shouldReturn` True

  it "Spider-Woman responds to her own Web-Warrior entry" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      spider <- putInHand p "52033"
      payment <- traverse (putInHand p) ["01088","01093"]
      playWith spider payment
      choose "Spider-Woman response" (isAbilityOfCard spider)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 1

  it "Ready for a Fight changes Cindy to hero form and replaces the pending scheme" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      ready <- putIntoPlay p "52019"
      mid <- addMinion p Minions.hydraRegular
      before <- mainSchemeThreatNow
      runNow [MinionMessage mid (MinionSchemesAgainst p)]
      choose "Ready for a Fight" (isAbilityOfCard ready)
      choose "Silk defends" isDefend
      pass
      identityMatches HeroIdentity p `shouldReturn` True
      mainSchemeThreatNow `shouldReturn` before

  it "Energy Shield spends energy resources to prevent that much attached-character damage" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      shield <- putInHand p "52018"
      playOn shield p []
      payment <- putInHand p "01088"
      L.runQueueT (L.capture $ L.damageIdentity p GameRef $ toDamage 3 FromAbility) >>= runNow
      choose "Energy Shield" (isAbilityOfCard shield)
      payWith [payment]
      pass
      damageOf p `shouldReturn` 1

  it "Madame Web looks at Web-Warrior-count cards and may discard one" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      (deck,_) <- setEncounterDeck [Treacheries.advance,Treacheries.assault,Treacheries.advance] []
      web <- putInHand p "52021"
      payment <- traverse (putInHand p) ["01088","01093"]
      playWith web payment
      choose "Madame Web response" (isAbilityOfCard web)
      choose "discard Advance" (isTargetChoice $ testHead deck)
      pass
      discard <- getEncounterDiscard
      discard `shouldSatisfy` elem (testHead deck)
      (testHead <$> getEncounterDeck) `shouldReturn` (testHead $ testTail deck)

  it "Investigative Journalism cancels a scheme activation and confuses that enemy" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- putIntoPlay p "27049"
      card <- putInHand p "52024"
      payment <- putInHand p "01089"
      mid <- addMinion p Minions.hydraRegular
      before <- mainSchemeThreatNow
      runNow [MinionMessage mid (MinionSchemesAgainst p)]
      playWith card [payment]
      pass
      mainSchemeThreatNow `shouldReturn` before
      enemyMatches ConfusedEnemy (EnemyMinionId mid) `shouldReturn` True

  it "Spider-Man 2099 can return himself after using his basic attack" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      spider <- putIntoPlay p "52032"
      choose "Spider-Man 2099 attacks" (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf spider) && abilityIndex ab == 301; _ -> False)
      chooseVillain
      choose "Spider-Man 2099 response" (isAbilityOfCard spider)
      choose "return Spider-Man 2099" (isTargetChoice $ allyIdOf spider)
      pass
      villainDamageNow `shouldReturn` 2
      hand <- handOf p
      hand `shouldSatisfy` any ((== "52032") . toCardCode)

  it "Spider-Man 2099 may return himself to hand after defending" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      spider <- putIntoPlay p "52032"
      mid <- addMinion p Minions.hydraBomber
      L.runQueueT (L.capture $ L.enemyAttacksAlly mid p (allyIdOf spider)) >>= start
      choose "ally defends" (\case AllyDefend aid _ -> aid == allyIdOf spider; _ -> False)
      choose "Spider-Man 2099 defense response" (isAbilityOfCard spider)
      choose "return Spider-Man 2099" (isTargetChoice $ allyIdOf spider)
      pass
      map toCardCode <$> handOf p >>= (`shouldSatisfy` elem "52032")
      selectAny (AllyWithId $ allyIdOf spider) `shouldReturn` False

  it "Ready for a Fight answers Advance while Silk is already in hero form" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      ready <- putIntoPlay p "52019"
      _ <- setEncounterDeck [Treacheries.explosion,Treacheries.advance] []
      before <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.advance
      choose "Ready for a Fight in hero form" (isAbilityOfCard ready)
      choose "Silk defends the replacement attack" isDefend
      pass
      identityMatches HeroIdentity p `shouldReturn` True
      mainSchemeThreatNow `shouldReturn` before
      damageOf p `shouldReturn` 1
      selectAny (UpgradeWithId $ upgradeIdOf ready) `shouldReturn` False
