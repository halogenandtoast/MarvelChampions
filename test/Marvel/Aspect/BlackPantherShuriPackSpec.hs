module Marvel.Aspect.BlackPantherShuriPackSpec (spec) where
import TestImport
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Minion.Cards qualified as Minions
import Marvel.Trait qualified as T
import Marvel.Matchers
import Marvel.Query
import Marvel.Modifier
import Marvel.Message.Lifted qualified as L

spec :: Spec
spec = describe "Black Panther Shuri pack" do
  it "White Wolf places one threat on the main scheme after attacking" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      wolf <- putIntoPlay p "51037"
      before <- mainSchemeThreatNow
      choose "White Wolf attacks" (\case UseAbility a -> abilitySource a == AllyRef (allyIdOf wolf) && abilityIndex a == 301; _ -> False)
      chooseVillain
      choose "White Wolf forced response" (isAbilityOfCard wolf)
      pass
      mainSchemeThreatNow `shouldReturn` before + 1
      villainDamageNow `shouldReturn` 2

  it "Dora Milaje costs no resources for a Wakandan identity" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "51030"
      playWith card []
      pass
      selectAny (SupportNamed "Dora Milaje") `shouldReturn` True

  it "Aneka removes threat using the Special selected by Dora Milaje" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      aneka <- putIntoPlay p "51022"
      dora <- putIntoPlay p "51030"
      placeMainThreat 3
      before <- mainSchemeThreatNow
      choose "Dora Milaje" (isAbilityOfCard dora)
      choose "Aneka" (isTargetChoice $ allyIdOf aneka)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 1

  it "Ayo deals one damage using the Special selected by Dora Milaje" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      ayo <- putIntoPlay p "51023"
      dora <- putIntoPlay p "51030"
      choose "Dora Milaje" (isAbilityOfCard dora)
      choose "Ayo" (isTargetChoice $ allyIdOf ayo)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 1

  it "Okoye's Special grants the chosen Wakandan hero +1 THW and ATK" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      okoye <- putIntoPlay p "51024"
      dora <- putIntoPlay p "51030"
      choose "Dora Milaje" (isAbilityOfCard dora)
      choose "Okoye" (isTargetChoice $ allyIdOf okoye)
      choose "Black Panther" (isTargetChoice $ IdentityCharacter p)
      pass
      modifiers <- getModifiers p
      modifiers `shouldSatisfy` elem (AttackModifier 1)
      modifiers `shouldSatisfy` elem (ThwartModifier 1)

  it "Invisibility Gear is limited to one per player" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- putIntoPlay p "51019"
      card <- putInHand p "51019"
      _ <- putInHand p "01088"
      offers (isPlay card) `shouldReturn` False

  it "Sting Operation is limited to one per player" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- putIntoPlay p "51021"
      card <- putInHand p "51021"
      _ <- putInHand p "01088"
      offers (isPlay card) `shouldReturn` False

  it "Target Spotter enters with two target counters" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      spotter <- putIntoPlay p "51038"
      getCountersOn (SupportId $ unCardId $ pcCardId spotter) `shouldReturn` 2

  it "Show of Empathy places removed threat on a minion and Redemption makes it an ally" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      scheme <- putIntoPlay p "51017"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      mid <- addMinion p Minions.hydraRegular
      choose "basic thwart" (isHeroThwart p)
      choose "Show of Empathy" (isTargetChoice sid)
      choose "Show of Empathy forced interrupt" (isAbilityOf sid)
      choose "redeem the minion" (isTargetChoice mid)
      pass
      selectAny (MinionWithId mid) `shouldReturn` False
      redeemed <- alliesWith "04056"
      length redeemed `shouldBe` 1
      aid <- selectOne (AllyWithTrait T.Redeemed)
      aid `shouldSatisfy` isJust
      let controlled = fromMaybe (error "no redeemed ally") aid
      choose "redeemed minion attacks" (\case UseAbility ab -> abilitySource ab == AllyRef controlled && abilityIndex ab == 301; _ -> False)
      chooseVillain
      pass
      getCharacterRemainingHitPoints (AllyCharacter controlled) `shouldReturn` 1
      resolve [AllyMessage controlled ReadiedAlly]
      choose "redeemed minion attacks again" (\case UseAbility ab -> abilitySource ab == AllyRef controlled && abilityIndex ab == 301; _ -> False)
      chooseVillain
      pass
      selectAny (AllyWithId controlled) `shouldReturn` False
      victory <- getPlayerVictoryDisplay
      map toCardCode victory `shouldSatisfy` elem "51036"

  it "Heart of the Panther is not playable without the other named character" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      card <- putInHand p "51025"
      _ <- putInHand p "01088"
      offers (isPlay card) `shouldReturn` False

  it "Manifold lets a chosen player search deck and discard for a player side scheme" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      schemes <- stackDeck p ["51016"]
      manifold <- putInHand p "51014"
      payment <- traverse (putInHand p) ["01088","01093"]
      playWith manifold payment
      choose "Manifold response" (isAbilityOfCard manifold)
      choose "searching player" (isTargetChoice p)
      choose "Going Undercover" (isTargetChoice $ testHead schemes)
      pass
      hand <- handOf p
      hand `shouldSatisfy` any ((== "51016") . toCardCode)

  it "Aneka's basic power can resolve another Dora Milaje ally's Special" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      aneka <- putIntoPlay p "51022"
      ayo <- putIntoPlay p "51023"
      choose "Aneka attacks" (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf aneka) && abilityIndex ab == 301; _ -> False)
      chooseVillain
      choose "Aneka response" (isAbilityOfCard aneka)
      choose "Ayo Special" (isTargetChoice $ allyIdOf ayo)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3

  it "Infiltration removes actual discarded-card threat before engaging a discarded minion" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      (deck,_) <- setEncounterDeck [Treacheries.advance,Minions.hydraRegular,Treacheries.assault] []
      placeMainThreat 5
      before <- mainSchemeThreatNow
      infiltration <- putInHand p "51015"
      payment <- putInHand p "01093"
      playWith infiltration [payment]
      choose "discard two" (isLabel $ countLabel "discardEncounterCards" 2)
      chooseMainScheme
      choose "put the discarded minion into play" (isTargetChoice $ testHead $ testTail deck)
      pass
      mainSchemeThreatNow `shouldReturn` before - 2
      selectAny (MinionIs Minions.hydraRegular <> MinionEngagedWith (IdentityWithId p)) `shouldReturn` True

  it "The Raft tucks a minion leaving play and removes its printed SCH in threat" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      raft <- putIntoPlay p "51018"
      mid <- addMinion p Minions.hydraRegular
      placeMainThreat 5
      before <- mainSchemeThreatNow
      strike <- putInHand p "51003"
      payment <- putInHand p "01088"
      playWith strike [payment]
      choose "defeat the minion" (isTargetChoice mid)
      choose "The Raft" (isAbilityOfCard raft)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 1
      held <- concatMap supportHeldEncounterCards <$> supportsWith "51018"
      map toCardCode held `shouldBe` ["04056"]

  it "Going Undercover puts a non-scenario card into victory and orders the remainder" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      scheme <- putIntoPlay p "51016"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      (deck,_) <- setEncounterDeck [Treacheries.advance,Treacheries.assault,Treacheries.advance,Treacheries.assault,Treacheries.advance] []
      prowl <- putInHand p "51004"
      payment <- putInHand p "01089"
      playWith prowl [payment]
      choose "Going Undercover" (isTargetChoice sid)
      pass
      choose "basic thwart" (isHeroThwart p)
      choose "defeat Going Undercover" (isTargetChoice sid)
      choose "put Advance into victory" (isTargetChoice $ testHead deck)
      for_ (testTail deck) $ \card -> do
        choose "next ordered card" (isTargetChoice card)
        choose "put on top" (isLabel "$label.placeOnTop")
      pass
      victory <- getVictoryDisplay
      victory `shouldSatisfy` elem (testHead deck)

  it "Heart of the Panther puts an upgrade into play and resolves its Special with T'Challa present" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      _ <- putIntoPlay p "51002"
      upgrades <- stackDeck p ["51011"]
      heart <- putInHand p "51025"
      payment <- putInHand p "01088"
      playWith heart [payment]
      choose "find Panther Claws" (isTargetChoice $ testHead upgrades)
      choose "resolve Panther Claws" (isTargetChoice $ upgradeIdOf $ testHead upgrades)
      chooseVillain
      choose "retain claws" (isLabel "$label.done")
      choose "finish resolving specials" (isLabel "$label.done")
      pass
      villainDamageNow `shouldReturn` 2
      selectAny (UpgradeWithId $ upgradeIdOf $ testHead upgrades) `shouldReturn` True

  it "Invisibility Gear replaces an attack against its controller with a scheme" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      gear <- putIntoPlay p "51019"
      mid <- addMinion p Minions.hydraRegular
      before <- mainSchemeThreatNow
      runNow [MinionMessage mid (MinionAttacks p)]
      choose "Invisibility Gear" (isAbilityOfCard gear)
      pass
      damageOf p `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` before + 1
      selectAny (UpgradeWithId $ upgradeIdOf gear) `shouldReturn` False

  it "Sting Operation discards the non-Elite minion after it schemes" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      operation <- putIntoPlay p "51021"
      mid <- addMinion p Minions.hydraRegular
      runNow [MinionMessage mid (MinionSchemesAgainst p)]
      choose "Sting Operation" (isAbilityOfCard operation)
      pass
      selectAny (MinionWithId mid) `shouldReturn` False
      selectAny (UpgradeWithId $ upgradeIdOf operation) `shouldReturn` False

  it "Target Spotter consumes a counter and prevents the engaged minion activating this phase" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      spotter <- putIntoPlay p "51038"
      card <- revealNow p Minions.hydraRegular
      let mid = minionOf card
      choose "Target Spotter" (isAbilityOfCard spotter)
      pass
      getCountersOn (SupportId $ unCardId $ pcCardId spotter) `shouldReturn` 1
      modifiers <- getModifiers (MinionRef mid)
      modifiers `shouldSatisfy` elem CannotActivate
      runNow [MinionMessage mid (MinionAttacks p)]
      pass
      damageOf p `shouldReturn` 0

  for_ [("51022","51023"),("51024","51023"),("51023","51022")] $ \(code,other) ->
    it (show code <> " can resolve another Dora Milaje Special after defending") do
      tg <- testGame ["black-panther-shuri"]
      inGame tg do
        let p = player1 tg
        changeForm p
        pass
        defender <- putIntoPlay p code
        partner <- putIntoPlay p other
        mid <- addMinion p Minions.hydraBomber
        placeMainThreat 5
        L.runQueueT (L.capture $ L.enemyAttacksAlly mid p (allyIdOf defender)) >>= start
        choose "ally defends" (\case AllyDefend aid _ -> aid == allyIdOf defender; _ -> False)
        choose "Dora Milaje defense response" (isAbilityOfCard defender)
        choose "partner's Special" (isTargetChoice $ allyIdOf partner)
        if other == "51023" then chooseVillain else chooseMainScheme
        pass
        if other == "51023" then villainDamageNow `shouldReturn` 1 else mainSchemeThreatNow `shouldReturn` 4
