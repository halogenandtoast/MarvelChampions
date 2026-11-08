module Marvel.Hero.SilkSpec (spec) where
import TestImport
import Control.Monad (replicateM_)
import Marvel.Card.Types qualified as MarvelCard
import Marvel.Query
import Marvel.Villain.Types qualified as V
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Silk.Cards qualified as Cards
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers
import Marvel.Trait qualified as T

perform :: L.QueueT GameAppT () -> GameAppT ()
perform action = L.runQueueT (L.capture action) >>= runNow

tuck :: IdentityId -> GameAppT ()
tuck p = perform (L.tuckTopEncounterCard p) >> pass

identityAbility :: IdentityId -> Choice -> Bool
identityAbility p = \case
  UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 1
  _ -> False

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

spec :: Spec
spec = describe "Silk" do
  it "Cindy Moon has six hand size and ten hit points" do
    tg <- testGame ["silk"]
    inGame tg do
      handSizeOf (player1 tg) `shouldReturn` 6
      hitPointsOf (player1 tg) `shouldReturn` 10

  it "her hero has five hand size" do
    tg <- testGame ["silk"]
    inGame tg do
      asHero (player1 tg)
      handSizeOf (player1 tg) `shouldReturn` 5

  it "Cindy cannot discard a tucked card when none is available" do
    tg <- testGame ["silk"]
    inGame tg do
      clearHand (player1 tg)
      offers (identityAbility $ player1 tg) `shouldReturn` False

  it "Cindy discards one tucked card to draw two, once per round" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      tuck p
      cards <- L.tuckedEncounterCards p
      choose "Cindy Moon" (identityAbility p)
      choose "spend a tucked card" (isTargetChoice $ testHead cards)
      pass
      (length <$> handOf p) `shouldReturn` 2
      L.tuckedEncounterCards p `shouldReturn` []
      tuck p
      offers (identityAbility p) `shouldReturn` False

  it "changing form preserves the same actual tucked cards" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      tuck p
      before <- L.tuckedEncounterCards p
      changeForm p
      pass
      L.tuckedEncounterCards p `shouldReturn` before

  it "a fifth tucked card is trimmed by the controlling player" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      replicateM_ 4 (tuck p)
      previous <- L.tuckedEncounterCards p
      perform (L.tuckTopEncounterCard p)
      choose "discard a tucked card" (\case CardLabel code _ -> code == toCardCode (testHead previous); _ -> False)
      pass
      (length <$> L.tuckedEncounterCards p) `shouldReturn` 4
      ((testHead previous `elem`) <$> L.tuckedEncounterCards p) `shouldReturn` False

  it "Albert Moon can tuck the top encounter card under Cindy" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      albert <- putIntoPlay p "52006"
      before <- testHead <$> getEncounterDeck
      choose "Albert Moon" (isAbilityOfCard albert)
      choose "tuck top card" (isLabel "$label.silkTuckTop")
      pass
      L.tuckedEncounterCards p `shouldReturn` [before]

  it "Albert Moon heals one damage per tucked card" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      albert <- putIntoPlay p "52006"
      replicateM_ 2 (tuck p)
      setDamage p 5
      choose "Albert Moon" (isAbilityOfCard albert)
      choose "heal" (isLabel "$label.heal")
      pass
      damageOf p `shouldReturn` 3

  it "Swinging Silk Kick deals seven when its optional tuck payment is declined" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      kick <- putInHand p "52003"
      pay1 <- putInHand p "01088"
      pay2 <- putInHand p "01089"
      playWith kick [pay1,pay2]
      chooseVillain
      choose "no tucked payment" (isLabel "$label.attackWithoutDiscarding")
      pass
      villainDamageNow `shouldReturn` 7

  it "Swinging Silk Kick can discard a tucked Rhino card for nine damage" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      stampede <- encounterCard Treacheries.stampede
      perform (L.tuckEncounterCard p stampede)
      pass
      kick <- putInHand p "52003"
      pay1 <- putInHand p "01088"
      pay2 <- putInHand p "01089"
      playWith kick [pay1,pay2]
      chooseVillain
      offers (isLabel "$label.attackWithoutDiscarding") `shouldReturn` True
      choose "discard Stampede" (isTargetChoice stampede)
      pass
      villainDamageNow `shouldReturn` 9
      L.tuckedEncounterCards p `shouldReturn` []

  it "Swinging Silk Kick matches tucked Rhino cards in saves missing encounter-set metadata" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      vid <- villainIdNow
      withGame_ $ entitiesL . villainsL . ix vid %~ \(V.Villain v) -> V.Villain $ V.overVillainAttrs
        (\attrs -> case fromJSON (toJSON attrs {V.villainCardDef = (V.villainCardDef attrs) {cdEncounterSet = Nothing}}) of
          Success restored -> restored
          Error err -> error err) v
      stampede <- encounterCard Treacheries.stampede
      perform (L.tuckEncounterCard p stampede)
      pass
      kick <- putInHand p "52003"
      pay1 <- putInHand p "01088"
      pay2 <- putInHand p "01089"
      playWith kick [pay1,pay2]
      chooseVillain
      offers (isLabel "$label.attackWithoutDiscarding") `shouldReturn` True
      choose "discard Stampede" (isTargetChoice stampede)
      pass
      villainDamageNow `shouldReturn` 9
      L.tuckedEncounterCards p `shouldReturn` []

  it "Wallcrawl first removes two threat even with no matching tuck" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      card <- putInHand p "52004"
      payment <- putInHand p "01093"
      playWith card [payment]
      chooseMainScheme
      chooseMainScheme
      choose "no matching payment" (isLabel "$label.done")
      pass
      mainSchemeThreatNow `shouldReturn` before - 2

  it "Organic Webbing increases THW and spends a tuck to ready and gain Aerial" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      webbing <- putIntoPlay p "52009"
      tuck p
      cards <- L.tuckedEncounterCards p
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      pass
      exhaustedNow p `shouldReturn` True
      choose "Organic Webbing" (isAbilityOfCard webbing)
      choose "discard tucked card" (isTargetChoice $ testHead cards)
      pass
      exhaustedNow p `shouldReturn` False
      identityMatches (IdentityWithTrait T.Aerial) p `shouldReturn` True

  it "Silk Sense tucks a minion defeated by a Silk event" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.hydraRegular
      kick <- putInHand p "52003"
      payment <- traverse (putInHand p) ["01088","01089"]
      playWith kick payment
      choose "target minion" (isTargetChoice mid)
      choose "no tucked card" (isLabel "$label.attackWithoutDiscarding")
      choose "Silk Sense" (identityAbility p)
      choose "tuck the defeated minion" (\case TargetLabel (CardIdRef _) _ -> True; _ -> False)
      pass
      (length <$> L.tuckedEncounterCards p) `shouldReturn` 1

  it "J. Jonah Jameson removes two threat from a side scheme" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      jonah <- putIntoPlay p "52007"
      scheme <- revealNow p Schemes.breakinAndTakin
      pass
      before <- sideSchemeThreat (sideSchemeOf scheme)
      choose "Jameson action" (\case UseAbility ab -> abilitySource ab == SupportRef (SupportId (unCardId $ pcCardId jonah)) && abilityIndex ab == 2; _ -> False)
      choose "side scheme" (isTargetChoice $ sideSchemeOf scheme)
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` before - 2

  it "Eidetic Memory swaps matching encounter sets in alter-ego without discarding the tucked card" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      memory <- putIntoPlay p "52008"
      hunted <- encounterCard Cards.huntingtheSpiderBrideSilk
      perform (L.tuckEncounterCard p hunted)
      pass
      _ <- revealNow p Cards.theGreatHuntSilk
      choose "Eidetic Memory" (isAbilityOfCard memory)
      choose "swap Hunting the Spider-Bride" (isTargetChoice hunted)
      pass
      damageOf p `shouldReturn` 0
      tucked <- L.tuckedEncounterCards p
      map toCardCode tucked `shouldSatisfy` elem "52030"

  it "Eidetic Memory is not offered for an unrelated encounter set" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      memory <- putIntoPlay p "52008"
      hunted <- encounterCard Cards.huntingtheSpiderBrideSilk
      perform (L.tuckEncounterCard p hunted)
      pass
      _ <- revealNow p Schemes.breakinAndTakin
      offers (isAbilityOfCard memory) `shouldReturn` False

  it "Outwit adds one THW for a tucked card in the thwarted scheme's encounter set" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      outwit <- putIntoPlay p "52010"
      hunted <- encounterCard Cards.huntingtheSpiderBrideSilk
      perform (L.tuckEncounterCard p hunted)
      pass
      scheme <- revealNow p Cards.theGreatHuntSilk
      pass
      choose "basic thwart" (isHeroThwart p)
      choose "The Great Hunt" (isTargetChoice $ sideSchemeOf scheme)
      choose "Outwit" (isAbilityOfCard outwit)
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 1

  it "Spider Claws adds matching ATK and gives the basic attack piercing" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      claws <- putIntoPlay p "52011"
      hunted <- encounterCard Cards.huntingtheSpiderBrideSilk
      perform (L.tuckEncounterCard p hunted)
      pass
      mid <- addMinion p Cards.morlunSilk
      resolve [GiveStatus (MinionRef mid) ToughStatus]
      choose "basic attack" (isHeroAttack p)
      choose "Morlun" (isTargetChoice mid)
      choose "Spider Claws" (isAbilityOfCard claws)
      pass
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 2

  it "Spider Reflexes increases defense and tucks the top discard after the attack" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      reflexes <- putIntoPlay p "52012"
      mid <- addMinion p Cards.morlunSilk
      (_,discarded) <- setEncounterDeck [Treacheries.advance] [Cards.theGreatHuntSilk]
      runNow [MinionMessage mid (MinionAttacks p)]
      choose "Silk defends" isDefend
      choose "Spider Reflexes" (isAbilityOfCard reflexes)
      pass
      damageOf p `shouldReturn` 0
      tucked <- L.tuckedEncounterCards p
      tucked `shouldSatisfy` elem (testHead discarded)

  it "Outwit boosts only THW when The Red House offers the ATK option" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      outwit <- putIntoPlay p "52010"
      copies <- traverse (const $ encounterCard Schemes.theRedHouse) [1 :: Int,2]
      traverse_ (\card -> perform (L.tuckEncounterCard p card)) copies
      pass
      house <- revealNow p Schemes.theRedHouse
      pass
      let sid = sideSchemeOf house
      resolve [SideSchemeMessage sid $ SideSchemePlaceThreat 5]
      before <- sideSchemeThreat sid
      choose "basic thwart" (isHeroThwart p)
      choose "The Red House" (isTargetChoice sid)
      choose "Outwit" (isAbilityOfCard outwit)
      choose "use ATK" (isLabel "$label.thwartWithAtk")
      pass
      sideSchemeThreat sid `shouldReturn` before - 2

  it "Smooth as Silk tucks the exact matching discarded encounter card" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Cards.morlunSilk
      (deck,_) <- setEncounterDeck [Treacheries.advance,Cards.theGreatHuntSilk,Treacheries.assault] []
      card <- putInHand p "52002"
      playWith card []
      choose "Morlun's encounter set" (isTargetChoice mid)
      pass
      tucked <- L.tuckedEncounterCards p
      tucked `shouldBe` [testHead $ testTail deck]

  it "Get the Scoop exhausts Cindy to remove threat and tucks one of the top two on defeat" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      scoop <- putIntoPlay p "52005"
      let sid = SideSchemeId $ unCardId $ pcCardId scoop
      (deck,_) <- setEncounterDeck [Treacheries.advance,Treacheries.assault,Treacheries.advance] []
      choose "Get the Scoop" (isAbilityOf sid)
      choose "exhaust Cindy" (isTargetChoice p)
      pass
      sideSchemeThreat sid `shouldReturn` 2
      exhaustedNow p `shouldReturn` True
      resolve [IdentityMessage p ReadiedIdentity]
      choose "Get the Scoop again" (isAbilityOf sid)
      choose "exhaust Cindy" (isTargetChoice p)
      choose "tuck the second encounter card" (isTargetChoice $ testHead $ testTail deck)
      pass
      tucked <- L.tuckedEncounterCards p
      tucked `shouldBe` [testHead $ testTail deck]

  it "J. Jonah Jameson finds Get the Scoop in the discard pile and puts it into play" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      jonah <- putIntoPlay p "52007"
      scoop <- putInHand p "52005"
      resolve [DiscardedCard $ MarvelCard.toCard scoop]
      choose "Jameson finds Get the Scoop" (\case UseAbility ab -> abilitySource ab == SupportRef (SupportId $ unCardId $ pcCardId jonah) && abilityIndex ab == 1; _ -> False)
      choose "Get the Scoop in discard" (isTargetChoice scoop)
      pass
      selectAny (SideSchemeIs Cards.gettheScoopSilk) `shouldReturn` True

  it "Organic Webbing can spend a tucked card while Silk is ready to gain Aerial" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      asHero p
      webbing <- putIntoPlay p "52009"
      tuck p
      cards <- L.tuckedEncounterCards p
      exhaustedNow p `shouldReturn` False
      choose "Organic Webbing" (isAbilityOfCard webbing)
      choose "spend tucked card" (isTargetChoice $ testHead cards)
      pass
      exhaustedNow p `shouldReturn` False
      identityMatches (IdentityWithTrait T.Aerial) p `shouldReturn` True
      L.tuckedEncounterCards p `shouldReturn` []
