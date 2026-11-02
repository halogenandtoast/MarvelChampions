module Marvel.Encounter.HerculesNemesisSpec (spec) where

import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HM
import Marvel.Hercules.Cards qualified as Cards
import Marvel.Hercules.Helpers
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Trait qualified as T
import Marvel.Matchers
import Marvel.Query
import Marvel.Keyword
import Marvel.Modifier
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero you = changeForm you >> pass >> clearHand you

privateDecks :: IdentityId -> GameAppT ([EncounterCard], [PlayerCard])
privateDecks you = do
  attrs <- playerAttrs you
  let values = [toJSON ae | AlterEgoSide ae <- HM.elems $ playerIdentitySides attrs]
  pure $ fromMaybe ([], []) $ headMay
    [(labor, gifts) | Object o <- values, Just lv <- [KM.lookup "laborDeck" o], Just gv <- [KM.lookup "giftDeck" o], Success labor <- [fromJSON lv], Success gifts <- [fromJSON gv]]

prepareLabor :: IdentityId -> CardCode -> GameAppT EncounterCard
prepareLabor you code = do
  (labor, gifts) <- privateDecks you
  let selected = fromMaybe (error "No matching Labor") $ headMay $ filter ((== code) . toCardCode) labor
      reordered = selected : filter (/= selected) labor
  runNow [CardSpecific (IdentityRef you) "hcSetDecks" $ toJSON (reordered, gifts)]
  pure selected

isNewLabor :: IdentityId -> Choice -> Bool
isNewLabor you = \case
  UseAbility ab -> abilitySource ab == toSource you && abilityIndex ab == 2
  _ -> False

completeHydraReveal :: IdentityId -> GameAppT (EncounterCard, MinionId)
completeHydraReveal you = do
  labor <- prepareLabor you "59002"
  candidates <- findEncounterCards $ CardIs Cards.hc59037
  let hydra = fromMaybe (error "No Hydra in nemesis set") $ headMay candidates
  choose "New Labors" (isNewLabor you)
  choose "Lernean Hydra" (isTargetChoice hydra)
  pass
  pure (labor, minionOf hydra)

spec :: Spec
spec = describe "Hercules nemesis and Labors" do
  it "sets up three distinct Labor cards and three Gift cards outside ordinary decks" do
    tg <- testGame ["hercules"]
    inGame tg do
      (labor, gifts) <- privateDecks $ player1 tg
      map toCardCode labor `shouldMatchList` ["59002", "59003", "59004"]
      map toCardCode gifts `shouldMatchList` ["59005", "59006", "59007"]

  it "Defeat the Hydra reveals a six-printed-HP minion, adds six HP, and grants Elite" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      (_, hydra) <- completeHydraReveal you
      getEnemyPrintedHitPoints (EnemyMinionId hydra) `shouldReturn` 6
      getEnemyRemainingHitPoints (EnemyMinionId hydra) `shouldReturn` 12
      enemyMatches (EnemyWithTrait T.Elite) (EnemyMinionId hydra) `shouldReturn` True
      offers (isNewLabor you) `shouldReturn` False

  it "a discarded Labor returns to the bottom of its own deck instead of the encounter discard" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      (labor, _) <- completeHydraReveal you
      resolve [RemoveFromPlay $ AttachmentRef $ AttachmentId $ unCardId $ ecCardId labor]
      (deck, _) <- privateDecks you
      length deck `shouldBe` 3
      toCardCode <$> lastMay deck `shouldBe` Just "59002"
      discarded <- encounterDiscard
      any ((== "59002") . toCardCode) discarded `shouldBe` False

  it "completing a Labor adds it to victory and grants a Gift through Atonement" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      (_, hydra) <- completeHydraReveal you
      asHero you
      (_, giftsBefore) <- privateDecks you
      let gift = fromMaybe (error "No Gift") $ headMay giftsBefore
      runNow [MinionMessage hydra MinionDefeated]
      choose "Atonement" (\case UseAbility ab -> abilitySource ab == toSource you && abilityIndex ab == 1; _ -> False)
      choose "draw four from the Gift" (isAbilityOfCard gift)
      choose "remain in hero form" (isLabel "$label.done")
      length <$> handOf you `shouldReturn` 4
      victory <- getVictoryDisplay
      any ((== "59002") . toCardCode) victory `shouldBe` True
      giftCount you `shouldReturn` 1
      (deck, gifts) <- privateDecks you
      length deck `shouldBe` 2
      length gifts `shouldBe` 2

  it "Hercules's labeled event attacks can damage the Hydra Labor's host" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      (_, hydra) <- completeHydraReveal you
      asHero you
      card <- putInHand you "59009"
      payment <- traverse (putInHand you) ["01088", "59008"]
      playWith card payment
      offers (isTargetChoice hydra) `shouldReturn` True
      choose "attack Hydra" (isTargetChoice hydra)
      pass
      getMinionDamageInPlay hydra >>= (\n -> n `shouldSatisfy` (> 0))

  it "nonattack ability damage cannot damage the Hydra Labor's host" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      (_, hydra) <- completeHydraReveal you
      Lifted.runQueueT (Lifted.capture $ Lifted.dealDamage hydra GameRef $ toDamage 3 FromAbility) >>= resolve
      getMinionDamageInPlay hydra `shouldReturn` 0

  it "Embody Pathos treats the target's base threat and Hinder per-player symbols as one" do
    tg <- testGame ["hercules", "wonder-man"]
    inGame tg do
      let you = player1 tg
      _ <- prepareLabor you "59003"
      ([scheme], _) <- setEncounterDeck [Cards.hc59041] [Cards.hc59040]
      choose "New Labors" (isNewLabor you)
      choose "All Versus All" (isTargetChoice scheme)
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 10

  it "Protect Humanity finds Amadeus Cho and redirects the villain's attack to an ally" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      _ <- prepareLabor you "59004"
      choose "New Labors" (isNewLabor you)
      pass
      selectAny (AllyIs Cards.hc59008) `shouldReturn` True
      asHero you
      before <- damageOf you
      Lifted.runQueueT (Lifted.capture $ Lifted.currentVillainAttacks you) >>= resolve
      damageOf you `shouldReturn` before

  it "Appeal to Athena counts Gifts as zero while retaining their permanent hit points" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "59005"
      _ <- revealNow you Cards.hc59035
      pass
      giftCount you `shouldReturn` 0
      hitPointsOf you `shouldReturn` 16

  it "Ares has retaliate one" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      asHero you
      ares <- addMinion you Cards.hc59036
      choose "Hercules attacks" (isHeroAttack you)
      choose "Ares" (isTargetChoice ares)
      pass
      damageOf you `shouldReturn` 1

  it "Olympic Feud adds threat for each Olympus card in play" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "59012"
      scheme <- revealNow you Cards.hc59038
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4

  it "Ares's Axe attaches to the villain when Ares is absent and gives plus two ATK" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      villain <- villainIdNow
      before <- getVillainAttackValue villain
      _ <- revealNow you Cards.hc59039
      pass
      getVillainAttackValue villain `shouldReturn` before + 2

  it "God of War finds and reveals a minion when none activate" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      _ <- setEncounterDeck [Cards.hc59036] [Cards.hc59040]
      card <- encounterCard Cards.hc59040
      resolve [RevealEncounterCard you card]
      selectAny (MinionIs Cards.hc59036) `shouldReturn` True

  it "All Versus All receives threat when an ally is defeated" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      scheme <- revealNow you Cards.hc59041
      pass
      ally <- putIntoPlay you "59008"
      before <- sideSchemeThreat $ sideSchemeOf scheme
      resolve [AllyMessage (allyIdOf ally) AllyDefeatedByDamage]
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` before + 2

  it "Hecate reveals All Versus All" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      _ <- setEncounterDeck [Cards.hc59041] [Cards.hc59040]
      card <- encounterCard Cards.hc59042
      resolve [RevealEncounterCard you card]
      selectAny (SideSchemeIs Cards.hc59041) `shouldReturn` True

  it "Kyknos has toughness and retaliate one" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      kyknos <- addMinion you Cards.hc59043
      getCharacterStatusCount (MinionCharacter kyknos) ToughStatus `shouldReturn` 1
      modifiers <- getEnemyKeywords $ EnemyMinionId kyknos
      (Retaliate 1 `elem` modifiers) `shouldBe` True

  it "Bewitched Officer's attacks are ranged and piercing" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      officer <- addMinion you Cards.hc59044
      modifiers <- getModifiers $ MinionRef officer
      (KeywordModifier Piercing `elem` modifiers) `shouldBe` True
      (KeywordModifier Ranged `elem` modifiers) `shouldBe` True

  it "Roving Mobs finds All Versus All when defeated" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      _ <- setEncounterDeck [Cards.hc59041] [Cards.hc59040]
      mobs <- revealNow you Cards.hc59045
      pass
      resolve [SideSchemeMessage (sideSchemeOf mobs) DefeatSideScheme]
      selectAny (SideSchemeIs Cards.hc59041) `shouldReturn` True

  it "a Labor with no valid target returns to the bottom of its deck" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      candidates <- findEncounterCards $ CardIs Cards.hc59037
      for_ candidates $ \card -> Lifted.runQueueT (Lifted.capture $ Lifted.dealFoundEncounterCard you card) >>= resolve
      _ <- prepareLabor you "59002"
      choose "New Labors" (isNewLabor you)
      pass
      (deck, _) <- privateDecks you
      length deck `shouldBe` 3
      toCardCode <$> lastMay deck `shouldBe` Just "59002"
      selectAny (AttachmentIs Cards.hc59002) `shouldReturn` False

  it "Embody Pathos permits Hercules's event thwart" do
    tg <- testGame ["hercules", "wonder-man"]
    inGame tg do
      let you = player1 tg
      _ <- prepareLabor you "59003"
      ([scheme], _) <- setEncounterDeck [Cards.hc59041] [Cards.hc59040]
      choose "New Labors" (isNewLabor you)
      choose "All Versus All" (isTargetChoice scheme)
      pass
      asHero you
      card <- putInHand you "59011"
      payment <- traverse (putInHand you) ["01088", "59008"]
      playWith card payment
      offers (isTargetChoice $ sideSchemeOf scheme) `shouldReturn` True
      choose "Hercules thwarts the Labor" (isTargetChoice $ sideSchemeOf scheme)
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 6

  it "Embody Pathos forbids an ally's thwart on its attached side scheme" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      _ <- prepareLabor you "59003"
      ([scheme], _) <- setEncounterDeck [Cards.hc59041] [Cards.hc59040]
      choose "New Labors" (isNewLabor you)
      choose "All Versus All" (isTargetChoice scheme)
      pass
      asHero you
      _ <- putIntoPlay you "59008"
      cho <- allyIn "59008"
      placeMainThreat 2
      choose "Cho thwarts" (isAllyThwart cho)
      offers (isTargetChoice $ sideSchemeOf scheme) `shouldReturn` False

  it "Protect Humanity loses one counter only after Hercules defends the redirected attack" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      labor <- prepareLabor you "59004"
      choose "New Labors" (isNewLabor you)
      pass
      asHero you
      villain <- villainIdNow
      runNow [VillainMessage villain $ VillainAttacksWithoutBoost you]
      choose "Protect Humanity" (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId labor)
      cho <- allyIn "59008"
      choose "target Cho" (isTargetChoice $ allyId cho)
      choose "Hercules defends Cho" isDefend
      pass
      getCountersOn (ObligationRef $ ObligationId $ unCardId $ ecCardId labor) `shouldReturn` 2

  it "Protect Humanity with no ally leaves the original villain attack against Hercules" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      _ <- prepareLabor you "59004"
      choose "New Labors" (isNewLabor you)
      pass
      cho <- allyIn "59008"
      resolve [AllyMessage (allyId cho) AllyDefeatedByDamage]
      asHero you
      villain <- villainIdNow
      resolve [VillainMessage villain $ VillainAttacksWithoutBoost you]
      damageOf you `shouldReturn` 2

  it "Defeat the Hydra preserves an in-play host's status and upgrades while explicitly healing its damage" do
    tg <- testGame ["hercules"]
    inGame tg do
      let you = player1 tg
      hydra <- addMinion you Cards.hc59037
      caught <- putInHand you "58030"
      playOn caught hydra []
      resolve [MinionMessage hydra $ MinionDamaged GameRef $ toDamage 1 FromAbility, GiveStatus (MinionRef hydra) ConfusedStatus]
      _ <- prepareLabor you "59002"
      candidates <- findEncounterCards $ CardIs Cards.hc59037
      let selected = fromMaybe (error "No physical host") $ headMay $ filter ((== CardId (unMinionId hydra)) . ecCardId) candidates
      choose "New Labors" (isNewLabor you)
      choose "the existing Hydra" (isTargetChoice selected)
      pass
      getMinionDamageInPlay hydra `shouldReturn` 0
      getCharacterStatusCount (MinionCharacter hydra) ConfusedStatus `shouldReturn` 1
      upgrades <- getsGame $ maybe mempty (minionUpgrades . toAttrs) . HM.lookup hydra . gameMinions
      member (upgradeIdOf caught) upgrades `shouldBe` True

  it "Protect Humanity cannot count a later defense for another player after an undefended redirected attack" do
    tg <- testGame ["hercules", "wonder-man"]
    inGame tg do
      let you = player1 tg
          other = player2 tg
      labor <- prepareLabor you "59004"
      choose "New Labors" (isNewLabor you)
      pass
      asHero you
      villain <- villainIdNow
      resolve [VillainMessage villain $ VillainAttacksWithoutBoost you]
      let obligation = ObligationRef $ ObligationId $ unCardId $ ecCardId labor
      getCountersOn obligation `shouldReturn` 3
      runNow [IdentityMessage other $ ChangedToForm A]
      runNow [VillainMessage villain $ VillainAttacksWithoutBoost other]
      choose "ask Hercules to defend" (isTargetChoice you)
      choose "Hercules defends" (isTargetChoice you)
      pass
      getCountersOn obligation `shouldReturn` 3

  it "All Versus All has two fixed threat plus two Hinder threat per player and a hazard icon" do
    tg <- testGame ["hercules", "wonder-man"]
    inGame tg do
      scheme <- revealNow (player1 tg) Cards.hc59041
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 6
      getHazardCount `shouldReturn` 1

  it "Olympic Feud has fixed initial threat and a hazard icon in multiplayer" do
    tg <- testGame ["hercules", "wonder-man"]
    inGame tg do
      scheme <- revealNow (player1 tg) Cards.hc59038
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 3
      getHazardCount `shouldReturn` 1

  it "Roving Mobs has three fixed threat and its crisis icon blocks the main scheme" do
    tg <- testGame ["hercules", "wonder-man"]
    inGame tg do
      scheme <- revealNow (player1 tg) Cards.hc59045
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 3
      hasCrisisInYourArea `shouldReturn` True
      schemes <- selectList ThwartableScheme
      any (\case SchemeMainSchemeId _ -> True; _ -> False) schemes `shouldBe` False
