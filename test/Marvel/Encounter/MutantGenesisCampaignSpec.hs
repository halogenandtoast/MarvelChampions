module Marvel.Encounter.MutantGenesisCampaignSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Control.Monad (replicateM_)
import Marvel.Campaign
import Marvel.Obligation.Types (obligationOwner)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Environment.Types qualified as Environment
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Support.Types qualified as Support
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Window qualified as W
import Test.Hspec qualified as Hspec
import TestImport

schemeId :: EncounterCard -> SideSchemeId
schemeId = SideSchemeId . unCardId . toCardId

thwartAll :: IdentityId -> SideSchemeId -> GameAppT ()
thwartAll you sid = do
  n <- getsGame (maybe 0 (SideScheme.sideSchemeThreat . toAttrs) . HashMap.lookup sid . gameSideSchemes)
  resolve [SideSchemeMessage sid (SideSchemeThwarted (toSource you) n)]

-- | Decline earlier response/defender windows until the requested player choice.
reachChoice :: (Choice -> Bool) -> GameAppT ()
reachChoice wanted = go (40 :: Int)
 where
  go 0 = error "requested choice did not appear"
  go n = do
    found <- offers wanted
    unless found do
      open <- asked
      case open of
        Just (_, choices) | any isNoResponse choices -> choose "pass earlier response" isNoResponse >> go (n - 1)
        Just (_, choices) | any isNoDefender choices -> choose "decline defender" isNoDefender >> go (n - 1)
        Just (_, choices) | any (isLabel "$label.declineToDefend") choices -> choose "decline another defender" (isLabel "$label.declineToDefend") >> go (n - 1)
        _ -> error $ "unexpected question before requested choice: " <> show open

spec :: Spec
spec = describe "Mutant Genesis campaign" do
  describe "campaign log" do
    it "has the five scenarios in order" do
      campaignScenarios MutantGenesis `Hspec.shouldBe` ["32060","32084","32109","32121a","32138"]
    it "starts with empty role choices, Future Past history and reward usage" do
      let c = newCampaignOf MutantGenesis StandardCampaign ["01001a","01010a"]
      map (.role) c.campaignLog.mutantGenesis.mutantPlayers `Hspec.shouldBe` [Nothing,Nothing]
      c.campaignLog.mutantGenesis.futurePastRemoved `Hspec.shouldBe` []
      c.campaignLog.mutantGenesis.roleUpgradesRemoved `Hspec.shouldBe` []
    it "records the chosen role without changing another player's choice" do
      let c = recordInLog (RecordMutantRole "01001a" Brawler) (newCampaignOf MutantGenesis StandardCampaign ["01001a","01010a"])
      map (.role) c.campaignLog.mutantGenesis.mutantPlayers `Hspec.shouldBe` [Just Brawler,Nothing]
    it "keeps permanently defeated Future Past cards while replacing the active list" do
      let c = newCampaignOf MutantGenesis StandardCampaign ["01001a"]
          c' = recordInLog (RecordFuturePast ["32167"] ["32166"]) c
          c'' = recordInLog (RecordFuturePast ["32168"] ["32167"]) c'
      c''.campaignLog.mutantGenesis.futurePastActive `Hspec.shouldBe` ["32168"]
      c''.campaignLog.mutantGenesis.futurePastRemoved `Hspec.shouldBe` ["32166","32167"]
    it "removes a role upgrade from both the pool and this game's tracked upgrades" do
      let c = recordInLog (RemoveMutantRoleUpgrade "32176")
            $ recordInLog (RecordMutantRoleUpgrade "32176")
            $ newCampaignOf MutantGenesis StandardCampaign ["01001a"]
      c.campaignLog.mutantGenesis.roleUpgradesRemoved `Hspec.shouldBe` ["32176"]
      c.campaignLog.mutantGenesis.roleUpgradesInPlay `Hspec.shouldBe` []
    it "keeps each role's five physical cards distinct despite duplicated titles" do
      map mutantRoleCards [Brawler,Commander,Defender,Peacekeeper]
        `Hspec.shouldBe` [["32176","32177","32178","32179","32180"],["32181","32182","32183","32184","32185"],["32186","32187","32188","32189","32190"],["32191","32192","32193","32194","32195"]]

  describe "Frightened Police and Metro P.D." do
    it "starts with three threat per player and flips to a first-player support" do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        card <- revealNow (player2 tg) Schemes.mgFrightenedPolice
        let sid = schemeId card
        getsGame (fmap (SideScheme.sideSchemeThreat . toAttrs) . HashMap.lookup sid . gameSideSchemes) `shouldReturn` Just 6
        thwartAll (player2 tg) sid
        supports <- getsGame (map toAttrs . HashMap.elems . gameSupports)
        let police = filter ((== "32171b") . toCardCode) supports
        liftIO $ map Support.supportController police `shouldBe` [player1 tg]

    it "offers its damage action, dealing one damage and exhausting Metro P.D." do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        police <- putIntoPlay p "32171b"
        before <- villainDamageNow
        choose "Metro P.D." (isAbilityOfCard police)
        choose "deal damage" (isLabel "$label.mutantMetroDamage")
        chooseVillain
        villainDamageNow `shouldReturn` (before + 1)
        support <- getsGame (HashMap.lookup (SupportId $ unCardId $ toCardId police) . gameSupports)
        liftIO $ fmap (Support.supportExhausted . toAttrs) support `shouldBe` Just True

  describe "Enemy of My Enemy and Magneto" do
    it "flips the defeated scheme to the Magneto ally under the first player's control" do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        front <- revealNow (player2 tg) Schemes.mgEnemyOfMyEnemy
        thwartAll (player2 tg) (schemeId front)
        magneto <- allyIn "32172b"
        liftIO $ allyController magneto `shouldBe` player1 tg

  describe "role upgrades" do
    for_ ["32179","32184"] \code ->
      it ("attacks for six then readies the correct characters: " <> show code) do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          changeForm p
          pass
          clearHand p
          role <- putIntoPlay p code
          money <- traverse (putInHand p) ["01088","01089"]
          choose "role attack" (isAbilityOfCard role)
          for_ money \card -> chooseIf (isPayWith card)
          chooseIf isFinishPayment
          chooseVillain
          pass
          villainDamageNow `shouldReturn` 6
          getsGame (HashMap.member (UpgradeId $ unCardId $ toCardId role) . gameUpgrades) `shouldReturn` False

    for_ ["32183","32193"] \code ->
      it ("removes the consumed consequential-damage protection upgrade: " <> show code) do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          changeForm p
          pass
          role <- putIntoPlay p code
          choose "activate role" (isAbilityOfCard role)
          pass
          getsGame (HashMap.member (UpgradeId $ unCardId $ toCardId role) . gameUpgrades) `shouldReturn` False

    for_ [("32180","01087"),("32185","01087"),("32190","01060"),("32195","01070")] \(code,event) ->
      it ("generates two wild resources for its permitted event and consumes itself: " <> show code) do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          changeForm p
          pass
          clearHand p
          role <- putIntoPlay p code
          when (code == "32185") $ void $ putIntoPlay p "01002"
          when (code == "32190") $ do
            scheme <- mainSchemeIdNow
            resolve [MainSchemeMessage scheme (MainSchemePlaceThreat 5)]
          eventCard <- putInHand p event
          choose "play event" (isPlay eventCard)
          choose "generate role resources" (isAbilityOfCard role)
          whenM (offers isFinishPayment) $ choose "finish payment" isFinishPayment
          chooseIf (isLabelStarting "$label.spendResourceAllocation")
          when (code == "32185") $ do
            cat <- allyIn "01002"
            choose "ready and heal Black Cat" (isTargetChoice $ allyId cat)
          when (code == "32195") chooseVillain
          whenM (offers isFinishPayment) $ choose "finish event payment" isFinishPayment
          chooseIf (isLabelStarting "$label.spendResourceAllocation")
          chooseIf (\case TargetLabel (VillainRef _) _ -> True; TargetLabel (EnemyRef (EnemyVillainId _)) _ -> True; _ -> False)
          pass
          getsGame (HashMap.member (UpgradeId $ unCardId $ toCardId role) . gameUpgrades) `shouldReturn` False

    for_ ["32176","32181"] \code ->
      it ("adds three damage to a basic attack and removes the card: " <> show code) do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          changeForm p
          pass
          role <- putIntoPlay p code
          choose "attack" (isHeroAttack p)
          chooseVillain
          reachChoice (isAbilityOfCard role)
          choose "use role interrupt" (isAbilityOfCard role)
          pass
          villainDamageNow `shouldReturn` 5
          getsGame (HashMap.member (UpgradeId $ unCardId $ toCardId role) . gameUpgrades) `shouldReturn` False

    for_ ["32182","32192"] \code ->
      it ("offers Compassion after recovering, and only in alter ego: " <> show code) do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          role <- putIntoPlay p code
          offers (isAbilityOfCard role) `shouldReturn` False
          resolve [IdentityMessage p (IdentityDamaged GameRef $ toDamage 6 FromAbility)]
          choose "recover" (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 200; _ -> False)
          choose "Compassion response" (isAbilityOfCard role)
          pass
          damageOf p `shouldReturn` 0
          getsGame (HashMap.member (UpgradeId $ unCardId $ toCardId role) . gameUpgrades) `shouldReturn` False

    for_ ["32177","32186"] \code ->
      it ("Swagger improves basic defense and readies its hero: " <> show code) do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          changeForm p
          pass
          role <- putIntoPlay p code
          _ <- stackEncounterDeck [Minions.vulture]
          vid <- villainIdNow
          runNow [VillainMessage vid (VillainAttacks p)]
          reachChoice isDefend
          choose "defend" isDefend
          choose "Swagger" (isAbilityOfCard role)
          pass
          damageOf p `shouldReturn` 0
          exhaustedNow p `shouldReturn` False
          getsGame (HashMap.member (upgradeOf role) . gameUpgrades) `shouldReturn` False

    for_ ["32187","32191"] \code ->
      it ("Surprise removes three additional threat after a hero thwart: " <> show code) do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          changeForm p
          pass
          role <- putIntoPlay p code
          sid <- mainSchemeIdNow
          resolve [MainSchemeMessage sid (MainSchemePlaceThreat 6)]
          choose "thwart" (isHeroThwart p)
          choose "main scheme" (isTargetChoice $ SchemeRef $ SchemeMainSchemeId sid)
          choose "Surprise" (isAbilityOfCard role)
          replicateM_ 3 (choose "assign threat removal" $ isTargetChoice sid)
          chooseVillain
          pass
          mainSchemeThreatNow `shouldReturn` 2
          getsGame (HashMap.member (upgradeOf role) . gameUpgrades) `shouldReturn` False

    for_ ["32188","32194"] \code ->
      it ("removes five divided threat as a role thwart action: " <> show code) do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          changeForm p
          pass
          clearHand p
          role <- putIntoPlay p code
          sid <- mainSchemeIdNow
          resolve [MainSchemeMessage sid (MainSchemePlaceThreat 6)]
          money <- traverse (putInHand p) ["01088","01089"]
          choose "role thwart" (isAbilityOfCard role)
          payWith money
          pass
          mainSchemeThreatNow `shouldReturn` 1
          getsGame (HashMap.member (upgradeOf role) . gameUpgrades) `shouldReturn` False

    it "Brazen Defense deals three attack damage and prevents three of the enemy attack" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        role <- putIntoPlay p "32178"
        money <- putInHand p "01088"
        _ <- stackEncounterDeck [Minions.vulture]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        reachChoice (isAbilityOfCard role)
        choose "Brazen Defense" (isAbilityOfCard role)
        payWith [money]
        pass
        villainDamageNow `shouldReturn` 3
        damageOf p `shouldReturn` 1
        getsGame (HashMap.member (upgradeOf role) . gameUpgrades) `shouldReturn` False

    it "Determined Defense replaces attack damage after basic defense with threat removal" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        role <- putIntoPlay p "32189"
        money <- putInHand p "01088"
        sid <- mainSchemeIdNow
        resolve [MainSchemeMessage sid (MainSchemePlaceThreat 6)]
        _ <- stackEncounterDeck [Minions.vulture]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        reachChoice isDefend
        choose "defend" isDefend
        choose "Determined Defense" (isAbilityOfCard role)
        payWith [money]
        pass
        choose "remove threat from the main scheme" (isTargetChoice sid)
        pass
        damageOf p `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` 5
        getsGame (HashMap.member (upgradeOf role) . gameUpgrades) `shouldReturn` False

  describe "Find the Prisoners and Rescue Captives" do
    it "preserves each chosen facedown ally when the side scheme becomes an environment" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        allies <- stackDeck p ["01002"]
        front <- revealNow p Schemes.mgFindThePrisoners
        choose "hide Black Cat" (isTargetChoice $ head allies)
        let sid = schemeId front
        before <- getsGame (maybe [] (SideScheme.sideSchemeHeldCards . toAttrs) . HashMap.lookup sid . gameSideSchemes)
        liftIO $ map toCardId before `shouldBe` map toCardId allies
        thwartAll p sid
        environments <- getsGame (map toAttrs . HashMap.elems . gameEnvironments)
        let rescued = [Environment.environmentHeldCards attrs | attrs <- environments, toCardCode attrs == "32173b"]
        liftIO $ map (map toCardId) rescued `shouldBe` [map toCardId allies]
        getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` False

  describe "Surprise Attack and Reactive Defense" do
    it "gives its reverse to the player who defeated the side scheme" do
      tg <- testGame ["spider-man","captain-marvel"]
      let p = player2 tg
      inGame tg do
        front <- revealNow (player1 tg) Schemes.mgSurpriseAttack
        thwartAll p (schemeId front)
        obligations <- getsGame (map toAttrs . HashMap.elems . gameObligations)
        let reactive = [obligationOwner attrs | attrs <- obligations, toCardCode attrs == "32174b"]
        liftIO $ reactive `shouldBe` [Just p]

    it "requires alter ego and the three different resource types, damages every enemy, then removes itself" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        front <- revealNow p Schemes.mgSurpriseAttack
        thwartAll p (schemeId front)
        money <- traverse (putInHand p) ["01088","01089","01090"]
        let isReactive = \case
              UseAbility ab -> case abilitySource ab of
                ObligationRef _ -> True
                _ -> False
              _ -> False
        choose "Reactive Defense" isReactive
        payWith money
        pass
        villainDamageNow `shouldReturn` 5
        remaining <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameObligations)
        liftIO $ remaining `shouldNotContain` ["32174b"]

  describe "Magneto's Fortress and Magneto's Power" do
    it "flips into a permanent attachment giving Magneto one attack and scheme" do
      tg <- testGameAgainst "32138" ["spider-man"]
      let p = player1 tg
      inGame tg do
        vid <- villainIdNow
        before <- getVillainAttackValue vid
        front <- revealNow p Schemes.mgMagnetosFortress
        thwartAll p (schemeId front)
        getVillainAttackValue vid `shouldReturn` (before + 1)
