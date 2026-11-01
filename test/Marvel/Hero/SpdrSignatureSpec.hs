module Marvel.Hero.SpdrSignatureSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.ActiveCost (allocationLabel, syncRatioAllocationLabel)
import Marvel.Ally.Types qualified as Ally
import Marvel.Cost.Types
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Query
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Villain.Types (villainStunned)
import TestImport

asSpdr :: IdentityId -> GameAppT ()
asSpdr p = clearHand p >> changeForm p >> pass

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n choice = case choice of
  UseAbility a -> isAbilityOfCard card choice && abilityIndex a == n
  _ -> False

syncWith :: PlayerCard -> Choice -> Bool
syncWith card = \case
  UseAbility a -> abilityCost a == ExhaustUpgradeCost (UpgradeWithId $ upgradeIdOf card)
  _ -> False

paySync :: PlayerCard -> [PlayerCard] -> [PlayerCard] -> GameAppT ()
paySync event interfaces hand = do
  choose "play the event" (isPlay event)
  for_ interfaces $ \u -> choose "Sync Ratio" (syncWith u)
  payWith hand

interfaceExhausted :: PlayerCard -> GameAppT Bool
interfaceExhausted card = getsGame (maybe False (Upgrade.upgradeExhausted . toAttrs) . HashMap.lookup (upgradeIdOf card) . gameUpgrades)

spec :: Spec
spec = describe "SP//dr signature" do
  describe "Web-Trap" do
    it "deals five damage without a stun when paid from hand" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        event <- putInHand p "31006"
        resources <- traverse (putInHand p) ["31004", "31005"]
        playWith event resources
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5
        (villainStunned . toAttrs <$> getVillain) `shouldReturn` False
    it "stuns the attacked enemy when Sync Ratio contributed, without discarding the Interface" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        interface <- putIntoPlay p "31011"
        event <- putInHand p "31006"
        resource <- putInHand p "31004"
        paySync event [interface] [resource]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5
        (villainStunned . toAttrs <$> getVillain) `shouldReturn` True
        interfaceExhausted interface `shouldReturn` True
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` notElem "31011")
  describe "Rapid Deployment" do
    it "removes three threat without a Sync Ratio contribution" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        placeMainThreat 6
        event <- putInHand p "31005"
        resources <- traverse (putInHand p) ["31004", "31006"]
        playWith event resources
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` 3
    it "chooses a scheme again after the first three threat with Sync Ratio" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        placeMainThreat 6
        interface <- putIntoPlay p "31012"
        event <- putInHand p "31005"
        resource <- putInHand p "31004"
        paySync event [interface] [resource]
        chooseMainScheme
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` 0
  describe "All Systems Go!" do
    it "readies every Interface, including the Pilot, while leaving other exhausted upgrades alone" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        first <- putIntoPlay p "31011"
        second <- putIntoPlay p "31012"
        other <- putIntoPlay p "01008"
        pilot <- selectList (UpgradeIs Upgrades.spdrPilot)
        resolve $ [UpgradeMessage (upgradeIdOf c) ExhaustedUpgrade | c <- [first,second,other]] <> [UpgradeMessage u ExhaustedUpgrade | u <- pilot]
        resolve [IdentityMessage p ExhaustedIdentity]
        event <- putInHand p "31004"
        resource <- putInHand p "31005"
        playWith event [resource]
        choose "ready all Interfaces" (isLabel "$label.spdrReadyInterfaces")
        pass
        interfaceExhausted first `shouldReturn` False
        interfaceExhausted second `shouldReturn` False
        interfaceExhausted other `shouldReturn` True
        isExhaustedNow p `shouldReturn` True
        for_ pilot $ \u -> getsGame (maybe True (Upgrade.upgradeExhausted . toAttrs) . HashMap.lookup u . gameUpgrades) `shouldReturn` False
    it "searches the deck and discard pile for an Interface and adds it to hand" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        [host] <- stackDeck p ["31010"]
        event <- putInHand p "31004"
        resource <- putInHand p "31005"
        playWith event [resource]
        choose "search for an Interface" (isLabel "$label.spdrSearchInterface")
        choose "take Host Spider" (isTargetChoice host)
        pass
        map toCardCode <$> handOf p >>= (`shouldSatisfy` elem "31010")
  describe "Aunt May & Uncle Ben" do
    for_ [(False,2 :: Int),(True,3)] $ \(alter,n) ->
      it ("discards " <> show n <> " cards in " <> if alter then "alter-ego" else "hero") do
        tg <- testGame ["spdr"]
        let p = player1 tg
        inGame tg do
          unless alter $ asSpdr p
          clearHand p
          support <- putIntoPlay p "31007"
          _ <- stackDeck p ["31004", "31014", "31006"]
          choose "use Aunt May and Uncle Ben" (cardAbility support $ if alter then 2 else 1)
          choose "discard the required cards" (const True)
          pass
          codes <- map toCardCode <$> handOf p
          codes `shouldSatisfy` elem "31004"
          codes `shouldSatisfy` notElem "31014"
          ("31006" `elem` codes) `shouldBe` alter
  describe "Ejection Protocol" do
    it "sets the dial to six, makes the identity tough, exhausts Interfaces and flips to alter-ego" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        interface <- putIntoPlay p "31010"
        protocol <- putIntoPlay p "31008"
        choose "eject" (cardAbility protocol 1)
        pass
        damageOf p `shouldReturn` 8
        playerIdentityTough <$> playerAttrs p `shouldReturn` True
        identityMatches AlterEgoIdentity p `shouldReturn` True
        interfaceExhausted interface `shouldReturn` True
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "31008")
  describe "SP//dr Command" do
    it "exhausts itself and a selected Interface to draw a card" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        interface <- putIntoPlay p "31011"
        command <- putIntoPlay p "31009"
        choose "draw using SP//dr Command" (cardAbility command 1)
        choose "exhaust Psychic Link" (isTargetChoice $ upgradeIdOf interface)
        pass
        length <$> handOf p `shouldReturn` 1
        interfaceExhausted interface `shouldReturn` True
    it "discards a chosen hand card to ready an Interface" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        interface <- putIntoPlay p "31011"
        command <- putIntoPlay p "31009"
        card <- putInHand p "31005"
        resolve [UpgradeMessage (upgradeIdOf interface) ExhaustedUpgrade]
        choose "ready using SP//dr Command" (cardAbility command 2)
        choose "discard Rapid Deployment" (isTargetChoice card)
        choose "ready Psychic Link" (isTargetChoice $ upgradeIdOf interface)
        pass
        interfaceExhausted interface `shouldReturn` False
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "31005")
  describe "VEN#m" do
    it "does not count an Interface discarded from hand as a Sync Ratio resource" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        venom <- putInHand p "31003"
        resources <- traverse (putInHand p) ["31011","31004","31005","31006"]
        playWith venom resources
        pass
        attrs <- getsGame (toAttrs . fromMaybe (error "missing VEN#m") . HashMap.lookup (allyIdOf venom) . gameAllies)
        Ally.allyCounters attrs `shouldBe` 0
    it "optionally gains a sym counter per Sync Ratio resource and boosts both powers" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        first <- putIntoPlay p "31011"
        second <- putIntoPlay p "31012"
        venom <- putInHand p "31003"
        resources <- traverse (putInHand p) ["31004","31006"]
        paySync venom [first,second] resources
        choose "VEN#m gains sym counters" (cardAbility venom 1)
        pass
        attrs <- getsGame (toAttrs . fromMaybe (error "missing VEN#m") . HashMap.lookup (allyIdOf venom) . gameAllies)
        Ally.allyCounters attrs `shouldBe` 2
        Ally.getModifiedAttack attrs `shouldReturn` 3
        Ally.getModifiedThwart attrs `shouldReturn` 3
    it "counts all Sync Ratio resources generated toward her cost, including surplus" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        interfaces <- traverse (putIntoPlay p) ["31010","31011","31012","31013"]
        venom <- putInHand p "31003"
        choose "play VEN#m" (isPlay venom)
        for_ interfaces $ \u -> choose "generate an Interface resource" (syncWith u)
        Just pilot <- selectOne (UpgradeIs Upgrades.spdrPilot)
        choose "generate one surplus resource with the Pilot" (\case
          UseAbility a -> abilityCost a == ExhaustUpgradeCost (UpgradeWithId pilot)
          _ -> False)
        payWith []
        choose "place sym counters" (cardAbility venom 1)
        pass
        attrs <- getsGame (toAttrs . fromMaybe (error "missing VEN#m") . HashMap.lookup (allyIdOf venom) . gameAllies)
        Ally.allyCounters attrs `shouldBe` 5
    it "can decline the optional sym counters after paying with Sync Ratio" do
      tg <- testGame ["spdr"]
      let p = player1 tg
      inGame tg do
        asSpdr p
        interface <- putIntoPlay p "31011"
        venom <- putInHand p "31003"
        resources <- traverse (putInHand p) ["31004","31005","31006"]
        paySync venom [interface] resources
        pass
        attrs <- getsGame (toAttrs . fromMaybe (error "missing VEN#m") . HashMap.lookup (allyIdOf venom) . gameAllies)
        Ally.allyCounters attrs `shouldBe` 0
        interfaceExhausted interface `shouldReturn` True

  describe "Sync Ratio resources actually paid" do
    for_ [([Energy, Energy], False), ([Energy, Mental], True)] $ \(spent, bonus) ->
      it ("uses Web-Trap's bonus only when its chosen allocation spends Sync Ratio: " <> show bonus) do
        tg <- testGame ["spdr"]
        let p = player1 tg
        inGame tg do
          asSpdr p
          interface <- putIntoPlay p "31011"
          event <- putInHand p "31006"
          energy <- putInHand p "01088"
          choose "play Web-Trap" (isPlay event)
          choose "generate ordinary Energy" (isPayWith energy)
          choose "generate surplus Mental with Sync Ratio" (syncWith interface)
          choose "finish generating resources" isFinishPayment
          choose "choose resources actually paid" (isLabel $ allocationLabel spent)
          chooseVillain
          pass
          villainDamageNow `shouldReturn` 5
          (villainStunned . toAttrs <$> getVillain) `shouldReturn` bonus

    for_ [([Energy, Energy], False), ([Energy, Mental], True)] $ \(spent, bonus) ->
      it ("uses Rapid Deployment's second thwart only when Sync Ratio actually pays: " <> show bonus) do
        tg <- testGame ["spdr"]
        let p = player1 tg
        inGame tg do
          asSpdr p
          placeMainThreat 6
          interface <- putIntoPlay p "31011"
          event <- putInHand p "31005"
          energy <- putInHand p "01088"
          choose "play Rapid Deployment" (isPlay event)
          choose "generate ordinary Energy" (isPayWith energy)
          choose "generate surplus Mental with Sync Ratio" (syncWith interface)
          choose "finish generating resources" isFinishPayment
          choose "choose resources actually paid" (isLabel $ allocationLabel spent)
          chooseMainScheme
          when bonus chooseMainScheme
          pass
          mainSchemeThreatNow `shouldReturn` if bonus then 0 else 3

    for_ [0, 1] $ \spentSync ->
      it ("lets the payer choose which source supplied identical Mental resources: " <> show spentSync) do
        tg <- testGame ["spdr"]
        let p = player1 tg
        inGame tg do
          asSpdr p
          interface <- putIntoPlay p "31011"
          event <- putInHand p "31006"
          genius <- putInHand p "01089"
          choose "play Web-Trap" (isPlay event)
          choose "generate two ordinary Mental resources" (isPayWith genius)
          choose "generate one Mental with Sync Ratio" (syncWith interface)
          choose "finish generating resources" isFinishPayment
          choose "choose how many Sync Ratio resources pay" (isLabel $ syncRatioAllocationLabel spentSync)
          chooseVillain
          pass
          villainDamageNow `shouldReturn` 5
          (villainStunned . toAttrs <$> getVillain) `shouldReturn` (spentSync > 0)
