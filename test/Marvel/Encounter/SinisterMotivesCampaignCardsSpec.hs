module Marvel.Encounter.SinisterMotivesCampaignCardsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Environment.Cards qualified as Environments
import Marvel.Hero.GhostSpiderSpec ( asGhost, ghostSpider )
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Window qualified as W
import TestImport

schemeThreat :: SideSchemeId -> GameAppT Natural
schemeThreat sid = getsGame (maybe 0 (SideScheme.sideSchemeThreat . toAttrs) . HashMap.lookup sid . gameSideSchemes)

spec :: Spec
spec = describe "Sinister Motives campaign cards" do
  describe "Public Outcry" do
    for_ [(Environments.smPublicOutcryA, 2), (Environments.smPublicOutcryB, 3)] $ \(def, counters) ->
      it ("enters with its printed per-player notoriety: " <> show (toCardCode def)) do
        tg <- ghostSpider
        inGame tg do
          card <- revealNow (player1 tg) def
          getCountersOn (EnvironmentRef $ EnvironmentId $ unCardId $ ecCardId card) `shouldReturn` counters

    it "removes notoriety after a minion is defeated and enters the victory display when the last is removed" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        outcry <- revealNow p Environments.smPublicOutcryA
        let eid = EnvironmentId $ unCardId $ ecCardId outcry
        for_ [1 :: Int, 2] $ \_ -> do
          mid <- addMinion p Minions.hydraBomber
          resolve [IdentityMessage p ReadiedIdentity]
          choose "attack the minion" (isHeroAttack p)
          choose "Hydra Bomber" (isTargetChoice mid)
          choose "Public Outcry response" (isAbilityOf $ EnvironmentRef eid)
          pass
        getVictoryDisplay >>= (`shouldSatisfy` elem outcry)
        getVictoryPoints `shouldReturn` 1
        getsGame (HashMap.member eid . gameEnvironments) `shouldReturn` False

  describe "Smear Campaign" do
    it "adds notoriety then removes itself from the game when Public Outcry is present" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        public <- revealNow p Environments.smPublicOutcryA
        smear <- revealNow p Treacheries.smSmearCampaign
        pass
        getCountersOn (EnvironmentRef $ EnvironmentId $ unCardId $ ecCardId public) `shouldReturn` 4
        encounterDiscard >>= (`shouldSatisfy` notElem smear)
        mainSchemeThreatNow `shouldReturn` 0

    it "places two threat on the main scheme when Public Outcry is absent" do
      tg <- ghostSpider
      inGame tg do
        _ <- revealNow (player1 tg) Treacheries.smSmearCampaign
        pass
        mainSchemeThreatNow `shouldReturn` 2

  describe "Community Service" do
    it "makes Back Alley Burglary's thwarter discard a random card when they cannot pay physical" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealNow p Schemes.smBackAlleyBurglary
        let sid = SideSchemeId $ unCardId $ ecCardId card
        energy <- putInHand p "01088"
        choose "thwart" (isHeroThwart p)
        choose "Back Alley Burglary" (isTargetChoice sid)
        choose "resolve Back Alley Burglary" (isTargetChoice sid)
        choose "discard at random" (isLabelStarting "$label.discardRandomCardsFromHand")
        handOf p `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem energy)
        schemeThreat sid `shouldReturn` 1

    it "charges Cat in a Tree two actual indirect damage before a basic thwart removes threat" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealNow p Schemes.smCatInATree
        let sid = SideSchemeId $ unCardId $ ecCardId card
        choose "thwart" (isHeroThwart p)
        choose "Cat in a Tree" (isTargetChoice sid)
        damageOf p `shouldReturn` 2
        schemeThreat sid `shouldReturn` 2

    it "does not offer Cat in a Tree as a thwart target when tough prevents paying the damage cost" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealNow p Schemes.smCatInATree
        let sid = SideSchemeId $ unCardId $ ecCardId card
        placeMainThreat 2
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        choose "thwart" (isHeroThwart p)
        offers (isTargetChoice sid) `shouldReturn` False

    it "places the actual amount removed from Henchmen Heist on the main scheme" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealNow p Schemes.smHenchmenHeist
        let sid = SideSchemeId $ unCardId $ ecCardId card
        choose "thwart" (isHeroThwart p)
        choose "Henchmen Heist" (isTargetChoice sid)
        choose "Henchmen Heist forced response" (isTargetChoice sid)
        pass
        mainSchemeThreatNow `shouldReturn` 1
        schemeThreat sid `shouldReturn` 2

    it "lets a basic thwarter use ATK instead of THW against Rubble Rescue" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealNow p Schemes.smRubbleRescue
        let sid = SideSchemeId $ unCardId $ ecCardId card
        choose "thwart" (isHeroThwart p)
        choose "Rubble Rescue" (isTargetChoice sid)
        choose "use ATK" (isLabel "$label.thwartWithAtk")
        schemeThreat sid `shouldReturn` 3

    it "removes Off the Rails after its second villain-phase speed counter and discards three cards from each deck" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        card <- revealNow p Schemes.smOffTheRails
        let sid = SideSchemeId $ unCardId $ ecCardId card
        before <- length . unDeck . playerIdentityDeck . toAttrs <$> getPlayer p
        for_ [1 :: Int, 2] $ \_ -> do
          runNow [CheckWindows [W.Window W.When $ W.PhaseBegan VillainPhase]]
          choose "Off the Rails" (isAbilityOf $ SideSchemeRef sid)
          pass
        getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` False
        length . unDeck . playerIdentityDeck . toAttrs <$> getPlayer p `shouldReturn` (before - 3)
        getVictoryDisplay >>= (`shouldSatisfy` notElem card)

  describe "Venom (Eddie Brock)" do
    it "pays a damage to deal the number of boost-area icons, including stars, after revealing a card" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        venom <- putIntoPlay p "27190"
        _ <- revealNow p Minions.badoonLieutenant
        choose "Venom response" (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf venom) && abilityIndex ab == 1; _ -> False)
        chooseVillain
        fmap (map allyDamage) (alliesWith "27190") `shouldReturn` [1]
        villainDamageNow `shouldReturn` 2

  describe "Snitches Get Stitches" do
    it "redirects a villain attack to Venom and puts both cards in the victory display if it defeats him" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        venom <- putIntoPlay p "27190"
        resolve [AllyMessage (allyIdOf venom) $ AllyDamaged GameRef $ toDamage 5 FromAbility]
        snitches <- revealNow p Attachments.smSnitchesGetStitches
        pass
        _ <- stackEncounterDeck [Treacheries.imTough]
        vid <- villainIdNow
        runNow [VillainMessage vid $ VillainAttacks p]
        choose "Snitches redirects the attack" (\case UseAbility ab -> abilitySource ab == toSource (AttachmentRef $ AttachmentId $ unCardId $ ecCardId snitches) && abilityIndex ab == 1; _ -> False)
        pass
        choose "Snitches moves the defeated Venom to victory" (\case UseAbility ab -> abilitySource ab == toSource (AttachmentRef $ AttachmentId $ unCardId $ ecCardId snitches) && abilityIndex ab == 2; _ -> False)
        pass
        damageOf p `shouldReturn` 0
        alliesWith "27190" `shouldReturn` []
        victory <- getVictoryDisplay
        victory `shouldSatisfy` any ((== "27190") . toCardCode)
        victory `shouldSatisfy` elem snitches
        getVictoryPoints `shouldReturn` (-1)
