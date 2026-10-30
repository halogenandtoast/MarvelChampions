module Marvel.Encounter.SinisterTechSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hero.GhostSpiderSpec (ghostSpider, asGhost, attackVillain)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity, getModifiedHp, getModifiedKeywords)
import Marvel.Keyword (Keyword (Steady, Stalwart))
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Phase (Phase (PlayerPhase, VillainPhase))
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W
import TestImport

stats :: IdentityId -> GameAppT (Natural, Natural, Natural)
stats p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> (,,) <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
  _ -> pure (0, 0, 0)

techAbility :: PlayerCard -> Natural -> Choice -> Bool
techAbility card index = \case
  UseAbility ab -> abilitySource ab == UpgradeRef (upgradeIdOf card) && abilityIndex ab == index
  _ -> False

spec :: Spec
spec = describe "Sinister Motives S.H.I.E.L.D. Tech" do
  describe "enhancement" do
    it "flips to the enhanced definition preserving the physical card, controller, owner and counters" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        tech <- putIntoPlay p "27182a"
        resolve [UpgradeMessage (upgradeIdOf tech) $ AddUpgradeUses 3]
        before <- head <$> upgradesWith "27182a"
        resolve [CardSpecific (UpgradeRef $ upgradeIdOf tech) "enhance" $ toJSON ()]
        [after] <- upgradesWith "27182b"
        upgradeId after `shouldBe` upgradeId before
        upgradeOwner after `shouldBe` upgradeOwner before
        upgradeController after `shouldBe` upgradeController before
        upgradeUses after `shouldBe` 3

  describe "Compact Darts" do
    it "loads three counters once per round in alter ego, then spends one after a hero attack to deal a damage" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        darts <- putIntoPlay p "27182a"
        resource <- putInHand p "27024"
        choose "reload darts" (techAbility darts 2)
        payWith [resource]
        fmap (map upgradeUses) (upgradesWith "27182a") `shouldReturn` [3]
        _ <- putInHand p "27024"
        offers (techAbility darts 2) `shouldReturn` False
        changeForm p
        attackVillain p
        choose "fire a dart" (techAbility darts 1)
        chooseVillain
        villainDamageNow `shouldReturn` 3
        fmap (map upgradeUses) (upgradesWith "27182a") `shouldReturn` [2]

    it "the enhanced dart can damage two different enemies for one counter" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        darts <- putIntoPlay p "27182b"
        resolve [UpgradeMessage (upgradeIdOf darts) $ AddUpgradeUses 3]
        mid <- addMinion p Minions.hydraMercenary
        choose "attack the minion" (isHeroAttack p)
        choose "Hydra Mercenary" (isTargetChoice mid)
        choose "fire enhanced darts" (techAbility darts 1)
        choose "finish the minion" (isTargetChoice mid)
        chooseVillain
        minionDamageOf mid `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 1
        fmap (map upgradeUses) (upgradesWith "27182b") `shouldReturn` [2]

  describe "Impact-Dampening Suit" do
    for_ [("27183a", 12), ("27183b", 13)] $ \(code, expected) ->
      it ("increases the identity's hit points: " <> show code) do
        tg <- ghostSpider
        inGame tg do
          let p = player1 tg
          _ <- putIntoPlay p code
          (getPlayer p >>= getModifiedHp) `shouldReturn` expected

    it "the basic suit can be paid for at villain-phase start to reduce each enemy attack that phase" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        suit <- putIntoPlay p "27183a"
        resource <- putInHand p "27024"
        _ <- stackEncounterDeck [Treacheries.imTough]
        runNow [BeginPhase VillainPhase]
        choose "activate the suit" (techAbility suit 1)
        payWith [resource]
        choose "take the attack" isNoDefender
        damageOf p `shouldReturn` 1

    it "the enhanced suit discards a deck card as a cost to prevent one attack damage" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        suit <- putIntoPlay p "27183b"
        [top] <- stackDeck p ["01088"]
        runNow $ damageIdentity p GameRef (toDamage 3 FromAttack)
        choose "use enhanced armor" (techAbility suit 1)
        pass
        damageOf p `shouldReturn` 2
        discardOf p >>= (`shouldSatisfy` elem top)
        runNow $ damageIdentity p GameRef (toDamage 2 FromAbility)
        offers (techAbility suit 1) `shouldReturn` False

  describe "Laser Goggles" do
    it "reduces THW, adds ATK and gives basic attacks overkill" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- putIntoPlay p "27184a"
        stats p `shouldReturn` (0, 3, 3)
        mid <- addMinion p Minions.hydraBomber
        choose "attack" (isHeroAttack p)
        choose "Hydra Bomber" (isTargetChoice mid)
        villainDamageNow `shouldReturn` 1

    it "enhanced goggles also give basic attacks piercing" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- putIntoPlay p "27184b"
        stats p `shouldReturn` (0, 4, 3)
        mid <- addMinion p Minions.armoredGuard
        choose "attack" (isHeroAttack p)
        choose "Armored Guard" (isTargetChoice mid)
        minionDamageOf mid `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 1

  describe "Propulsion Gauntlet" do
    for_ ["27185a", "27185b"] $ \code ->
      it ("takes two actual indirect damage as a cost and readies the hero: " <> show code) do
        tg <- ghostSpider
        let p = player1 tg
        inGame tg do
          asGhost p
          tech <- putIntoPlay p code
          resolve [IdentityMessage p ExhaustedIdentity]
          choose "use propulsion" (techAbility tech 1)
          damageOf p `shouldReturn` 2
          exhaustedNow p `shouldReturn` False
          upgradeIsExhausted (upgradeIdOf tech) `shouldReturn` True
          stats p `shouldReturn` (if code == "27185b" then (2, 3, 4) else (1, 2, 3))

    it "cannot pay indirect damage while all controlled characters are tough" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        tech <- putIntoPlay p "27185a"
        resolve [IdentityMessage p ExhaustedIdentity, GiveStatus (IdentityRef p) ToughStatus]
        offers (techAbility tech 1) `shouldReturn` False

    it "the enhanced propulsion bonus ends with the phase" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        tech <- putIntoPlay p "27185b"
        resolve [IdentityMessage p ExhaustedIdentity]
        choose "use propulsion" (techAbility tech 1)
        stats p `shouldReturn` (2, 3, 4)
        runNow [EndPhase PlayerPhase]
        stats p `shouldReturn` (1, 2, 3)

  describe "Retinal Display" do
    it "lets only a basic thwart ignore crisis, and restricts it to the most threatened scheme" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- putIntoPlay p "27186a"
        crisis <- revealNow p Schemes.crowdControl
        placeMainThreat 4
        event <- putInHand p "27004"
        genius <- putInHand p "01089"
        choose "basic thwart" (isHeroThwart p)
        let sid = SideSchemeId $ unCardId $ ecCardId crisis
        offers (isTargetChoice sid) `shouldReturn` False
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` 2
        playWith event [genius]
        mid <- mainSchemeId
        offers (isTargetChoice mid) `shouldReturn` False
        choose "Phantom Flip thwarts the crisis instead" (isTargetChoice sid)

    it "the enhanced display lets basic thwarts ignore patrol as well" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- putIntoPlay p "27186b"
        _ <- addMinion p Minions.badoonLieutenant
        placeMainThreat 4
        choose "basic thwart through patrol" (isHeroThwart p)
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` 1

  describe "Shock Knuckles" do
    for_ [("27187a", Treacheries.imTough, True), ("27187a", Minions.hydraMercenary, False), ("27187b", Minions.hydraMercenary, True), ("27187b", Minions.shocker, False)] $ \(code, boost, stunned) ->
      it ("uses its printed boost threshold: " <> show (code, toCardCode boost)) do
        tg <- ghostSpider
        let p = player1 tg
        inGame tg do
          asGhost p
          tech <- putIntoPlay p code
          _ <- setEncounterDeck [boost, Treacheries.advance] []
          attackVillain p
          choose "Shock Knuckles" (techAbility tech 1)
          pass
          Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` stunned

  describe "Wave Bracers" do
    for_ [("27188a", (1, 1, 4), Steady), ("27188b", (1, 1, 5), Stalwart)] $ \(code, expected, keyword) ->
      it ("changes the hero's stats and grants its printed status protection: " <> show code) do
        tg <- ghostSpider
        let p = player1 tg
        inGame tg do
          asGhost p
          _ <- putIntoPlay p code
          stats p `shouldReturn` expected
          getPlayer p >>= getModifiedKeywords >>= (`shouldSatisfy` elem keyword)

    it "retaliates after an enemy attack in hero form" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- putIntoPlay p "27188a"
        _ <- stackEncounterDeck [Treacheries.imTough]
        vid <- villainIdNow
        runNow [VillainMessage vid $ VillainAttacks p]
        choose "take the attack" isNoDefender
        pass
        villainDamageNow `shouldReturn` 1

  describe "Wrist Navigator" do
    for_ ["27189a", "27189b"] $ \code ->
      it ("attaches to an entering side scheme, rewards its defeat and returns to the play area: " <> show code) do
        tg <- testGame ["captain-marvel"]
        let p = player1 tg
        inGame tg do
          asGhost p
          wrist <- putIntoPlay p code
          card <- revealNow p Schemes.crowdControl
          let sid = SideSchemeId $ unCardId $ ecCardId card
          choose "attach Wrist Navigator" (techAbility wrist 1)
          before <- length <$> handOf p
          choose "thwart" (isHeroThwart p)
          choose "Crowd Control" (isTargetChoice sid)
          choose "draw from Wrist Navigator" (techAbility wrist 2)
          when (code == "27189b") do
            cards <- handOf p
            choose "discard after drawing two" (isTargetChoice $ head cards)
          pass
          length <$> handOf p `shouldReturn` (before + 1)
          [attrs] <- upgradesWith code
          upgradeAttachedSideScheme attrs `shouldBe` Nothing
          upgradeAttachedEnemy attrs `shouldBe` Nothing
          upgradeId attrs `shouldBe` upgradeIdOf wrist
