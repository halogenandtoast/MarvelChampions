module Marvel.Encounter.CaptainAmericaNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet (EncounterSet (CaptainAmericaNemesis))
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

-- | Whether the minion is engaged with the player.
engagedWith :: IdentityId -> MinionId -> GameAppT Bool
engagedWith p m = getsGame (maybe False ((== p) . minionEngagedIdentity . toAttrs) . HashMap.lookup m . gameMinions)

spec :: Spec
spec = describe "Captain America's nemesis set" do
  it "is Hit Squad, Baron Zemo, two Hydra Soldiers and Hail Hydra!" do
    cards <- gatherEncounterSet CaptainAmericaNemesis :: IO [EncounterCard]
    sort (map (toCardCode @EncounterCard) cards) `shouldBe` ["03027", "03028", "03029", "03029", "03030" :: CardCode]

  describe "Hit Squad" do
    it "has each player discard the top encounter card and take 1 damage for each boost icon on it" do
      tg <- testGame ["captain-america", "spider-man"]
      inGame tg do
        [p1, p2] <- players
        -- Vulture has 2 boost icons, Hail Hydra! none
        _ <- stackEncounterDeck [Minions.vulture, Treacheries.hailHydra]
        squad <- revealNow p1 SideSchemes.hitSquad
        pass
        damageOf p1 `shouldReturn` 2
        damageOf p2 `shouldReturn` 0
        take 2 <$> encounterDiscardCodes `shouldReturn` ["03030", "01167"]
        -- 3 threat per player
        sideSchemeThreat (sideSchemeOf squad) `shouldReturn` 6

  describe "Baron Zemo" do
    it "attacks a hero as he engages (quickstrike)" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p (ChangedToForm A)]
        _ <- stackEncounterDeck [Treacheries.hailHydra]
        _ <- revealNow p Minions.baronZemo
        pass
        damageOf p `shouldReturn` 3

    it "keeps the player engaged with him from thwarting" do
      tg <- testGame ["captain-america"]
      inGame tg do
        [p1] <- players
        resolve [IdentityMessage p1 (ChangedToForm A)]
        placeMainThreat 3
        offers (isHeroThwart p1) `shouldReturn` True
        _ <- stackEncounterDeck [Treacheries.hailHydra]
        _ <- revealNow p1 Minions.baronZemo
        pass
        offers (isHeroThwart p1) `shouldReturn` False
        -- a thwart card is not played either
        clearHand p1
        justice <- putInHand p1 "01060"
        _ <- putInHand p1 "01088"
        offers (isPlay justice) `shouldReturn` False
        -- "you" is the identity: their allies still thwart (rules reference, "You, Your")
        _ <- putIntoPlay p1 "03013"
        pass
        squirrel <- allyIn "03013"
        offers (isAllyThwart squirrel) `shouldReturn` True
        -- removing threat is not thwarting
        before <- mainSchemeThreat
        agent13 <- putInHand p1 "03002"
        runNow [PutCardIntoPlay p1 agent13 NoPayment Nothing]
        choose "use Agent 13" (isAbilityOfCard agent13)
        mainSchemeThreat `shouldReturn` (before - 2)

  describe "Hydra Soldier (03029)" do
    it "is the core set's Hydra Soldier: guard, and deals an encounter card when defeated" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        soldier <- addMinion p Minions.hydraSoldierCaptainAmerica
        vid <- villainIdNow
        resolve [IdentityMessage p (ChangedToForm A)]
        choose "attack" (isHeroAttack p)
        offers (isTargetChoice vid) `shouldReturn` False
        choose "attack the soldier" (isTargetChoice soldier)
        pass
        before <- length . (.playerIdentityEncounterCards) <$> playerAttrs p
        resolve [MinionMessage soldier (MinionDamaged GameRef (toDamage 4 FromAbility))]
        minionDamageOf soldier `shouldReturn` Nothing
        after <- length . (.playerIdentityEncounterCards) <$> playerAttrs p
        after `shouldBe` before + 1

  describe "Hail Hydra!" do
    it "has each Hydra minion engaged with a hero attack them, and each player not attacked put one into play" do
      tg <- testGame ["captain-america", "spider-man"]
      inGame tg do
        [p1, p2] <- players
        resolve [IdentityMessage p1 (ChangedToForm A)]
        soldier <- addMinion p1 Minions.hydraSoldierCaptainAmerica
        pass
        -- the soldier's attack draws a boost card with no icons
        [_, bomber] <- stackEncounterDeck [Treacheries.hailHydra, Minions.hydraBomber]
        before <- getsGame (HashMap.keys . gameMinions)
        _ <- revealNow p1 Treacheries.hailHydra
        pass
        choose "the bomber" (isTargetChoice bomber)
        pass
        damageOf p1 `shouldReturn` 2
        new <- getsGame (filter (`notElem` before) . HashMap.keys . gameMinions)
        length new `shouldBe` 1
        for_ new \m -> engagedWith p2 m `shouldReturn` True
        engagedWith p1 soldier `shouldReturn` True

    it "a player whose Hydra minion's attack is cancelled was not attacked, and puts another into play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        soldier <- addMinion p Minions.hydraSoldierCaptainAmerica
        webbed <- putInHand p "01009"
        payment <- traverse (putInHand p) ["01088", "01090"]
        playOn webbed (EnemyMinionId soldier) payment
        [bomber] <- stackEncounterDeck [Minions.hydraBomber]
        before <- getsGame (HashMap.keys . gameMinions)
        _ <- revealNow p Treacheries.hailHydra
        choose "resolve Webbed Up" (isAbilityOfCard webbed)
        pass
        choose "the bomber" (isTargetChoice bomber)
        pass
        damageOf p `shouldReturn` 0
        new <- getsGame (filter (`notElem` before) . HashMap.keys . gameMinions)
        length new `shouldBe` 1

    it "a Hydra minion engaged with an alter-ego does not attack, and that player puts another into play" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        soldier <- addMinion p Minions.hydraSoldierCaptainAmerica
        [bomber] <- stackEncounterDeck [Minions.hydraBomber]
        before <- getsGame (HashMap.keys . gameMinions)
        _ <- revealNow p Treacheries.hailHydra
        choose "the bomber" (isTargetChoice bomber)
        pass
        damageOf p `shouldReturn` 0
        new <- getsGame (filter (`notElem` before) . HashMap.keys . gameMinions)
        length new `shouldBe` 1
        engagedWith p soldier `shouldReturn` True

    it "a stunned Hydra minion does not attack, so its player was not attacked and searches" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p (ChangedToForm A)]
        soldier <- addMinion p Minions.hydraSoldierCaptainAmerica
        pass
        resolve [GiveStatus (MinionRef soldier) StunnedStatus]
        _ <- setEncounterDeck [Minions.hydraBomber] [Treacheries.advance]
        before <- getsGame (HashMap.keys . gameMinions)
        _ <- revealNow p Treacheries.hailHydra
        pass
        damageOf p `shouldReturn` 0
        new <- getsGame (filter (`notElem` before) . HashMap.keys . gameMinions)
        length new `shouldBe` 1

    it "the searching player chooses the Hydra minion" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        ([_, mercenary], _) <- setEncounterDeck [Minions.hydraBomber, Minions.hydraMercenary] [Treacheries.advance]
        _ <- revealNow p Treacheries.hailHydra
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        getsGame (HashMap.member (minionOf mercenary) . gameMinions) `shouldReturn` True
        length <$> getsGame (HashMap.keys . gameMinions) `shouldReturn` 1
