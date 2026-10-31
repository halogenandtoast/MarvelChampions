module Marvel.Aspect.GambitPackSpec (spec) where

import Control.Monad (forM_)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Villain.Types (villainConfused)
import TestImport

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == n
  _ -> False

asHero :: TestGame -> GameAppT ()
asHero tg = changeForm (player1 tg) >> clearHand (player1 tg)

deckNow :: IdentityId -> GameAppT [PlayerCard]
deckNow p = unDeck . playerIdentityDeck <$> playerAttrs p

usesNow :: PlayerCard -> GameAppT Natural
usesNow card = getsGame $ maybe 0 (upgradeUses . toAttrs) . HashMap.lookup (upgradeIdOf card) . gameUpgrades

spec :: Spec
spec = describe "Gambit pack" do
  describe "Bishop" do
    it "gains one energy counter after a minion attacks you" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        card <- putIntoPlay p "37011"
        minion <- addMinion p Minions.hydraMercenary
        runNow [MinionMessage minion $ MinionAttacks p]
        choose "take the attack" isNoDefender
        choose "Bishop's energy response" (cardAbility card 1)
        pass
        getCountersOn (allyIdOf card) `shouldReturn` 1

    it "also gains a counter when your ally defended the attack" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        card <- putIntoPlay p "37011"
        defender <- putIntoPlay p "01002"
        minion <- addMinion p Minions.hydraMercenary
        runNow [MinionMessage minion $ MinionAttacks p]
        choose "defend with Black Cat" (\case AllyDefend aid _ -> aid == allyIdOf defender; _ -> False)
        choose "Bishop responds to the attack on his player" (cardAbility card 1)
        pass
        getCountersOn (allyIdOf card) `shouldReturn` 1
        damageOf p `shouldReturn` 0

    it "does not answer an attack against another player" do
      tg <- testGame ["gambit", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        asHero tg
        card <- putIntoPlay p "37011"
        minion <- addMinion q Minions.hydraMercenary
        runNow [MinionMessage minion $ MinionAttacks q]
        choose "the other player takes the attack" isNoDefender
        offers (cardAbility card 1) `shouldReturn` False
        pass
        getCountersOn (allyIdOf card) `shouldReturn` 0

    it "removes every counter, caps the bonus at six, and boosts only that attack" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        card <- putIntoPlay p "37011"
        let aid = allyIdOf card
        resolve [PlaceCounters (AllyRef aid) 5]
        bishop <- allyIn "37011"
        choose "Bishop attacks" (isAllyAttack bishop)
        chooseVillain
        choose "spend all five counters" (cardAbility card 2)
        pass
        villainDamageNow `shouldReturn` 6
        getCountersOn aid `shouldReturn` 0
        allyDamage <$> allyIn "37011" `shouldReturn` 2
        resolve [AllyMessage aid $ AllyHealed 2, AllyMessage aid ReadiedAlly]
        bishop2 <- allyIn "37011"
        choose "attack again" (isAllyAttack bishop2)
        chooseVillain
        offers (cardAbility card 2) `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 6

    it "gets two ATK per energy counter below the cap" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        card <- putIntoPlay p "37011"
        resolve [PlaceCounters (AllyRef $ allyIdOf card) 2]
        bishop <- allyIn "37011"
        choose "Bishop attacks" (isAllyAttack bishop)
        chooseVillain
        choose "spend both counters" (cardAbility card 2)
        pass
        villainDamageNow `shouldReturn` 4

  describe "Dazzler" do
    it "confuses the villain after being played" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        card <- putInHand p "37012"
        energy <- putInHand p "01088"
        strength <- putInHand p "01090"
        playWith card [energy, strength]
        choose "Dazzler enters play" (cardAbility card 1)
        chooseVillain
        villainConfused . toAttrs <$> getVillain `shouldReturn` True

    it "can choose a minion and leaves other enemies unchanged" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        minion <- addMinion p Minions.hydraMercenary
        card <- putInHand p "37012"
        energy <- putInHand p "01088"
        strength <- putInHand p "01090"
        playWith card [energy, strength]
        choose "Dazzler enters play" (cardAbility card 1)
        choose "confuse the minion" (isTargetChoice minion)
        getsGame (maybe False (minionConfused . toAttrs) . HashMap.lookup minion . gameMinions) `shouldReturn` True
        villainConfused . toAttrs <$> getVillain `shouldReturn` False

    it "does not trigger her entry response when she later attacks" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        card <- putIntoPlay p "37012"
        dazzler <- allyIn "37012"
        choose "Dazzler attacks" (isAllyAttack dazzler)
        chooseVillain
        offers (cardAbility card 1) `shouldReturn` False
        villainDamageNow `shouldReturn` 2
        allyDamage <$> allyIn "37012" `shouldReturn` 1

    it "may decline the entry response" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        card <- putInHand p "37012"
        energy <- putInHand p "01088"
        strength <- putInHand p "01090"
        playWith card [energy, strength]
        pass
        villainConfused . toAttrs <$> getVillain `shouldReturn` False

  describe "Operative Skill" do
    it "adds one to each basic thwart and discards after its third use" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        card <- putIntoPlay p "37013"
        usesNow card `shouldReturn` 3
        placeMainThreat 6
        before <- mainSchemeThreatNow
        forM_ [1 .. 3 :: Natural] $ \n -> do
          when (n > 1) $ resolve [IdentityMessage p ReadiedIdentity]
          choose "basic thwart" (isHeroThwart p)
          choose "use Operative Skill" (cardAbility card 1)
          pass
          chooseMainScheme
          mainSchemeThreatNow `shouldReturn` (before - 2 * n)
        discardOf p >>= (`shouldSatisfy` elem card)
        usesNow card `shouldReturn` 0

    it "still increases an event when its final counter discards the upgrade" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        skill <- putIntoPlay p "37013"
        resolve [RemoveCounters (UpgradeRef $ upgradeIdOf skill) 2]
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand p "37015"
        energy <- putInHand p "01088"
        playWith card [energy]
        chooseMainScheme
        choose "spend the last operative counter" (cardAbility skill 1)
        pass
        mainSchemeThreatNow `shouldReturn` (before - 4)
        discardOf p >>= (`shouldSatisfy` elem skill)

    it "permits only one copy per player" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "37013"
        second <- putInHand p "37013"
        _ <- putInHand p "01088"
        offers (isPlay second) `shouldReturn` False

    it "increases an alter-ego thwart and stacks with the reprinted Passion for Justice" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        skill <- putIntoPlay p "37013"
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand p "37015"
        passion <- putInHand p "37016"
        energy <- putInHand p "37021"
        playWith card [passion, energy]
        choose "Passion for Justice" (isLabel "$label.passionForJustice")
        chooseMainScheme
        choose "Operative Skill on the alter-ego thwart" (cardAbility skill 1)
        pass
        mainSchemeThreatNow `shouldReturn` (before - 5)
        usesNow skill `shouldReturn` 2

    it "increases Remy's thwart but expires before his next use" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        skill <- putIntoPlay p "37013"
        placeMainThreat 6
        before <- mainSchemeThreatNow
        (cards, _) <- setEncounterDeck [Minions.hydraMercenary, Minions.hydraBomber, Minions.shocker] []
        choose "Thief Extraordinaire" (isNamedAbility "$label.thiefExtraordinaire")
        choose "discard the one-boost mercenary" (isTargetChoice $ CardIdRef $ toCardId $ cards !! 0)
        chooseMainScheme
        choose "increase Remy's thwart" (cardAbility skill 1)
        pass
        mainSchemeThreatNow `shouldReturn` (before - 2)
        resolve [IdentityMessage p ReadiedIdentity]
        choose "Thief Extraordinaire again" (isNamedAbility "$label.thiefExtraordinaire")
        choose "discard the one-boost bomber" (isTargetChoice $ CardIdRef $ toCardId $ cards !! 1)
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` (before - 3)
        usesNow skill `shouldReturn` 2

    it "spends one counter for a whole multi-instance thwart and increases both removals" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        skill <- putIntoPlay p "37013"
        side <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        resolve [SideSchemeMessage side $ SideSchemePlaceThreat 5]
        sideBefore <- sideSchemeThreat side
        placeMainThreat 6
        mainBefore <- mainSchemeThreatNow
        card <- putInHand p "15013"
        mental <- putInHand p "01089"
        playWith card [mental]
        chooseMainScheme
        choose "increase the entire thwart" (cardAbility skill 1)
        pass
        mainSchemeThreatNow `shouldReturn` (mainBefore - 3)
        choose "remove threat from the different scheme" (isTargetChoice $ SchemeSideSchemeId side)
        sideSchemeThreat side `shouldReturn` (sideBefore - 3)
        usesNow skill `shouldReturn` 2

    it "adds once to Mutant Peacekeepers' total before distribution" do
      tg <- testGame ["phoenix"]
      let p = player1 tg
      inGame tg do
        asHero tg
        skill <- putIntoPlay p "37013"
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand p "34018"
        resource <- putInHand p "34011"
        playWith card [resource]
        choose "increase the total" (cardAbility skill 1)
        pass
        mainSchemeThreatNow `shouldReturn` (before - 4)
        usesNow skill `shouldReturn` 2

    it "does not answer an ally's thwart" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        skill <- putIntoPlay p "37013"
        _ <- putIntoPlay p "37011"
        placeMainThreat 6
        before <- mainSchemeThreatNow
        bishop <- allyIn "37011"
        choose "Bishop thwarts" (isAllyThwart bishop)
        chooseMainScheme
        offers (cardAbility skill 1) `shouldReturn` False
        usesNow skill `shouldReturn` 3
        mainSchemeThreatNow `shouldReturn` (before - 2)

  describe "Breaking and Entering" do
    forM_ [False, True] $ \heroForm ->
      it ("removes three threat in " <> if heroForm then "hero form" else "alter-ego form") do
        tg <- testGame ["gambit"]
        let p = player1 tg
        inGame tg do
          when heroForm $ changeForm p
          clearHand p
          placeMainThreat 6
          before <- mainSchemeThreatNow
          card <- putInHand p "37015"
          energy <- putInHand p "01088"
          playWith card [energy]
          chooseMainScheme
          mainSchemeThreatNow `shouldReturn` (before - 3)

    it "also allows an identity with the Spy trait" do
      tg <- testGame ["black-widow"]
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand p "37015"
        energy <- putInHand p "01088"
        playWith card [energy]
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (before - 3)

    it "is unavailable without the Spy or Thief trait" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero tg
        card <- putInHand p "37015"
        _ <- putInHand p "01088"
        placeMainThreat 4
        offers (isPlay card) `shouldReturn` False

    it "cannot initiate through patrol unless it will clear confusion" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- addMinion p Minions.headhuntersHenchman
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand p "37015"
        energy <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False
        resolve [IdentityMessage p IdentityConfused]
        offers (isPlay card) `shouldReturn` True
        playWith card [energy]
        playerIdentityConfused <$> playerAttrs p `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` before

    it "clears confusion instead of removing threat or triggering Operative Skill" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        skill <- putIntoPlay p "37013"
        resolve [IdentityMessage p IdentityConfused]
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand p "37015"
        energy <- putInHand p "01088"
        playWith card [energy]
        playerIdentityConfused <$> playerAttrs p `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` before
        usesNow skill `shouldReturn` 3

  describe "Beauty and the Thief" do
    it "deals four damage then removes four threat with both characters in play" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        _ <- putIntoPlay p "37002"
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand p "37019"
        energy <- putInHand p "01088"
        playWith card [energy]
        pass
        chooseVillain
        chooseMainScheme
        villainDamageNow `shouldReturn` 4
        mainSchemeThreatNow `shouldReturn` (before - 4)

    it "completes its attack without asking an illegal thwart target through patrol" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        _ <- putIntoPlay p "37002"
        _ <- addMinion p Minions.headhuntersHenchman
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand p "37019"
        energy <- putInHand p "01088"
        playWith card [energy]
        pass
        chooseVillain
        villainDamageNow `shouldReturn` 4
        mainSchemeThreatNow `shouldReturn` before
        main <- mainSchemeIdNow
        offers (isTargetChoice main) `shouldReturn` False

    it "requires Rogue and hero form" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        card <- putInHand p "37019"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False
        _ <- putIntoPlay p "37002"
        offers (isPlay card) `shouldReturn` True
      alterEgoGame <- testGame ["gambit"]
      inGame alterEgoGame do
        let q = player1 alterEgoGame
        clearHand q
        _ <- putIntoPlay q "37002"
        card <- putInHand q "37019"
        _ <- putInHand q "01088"
        offers (isPlay card) `shouldReturn` False

  describe "Mutant Education" do
    it "shuffles up to two identity-specific cards and excludes aspect and basic cards" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        first <- putInDiscard p "37006"
        second <- putInDiscard p "37009"
        aspect <- putInDiscard p "37015"
        basic <- putInDiscard p "01088"
        card <- putInHand p "37021"
        playWith card []
        offers (isTargetChoice aspect) `shouldReturn` False
        offers (isTargetChoice basic) `shouldReturn` False
        choose "first identity card" (isTargetChoice first)
        choose "second identity card" (isTargetChoice second)
        discards <- discardOf p
        discards `shouldSatisfy` (\cs -> first `notElem` cs && second `notElem` cs && aspect `elem` cs && basic `elem` cs)
        deckNow p >>= (`shouldSatisfy` \cs -> first `elem` cs && second `elem` cs)
        length <$> handOf p `shouldReturn` 0

    it "allows stopping after one selected card" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        first <- putInDiscard p "37006"
        second <- putInDiscard p "37009"
        card <- putInHand p "37021"
        playWith card []
        choose "one identity card" (isTargetChoice first)
        choose "stop at one" (isLabel "$label.done")
        discardOf p >>= (`shouldSatisfy` elem second)
        deckNow p >>= (`shouldSatisfy` elem first)

    it "may choose zero cards without X-Mansion" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        signature <- putInDiscard p "37006"
        card <- putInHand p "37021"
        playWith card []
        choose "choose zero" (isLabel "$label.done")
        discardOf p >>= (`shouldSatisfy` elem signature)
        length <$> handOf p `shouldReturn` 0

    it "draws with X-Mansion even when choosing zero cards" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "37018"
        signature <- putInDiscard p "37006"
        top <- head <$> stackDeck p ["37007"]
        card <- putInHand p "37021"
        playWith card []
        choose "choose no cards" (isLabel "$label.done")
        handOf p `shouldReturn` [top]
        discardOf p >>= (`shouldSatisfy` elem signature)

    it "can draw with X-Mansion without any eligible discard" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs {playerIdentityDiscard = Discard []}
        _ <- putIntoPlay p "37018"
        top <- head <$> stackDeck p ["37007"]
        card <- putInHand p "37021"
        playWith card []
        handOf p `shouldReturn` [top]

    it "requires Mutant, alter-ego form and a possible effect" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInDiscard p "01005"
        card <- putInHand p "37021"
        offers (isPlay card) `shouldReturn` False
      other <- testGame ["gambit"]
      inGame other do
        let q = player1 other
        clearHand q
        withGame_ $ entitiesL . playersL . ix q %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs {playerIdentityDiscard = Discard []}
        card <- putInHand q "37021"
        offers (isPlay card) `shouldReturn` False
        _ <- putInDiscard q "37006"
        changeForm q
        offers (isPlay card) `shouldReturn` False

  describe "X-Men Instruction" do
    it "shuffles two X-Men allies, including identity and aspect allies, but no other cards" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        rogue <- putInDiscard p "37002"
        bishop <- putInDiscard p "37011"
        nonXmen <- putInDiscard p "01083"
        signatureEvent <- putInDiscard p "37006"
        card <- putInHand p "37031"
        playWith card []
        offers (isTargetChoice nonXmen) `shouldReturn` False
        offers (isTargetChoice signatureEvent) `shouldReturn` False
        choose "Rogue" (isTargetChoice rogue)
        choose "Bishop" (isTargetChoice bishop)
        deckNow p >>= (`shouldSatisfy` \cs -> rogue `elem` cs && bishop `elem` cs)
        discardOf p >>= (`shouldSatisfy` \cs -> nonXmen `elem` cs && signatureEvent `elem` cs)
        length <$> handOf p `shouldReturn` 0

    it "can shuffle only one ally" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        rogue <- putInDiscard p "37002"
        bishop <- putInDiscard p "37011"
        card <- putInHand p "37031"
        playWith card []
        choose "Rogue" (isTargetChoice rogue)
        choose "stop after one" (isLabel "$label.done")
        deckNow p >>= (`shouldSatisfy` elem rogue)
        discardOf p >>= (`shouldSatisfy` elem bishop)

    it "draws after selecting zero allies if another player has X-Mansion" do
      tg <- testGame ["gambit", "phoenix"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay (player2 tg) "37018"
        rogue <- putInDiscard p "37002"
        top <- head <$> stackDeck p ["37007"]
        card <- putInHand p "37031"
        playWith card []
        choose "no allies" (isLabel "$label.done")
        handOf p `shouldReturn` [top]
        discardOf p >>= (`shouldSatisfy` elem rogue)

    it "requires Mutant and alter-ego form" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInDiscard p "37011"
        card <- putInHand p "37031"
        offers (isPlay card) `shouldReturn` False
      other <- testGame ["gambit"]
      inGame other do
        asHero other
        let q = player1 other
        _ <- putInDiscard q "37011"
        card <- putInHand q "37031"
        offers (isPlay card) `shouldReturn` False

  describe "War Room" do
    it "exhausts after an ally defeats a minion, ignores patrol, and cannot retrigger exhausted" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        room <- putIntoPlay p "37030"
        _ <- addMinion p Minions.headhuntersHenchman
        skill <- putIntoPlay p "37013"
        _ <- putIntoPlay p "37012"
        enemy <- addMinion p Minions.weaponsRunner
        placeMainThreat 5
        before <- mainSchemeThreatNow
        dazzler <- allyIn "37012"
        choose "Dazzler attacks" (isAllyAttack dazzler)
        choose "defeat the minion" (isTargetChoice enemy)
        choose "War Room" (cardAbility room 1)
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (before - 1)
        usesNow skill `shouldReturn` 3
        supportsWith "37030" >>= (`shouldSatisfy` all supportExhausted)
        resolve [AllyMessage (allyId dazzler) ReadiedAlly]
        second <- addMinion p Minions.weaponsRunner
        readyDazzler <- allyIn "37012"
        choose "Dazzler attacks again" (isAllyAttack readyDazzler)
        choose "defeat another minion" (isTargetChoice second)
        offers (cardAbility room 1) `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` (before - 1)

    it "permits only one copy per player" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "37030"
        second <- putInHand p "37030"
        _ <- putInHand p "01088"
        offers (isPlay second) `shouldReturn` False

    it "responds to another player's ally but crisis prevents targeting the main scheme" do
      tg <- testGame ["gambit", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        asHero tg
        room <- putIntoPlay p "37030"
        attacker <- putIntoPlay q "37012"
        side <- sideSchemeOf <$> revealNow p SideSchemes.underAttack
        choose "Gambit places threat on Under Attack" (isLabel $ countLabel "placeThreatOnUnderAttack" 2)
        choose "Peter places threat on Under Attack" (isLabel $ countLabel "placeThreatOnUnderAttack" 2)
        sideBefore <- sideSchemeThreat side
        enemy <- addMinion q Minions.weaponsRunner
        placeMainThreat 5
        before <- mainSchemeThreatNow
        runNow [AllyMessage (allyIdOf attacker) AllyAttacked]
        choose "defeat the minion" (isTargetChoice enemy)
        choose "War Room sees the other ally" (cardAbility room 1)
        main <- mainSchemeIdNow
        offers (isTargetChoice main) `shouldReturn` False
        choose "remove side-scheme threat" (isTargetChoice side)
        mainSchemeThreatNow `shouldReturn` before
        sideSchemeThreat side `shouldReturn` (sideBefore - 1)

    it "does not answer an ally's non-attack damage or a hero's attack" do
      tg <- testGame ["gambit"]
      let p = player1 tg
      inGame tg do
        asHero tg
        room <- putIntoPlay p "37030"
        ally <- putIntoPlay p "37012"
        enemy <- addMinion p Minions.weaponsRunner
        runNow [MinionMessage enemy $ MinionDamaged (AllyRef $ allyIdOf ally) (toDamage 2 FromAbility)]
        offers (cardAbility room 1) `shouldReturn` False
        other <- addMinion p Minions.weaponsRunner
        choose "Gambit attacks" (isHeroAttack p)
        choose "defeat the other minion" (isTargetChoice other)
        offers (cardAbility room 1) `shouldReturn` False
        supportsWith "37030" >>= (`shouldSatisfy` all (not . supportExhausted))
