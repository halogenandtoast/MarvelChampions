module Marvel.Aspect.SpiderHamPackSpec (spec) where

import Marvel.Damage
import Marvel.Ally.Cards qualified as Allies
import Marvel.Keyword (Keyword (Patrol))
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Phase
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait
import Data.HashSet qualified as HashSet
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import TestImport

pack :: IO TestGame
pack = testGame ["spider-ham"]

ownAbility :: PlayerCard -> Choice -> Bool
ownAbility card c@(UseAbility ab) = abilityIndex ab == 1 && isAbilityOfCard card c
ownAbility _ _ = False

playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying who card codes = traverse (putInHand who) codes >>= playWith card

side :: IdentityId -> CardDef -> GameAppT SideSchemeId
side who def = do
  card <- encounterCard def
  resolve [RevealEncounterCard who card]
  pure $ sideSchemeOf card

spec :: Spec
spec = describe "Spider-Ham pack" do
  describe "Lady Spider" do
    it "copies the actual threat removed by her thwart to a different scheme before consequential damage" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        sid <- side p Schemes.bombScare
        current <- sideSchemeThreat sid
        resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved GameRef (current - 1)]
        sideSchemeThreat sid `shouldReturn` 1
        placeMainThreat 4
        card <- putInHand p "30012"
        playPaying p card ["01088", "01089"]
        attrs <- allyIn "30012"
        choose "Lady Spider thwarts" (isAllyThwart attrs)
        choose "side scheme" (isTargetChoice sid)
        choose "copy removal" (ownAbility card)
        offers (isTargetChoice sid) `shouldReturn` False
        chooseMainScheme
        mainSchemeThreat `shouldReturn` 3
        allyDamage <$> allyIn "30012" `shouldReturn` 2

    it "needs another Web-Warrior card under her controller's control" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- side p Schemes.breakinAndTakin
        placeMainThreat 4
        card <- putInHand p "30012"
        playPaying p card ["01088", "01089"]
        attrs <- allyIn "30012"
        choose "Lady Spider thwarts" (isAllyThwart attrs)
        chooseMainScheme
        offers (ownAbility card) `shouldReturn` False

    it "does not respond to another ally's thwart" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "30012"
        _ <- putIntoPlay p "01084"
        placeMainThreat 4
        attrs <- allyIn "01084"
        choose "Mockingbird thwarts" (isAllyThwart attrs)
        chooseMainScheme
        offers (ownAbility card) `shouldReturn` False

  describe "Spider-Man (Pavitr Prabhakar)" do
    it "counts himself and every controlled Web-Warrior card" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "30023"
        placeMainThreat 5
        card <- putInHand p "30013"
        playPaying p card ["01088", "01089"]
        choose "Pavitr response" (ownAbility card)
        chooseMainScheme
        mainSchemeThreat `shouldReturn` 2

    it "removes threat in alter-ego and does not count other players' cards" do
      tg <- testGame ["spider-ham", "ghost-spider"]
      let p = player1 tg
      inGame tg do
        placeMainThreat 4
        card <- putInHand p "30013"
        playPaying p card ["01088", "01089"]
        choose "Pavitr response" (ownAbility card)
        chooseMainScheme
        mainSchemeThreat `shouldReturn` 3

  describe "Even the Odds" do
    it "removes one threat per player from every side scheme and damages for each defeated scheme" do
      tg <- testGame ["spider-ham", "spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        first <- side p (Schemes.breakinAndTakin {cdHazards = 0})
        second <- side p Schemes.bombScare
        resolve [SideSchemeMessage first $ SideSchemeThreatRemoved GameRef 2, SideSchemeMessage second $ SideSchemeThreatRemoved GameRef 2]
        card <- putInHand p "30014"
        playPaying p card ["01088"]
        sideSchemeThreat first `shouldReturn` 0
        sideSchemeThreat second `shouldReturn` 0
        villainDamageNow `shouldReturn` 2

    it "still counts an eliminated player for the per-player amount" do
      tg <- testGame ["spider-ham", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        changeForm p
        sid <- side p Schemes.breakinAndTakin
        resolve [RemoveFromPlay $ IdentityRef other]
        card <- putInHand p "30014"
        playPaying p card ["01088"]
        sideSchemeThreat sid `shouldReturn` 2

    it "deals no damage for schemes that survive and does not remove main-scheme threat" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        sid <- side p Schemes.crowdControl
        placeMainThreat 3
        before <- sideSchemeThreat sid
        card <- putInHand p "30014"
        playPaying p card ["01088"]
        sideSchemeThreat sid `shouldReturn` (before - 1)
        mainSchemeThreat `shouldReturn` 3
        villainDamageNow `shouldReturn` 0

    it "requires an energy resource" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- side p Schemes.breakinAndTakin
        clearHand p
        card <- putInHand p "30014"
        _ <- putInHand p "01089"
        offers (isPlay card) `shouldReturn` False
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` True

  describe "Overwatch" do
    it "attaches to the main scheme, copies only the actual removal, and discards as cost" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        sid <- side p Schemes.bombScare
        placeMainThreat 1
        main <- mainSchemeIdNow
        card <- putInHand p "30019"
        playOn card main []
        before <- sideSchemeThreat sid
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        choose "Overwatch interrupt" (ownAbility card)
        upgradesWith "30019" `shouldReturn` []
        offers (isTargetChoice main) `shouldReturn` False
        choose "copy to side scheme" (isTargetChoice sid)
        sideSchemeThreat sid `shouldReturn` (before - 1)
        mainSchemeThreat `shouldReturn` 0

    it "attaches to a side scheme and can copy past patrol" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        sid <- side p Schemes.breakinAndTakin
        placeMainThreat 4
        _ <- addMinion p (Minions.hydraMercenary {cdKeywords = HashSet.singleton Patrol})
        card <- putInHand p "30019"
        playOn card sid []
        choose "basic thwart" (isHeroThwart p)
        choose "thwart the side scheme" (isTargetChoice sid)
        choose "Overwatch interrupt" (ownAbility card)
        chooseMainScheme
        mainSchemeThreat `shouldReturn` 2

    it "does not trigger from a non-thwart removal or duplicate another Overwatch on the same scheme" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        sid <- side p Schemes.crowdControl
        main <- mainSchemeIdNow
        placeMainThreat 4
        card <- putInHand p "30019"
        playOn card sid []
        runNow [SideSchemeMessage sid $ SideSchemeThreatRemoved (IdentityRef p) 1]
        offers (ownAbility card) `shouldReturn` False
        _ <- side p Schemes.bombScare
        duplicate <- putInHand p "30019"
        choose "play second Overwatch" (isPlay duplicate)
        offers (isTargetChoice sid) `shouldReturn` False
        offers (isTargetChoice main) `shouldReturn` True
        chooseMainScheme
        length <$> upgradesWith "30019" `shouldReturn` 2

    it "requires hero form for its interrupt" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        sid <- side p Schemes.crowdControl
        card <- putInHand p "30019"
        playOn card sid []
        runNow [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef p) 1]
        offers (ownAbility card) `shouldReturn` False

    it "cannot copy to the main scheme while a crisis icon remains in play" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        sid <- side p Schemes.crowdControl
        placeMainThreat 4
        card <- putInHand p "30019"
        playOn card sid []
        choose "basic thwart" (isHeroThwart p)
        choose "thwart crisis scheme" (isTargetChoice sid)
        offers (ownAbility card) `shouldReturn` False
        mainSchemeThreat `shouldReturn` 4

  describe "Scarlet Spider" do
    it "names the type before looking, then takes damage and draws for a correct guess" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "30020"
        playPaying p card ["01088", "01089"]
        encounter <- revealNow p Minions.hydraMercenary
        choose "Scarlet Spider interrupt" (ownAbility card)
        loggedBeforeName <- liftIO $ Text.concat . map entryText <$> readIORef tg.logged
        loggedBeforeName `shouldSatisfy` (not . Text.isInfixOf "Hydra Mercenary")
        getsGame gameFocusedCards `shouldReturn` []
        choose "name a minion" (isLabel "$label.nameMinionType")
        getsGame gameFocusedCards `shouldReturn` []
        getsGame (HashMap.lookup p . gamePrivateCards) `shouldReturn` Just [EncounterCard encounter]
        before <- length <$> handOf p
        choose "finish looking" (isLabel "$label.done")
        allyDamage <$> allyIn "30020" `shouldReturn` 1
        length <$> handOf p `shouldReturn` (before + 1)

    it "still shows the looked-at card on a wrong guess but gives neither damage nor draw" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "30020"
        encounter <- revealNow p Minions.hydraMercenary
        choose "Scarlet Spider interrupt" (ownAbility card)
        choose "name a treachery" (isLabel "$label.nameTreacheryType")
        getsGame gameFocusedCards `shouldReturn` []
        getsGame (HashMap.lookup p . gamePrivateCards) `shouldReturn` Just [EncounterCard encounter]
        before <- length <$> handOf p
        choose "finish looking" (isLabel "$label.done")
        allyDamage <$> allyIn "30020" `shouldReturn` 0
        length <$> handOf p `shouldReturn` before

    it "cannot be played without an already controlled Web-Warrior" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "30020"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay card) `shouldReturn` False

    it "draws even when the correct guess's damage defeats Scarlet Spider" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "30020"
        resolve [AllyMessage (allyIdOf card) $ AllySetDamage 3]
        _ <- revealNow p Minions.hydraMercenary
        choose "Scarlet Spider interrupt" (ownAbility card)
        choose "name a minion" (isLabel "$label.nameMinionType")
        before <- length <$> handOf p
        choose "finish looking" (isLabel "$label.done")
        alliesWith "30020" `shouldReturn` []
        length <$> handOf p `shouldReturn` (before + 1)

  describe "SP//dr" do
    it "returns to hand when thwarting with one remaining HP takes excess consequential damage" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "30021"
        playPaying p card ["01088"]
        choose "SP//dr attacks" . isAllyAttack =<< allyIn "30021"
        chooseVillain
        resolve [AllyMessage (allyIdOf card) ReadiedAlly]
        placeMainThreat 3
        choose "SP//dr thwarts" . isAllyThwart =<< allyIn "30021"
        chooseMainScheme
        pressWhenRevealed
        alliesWith "30021" `shouldReturn` []
        handOf p >>= (`shouldSatisfy` any ((== pcCardId card) . pcCardId))
        discardOf p >>= (`shouldSatisfy` all ((/= pcCardId card) . pcCardId))

    it "does not return for exactly lethal consequential damage" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "30021"
        placeMainThreat 3
        choose "SP//dr thwarts" . isAllyThwart =<< allyIn "30021"
        chooseMainScheme
        pressWhenRevealed
        handOf p >>= (`shouldSatisfy` all ((/= pcCardId card) . pcCardId))
        discardOf p >>= (`shouldSatisfy` any ((== pcCardId card) . pcCardId))

    it "does not return for excess damage from an ability" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "30021"
        resolve [AllyMessage (allyIdOf card) $ AllyDamaged GameRef (toDamage 3 FromAbility)]
        handOf p >>= (`shouldSatisfy` all ((/= pcCardId card) . pcCardId))
        discardOf p >>= (`shouldSatisfy` any ((== pcCardId card) . pcCardId))

    it "does not return when tough prevents the consequential damage" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "30021"
        resolve [AllyMessage (allyIdOf card) $ AllySetDamage 1, GiveStatus (AllyRef $ allyIdOf card) ToughStatus]
        placeMainThreat 3
        choose "SP//dr thwarts" . isAllyThwart =<< allyIn "30021"
        chooseMainScheme
        allyDamage <$> allyIn "30021" `shouldReturn` 1
        handOf p >>= (`shouldSatisfy` all ((/= pcCardId card) . pcCardId))

    it "does not resolve its When Defeated text while blanked" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "30021"
        -- Apply the engine's existing blanking effect to the ally to exercise this interaction.
        resolve [CreatedEffect Allies.vivianIronheart GameRef (AllyEntity $ AllyWithId $ allyIdOf card),
          AllyMessage (allyIdOf card) $ AllySetDamage 1]
        placeMainThreat 3
        choose "SP//dr thwarts" . isAllyThwart =<< allyIn "30021"
        chooseMainScheme
        pressWhenRevealed
        handOf p >>= (`shouldSatisfy` all ((/= pcCardId card) . pcCardId))
        discardOf p >>= (`shouldSatisfy` any ((== pcCardId card) . pcCardId))

  describe "Warrior of the Great Web" do
    it "attaches only to a title containing Spider, excluding SP//dr, and grants Web-Warrior" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "30021"
        _ <- putIntoPlay p "01084"
        spider <- putIntoPlay p "30013"
        card <- putInHand p "30029"
        payment <- putInHand p "01090"
        choose "play Warrior" (isPlay card)
        offers (isTargetChoice $ allyIdOf spider) `shouldReturn` True
        spdr <- allyIn "30021"
        offers (isTargetChoice $ allyId spdr) `shouldReturn` False
        mock <- allyIn "01084"
        offers (isTargetChoice $ allyId mock) `shouldReturn` False
        choose "attach to Spider-Ham" (isTargetChoice p)
        payWith [payment]
        getModifiers p >>= (`shouldSatisfy` elem (TraitModifier WebWarrior))

    it "stacks one ATK per Web-Warrior departure until phase end, even if the upgrade leaves play" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "30029"
        payment <- putInHand p "01090"
        playOn card p [payment]
        first <- putIntoPlay p "30021"
        second <- putIntoPlay p "30013"
        runNow [RemoveFromPlay $ AllyRef $ allyIdOf first]
        choose "Warrior response" (ownAbility card)
        runNow [RemoveFromPlay $ AllyRef $ allyIdOf second]
        choose "Warrior response again" (ownAbility card)
        modifiers <- getModifiers p
        sum [n | AttackModifier n <- modifiers] `shouldBe` 2
        resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf card]
        modifiers' <- getModifiers p
        sum [n | AttackModifier n <- modifiers'] `shouldBe` 2
        resolve [EndPhase PlayerPhase]
        modifiers'' <- getModifiers p
        sum [n | AttackModifier n <- modifiers''] `shouldBe` 0

    it "does not trigger for an ally without Web-Warrior" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "30029"
        payment <- putInHand p "01090"
        playOn card p [payment]
        ally <- putIntoPlay p "01084"
        runNow [RemoveFromPlay $ AllyRef $ allyIdOf ally]
        offers (ownAbility card) `shouldReturn` False

  describe "Even the Odds validity" do
    it "cannot be played when the only side scheme is protected from threat removal" do
      tg <- pack
      inGame tg do
        let p = player1 tg
        changeForm p
        _ <- addMinion p Minions.abomination
        _ <- side p Schemes.totalDestruction
        card <- putInHand p "30014"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False
