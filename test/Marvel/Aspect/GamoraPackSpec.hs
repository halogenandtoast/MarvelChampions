module Marvel.Aspect.GamoraPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types (getModifiedHitPoints)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

import Control.Monad (replicateM, replicateM_)

-- | The player in hero form, their hand empty: Captain Marvel (THW 2, ATK 2, not a
-- Guardian), or Star-Lord (a Guardian; Peter Quill is not).
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

-- | Play a card from hand paying with new Energy cards (2 resources each), declining an
-- interrupt to its being played (Star-Lord's).
playPaying :: IdentityId -> PlayerCard -> Int -> GameAppT ()
playPaying p card n = do
  payment <- replicateM n (putInHand p "01088")
  choose "play the card" (isPlay card)
  chooseIf isNoResponse
  payWith payment

-- | Pick the villain, when it is not the only target and so asked.
pickVillain :: GameAppT ()
pickVillain = chooseIf . isTargetChoice =<< villainIdNow

pickMainScheme :: GameAppT ()
pickMainScheme = chooseIf . isTargetChoice =<< mainSchemeIdNow

-- | Decline responses until the choice is offered.
untilOffered :: (Choice -> Bool) -> GameAppT ()
untilOffered p = go (20 :: Int)
 where
  go 0 = error "never offered"
  go n = whenM (not <$> offers p) do
    choose "decline" isNoResponse
    go (n - 1)

-- | To the villain's attack on the player in the villain phase.
toVillainAttack :: (HasCallStack) => GameAppT ()
toVillainAttack = endTurn >> untilOffered isDefend

minionCount :: GameAppT Int
minionCount = getsGame (HashMap.size . gameMinions)

spec :: Spec
spec = describe "Gamora pack aspect and basic cards" do
  describe "Angela (18011)" do
    it "puts a minion from the top 10 encounter cards into play engaged with you" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        ([_, merc, _], _) <- setEncounterDeck [Treacheries.imTough, Minions.hydraMercenary, Treacheries.imTough] []
        angela <- putInHand p "18011"
        playWith angela []
        chooseIf (isAbilityOfCard angela)
        chooseIf (isTargetChoice merc)
        getsGame (map (minionEngagedIdentity . toAttrs) . HashMap.elems . gameMinions) `shouldReturn` [p]
        encounterDeckSize `shouldReturn` 2
        length <$> alliesWith "18011" `shouldReturn` 1

    it "is discarded when there is no minion in the top 10" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- setEncounterDeck (replicate 10 Treacheries.imTough <> [Minions.hydraMercenary]) []
        angela <- putInHand p "18011"
        playWith angela []
        chooseIf (isAbilityOfCard angela)
        minionCount `shouldReturn` 0
        encounterDeckSize `shouldReturn` 11
        alliesWith "18011" `shouldReturn` []
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "18011")

  describe "Clobber (18012)" do
    it "deals 3 damage, back to hand as the first card played this round, discarded as the second" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        clobber <- putInHand p "18012"
        playPaying p clobber 1
        pickVillain
        pass
        villainDamageNow `shouldReturn` 3
        map toCardCode <$> handOf p `shouldReturn` ["18012"]
        [again] <- handOf p
        playPaying p again 1
        pickVillain
        pass
        villainDamageNow `shouldReturn` 6
        handOf p `shouldReturn` []
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "18012")

  describe "Plan of Attack (18013)" do
    it "finds an attack event in the top 4 cards in hero form" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        [_, _, _, clobber] <- stackDeck p ["01089", "01089", "01089", "18012"]
        plan <- putInHand p "18013"
        playWith plan []
        chooseIf (isTargetChoice clobber)
        handOf p `shouldReturn` [clobber]

    it "not in the 5th card in hero form" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- stackDeck p ["01089", "01089", "01089", "01089", "18012"]
        plan <- putInHand p "18013"
        playWith plan []
        chooseIf (isLabel "$label.noMatchingCardsFound")
        handOf p `shouldReturn` []

    it "searches the top 7 cards in alter-ego form" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        [_, _, _, _, _, _, clobber] <- stackDeck p ["01089", "01089", "01089", "01089", "01089", "01089", "18012"]
        plan <- putInHand p "18013"
        playWith plan []
        chooseIf (isTargetChoice clobber)
        handOf p `shouldReturn` [clobber]

  describe "First Hit (18015)" do
    it "as a Hero Action deals 2 damage to the villain" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        firstHit <- putInHand p "18015"
        playPaying p firstHit 1
        pickVillain
        pass
        villainDamageNow `shouldReturn` 2

    it "as a Hero Action, not while a guard minion keeps the villain from you" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- addMinion p Minions.hydraMercenary
        pass
        firstHit <- putInHand p "18015"
        _ <- putInHand p "01088"
        offers (isPlay firstHit) `shouldReturn` False

    it "as a Hero Interrupt deals 2 damage to a minion initiating an attack" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        merc <- addMinion p Minions.hydraMercenary
        pass
        firstHit <- putInHand p "18015"
        energy <- putInHand p "01088"
        runNow [MinionMessage merc (MinionAttacks p)]
        playWith firstHit [energy]
        minionDamageOf merc `shouldReturn` Just 2
        villainDamageNow `shouldReturn` 0

    it "when another player's minion attacks them" do
      tg <- testGame ["captain-marvel", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        merc <- addMinion other Minions.hydraMercenary
        pass
        firstHit <- putInHand p "18015"
        energy <- putInHand p "01088"
        runNow [MinionMessage merc (MinionAttacks other)]
        untilOffered (isPlay firstHit)
        playWith firstHit [energy]
        minionDamageOf merc `shouldReturn` Just 2

    it "not when the villain attacks" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        firstHit <- putInHand p "18015"
        _ <- putInHand p "01088"
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        offers (isPlay firstHit) `shouldReturn` False

  describe "Impede (18016)" do
    it "removes 3 threat from the main scheme and returns as the first card played this round" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 4
        threat <- mainSchemeThreat
        impede <- putInHand p "18016"
        playPaying p impede 1
        pickMainScheme
        pass
        mainSchemeThreat `shouldReturn` threat - 3
        map toCardCode <$> handOf p `shouldReturn` ["18016"]

    it "is discarded when another card was played first this round" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 4
        -- no attack event to find
        _ <- stackDeck p ["01089", "01089", "01089", "01089"]
        plan <- putInHand p "18013"
        playWith plan []
        chooseIf (isLabel "$label.noMatchingCardsFound")
        impede <- putInHand p "18016"
        playPaying p impede 1
        pickMainScheme
        pass
        handOf p `shouldReturn` []
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "18016")

  describe "Godslayer (18018)" do
    it "exhausts for +2 ATK on a basic attack against the villain" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        godslayer <- putInHand p "18018"
        playPaying p godslayer 2
        pass
        choose "attack" (isHeroAttack p)
        pickVillain
        choose "use Godslayer" (isAbilityOfCard godslayer)
        pass
        villainDamageNow `shouldReturn` 4
        upgradeIsExhausted (upgradeIdOf godslayer) `shouldReturn` True

    it "against a unique minion" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        shocker <- addMinion p Minions.shocker
        pass
        godslayer <- putInHand p "18018"
        playPaying p godslayer 2
        pass
        choose "attack" (isHeroAttack p)
        choose "Shocker" (isTargetChoice shocker)
        choose "use Godslayer" (isAbilityOfCard godslayer)
        pass
        -- 3 hit points, 4 damage
        minionDamageOf shocker `shouldReturn` Nothing

    it "not against a minion that is not unique" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        merc <- addMinion p Minions.hydraMercenary
        pass
        godslayer <- putInHand p "18018"
        playPaying p godslayer 2
        pass
        choose "attack" (isHeroAttack p)
        chooseIf (isTargetChoice merc)
        offers (isAbilityOfCard godslayer) `shouldReturn` False
        pass
        minionDamageOf merc `shouldReturn` Just 2

  describe "Drax (18019)" do
    it "can't be played without a Guardian identity" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        drax <- putInHand p "18019"
        replicateM_ 2 (putInHand p "01088")
        offers (isPlay drax) `shouldReturn` False

    it "is played by a Guardian, and attacks the villain but not a minion" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        asHero p
        shocker <- addMinion p Minions.shocker
        pass
        draxCard <- putInHand p "18019"
        playPaying p draxCard 2
        pass
        drax <- allyIn "18019"
        choose "attack with Drax" (isAllyAttack drax)
        offers (isTargetChoice shocker) `shouldReturn` False
        pickVillain
        pass
        villainDamageNow `shouldReturn` 3
        minionDamageOf shocker `shouldReturn` Just 0

    it "has no attack while a guard minion keeps the villain from him" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- addMinion p Minions.hydraMercenary
        pass
        _ <- putIntoPlay p "18019"
        drax <- allyIn "18019"
        offers (isAllyAttack drax) `shouldReturn` False

  describe "Hit and Run (18020)" do
    it "deals 2 damage to an enemy and removes 2 threat from a scheme" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        threat <- mainSchemeThreat
        hitAndRun <- putInHand p "18020"
        playPaying p hitAndRun 2
        pickVillain
        pickMainScheme
        pass
        villainDamageNow `shouldReturn` 2
        mainSchemeThreat `shouldReturn` threat - 2

  describe "Pivotal Moment (18029)" do
    it "deals 2 damage to the villain while there is threat on the main scheme" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 1
        moment <- putInHand p "18029"
        playPaying p moment 1
        pickVillain
        pass
        villainDamageNow `shouldReturn` 2

    it "deals 5 with no threat on the main scheme" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        threat <- mainSchemeThreat
        ms <- mainSchemeIdNow
        resolve [MainSchemeMessage ms (MainSchemeThreatRemoved (toSource p) threat) | threat > 0]
        mainSchemeThreat `shouldReturn` 0
        moment <- putInHand p "18029"
        playPaying p moment 1
        pickVillain
        pass
        villainDamageNow `shouldReturn` 5

  describe "Comms Implant (18030)" do
    it "attaches only to a Guardian ally, max 1 per ally" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        implant <- putInHand p "18030"
        energy <- putInHand p "01088"
        offers (isPlay implant) `shouldReturn` False
        _ <- putIntoPlay p "17013"
        yondu <- allyIn "17013"
        playOn implant (allyId yondu) [energy]
        again <- putInHand p "18030"
        _ <- putInHand p "01088"
        offers (isPlay again) `shouldReturn` False

    it "gives the ally +1 THW and +1 hit point" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "17013"
        yondu <- allyIn "17013"
        implant <- putInHand p "18030"
        energy <- putInHand p "01088"
        playOn implant (allyId yondu) [energy]
        (getModifiedHitPoints =<< allyIn "17013") `shouldReturn` 3
        placeMainThreat 4
        threat <- mainSchemeThreat
        choose "thwart with Yondu" (isAllyThwart yondu)
        pickMainScheme
        pass
        -- Yondu has 2 THW
        mainSchemeThreat `shouldReturn` threat - 3

  describe "True Grit (18031)" do
    it "after your hero defends, removes threat equal to your hero's THW" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        grit <- putInHand p "18031"
        energy <- putInHand p "01088"
        toVillainAttack
        choose "defend" isDefend
        untilOffered (isPlay grit)
        threat <- mainSchemeThreat
        playWith grit [energy]
        pickMainScheme
        -- Captain Marvel has 2 THW
        mainSchemeThreat `shouldReturn` threat - 2

    it "not when the hero does not defend" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        grit <- putInHand p "18031"
        _ <- putInHand p "01088"
        toVillainAttack
        choose "no defender" isNoDefender
        offers (isPlay grit) `shouldReturn` False

  describe "reprints" do
    it "Uppercut (18014) plays as 01054: 5 damage to an enemy" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        uppercut <- putInHand p "18014"
        playPaying p uppercut 2
        pickVillain
        pass
        villainDamageNow `shouldReturn` 5

    it "Combat Training (18017) plays as 01057: +1 ATK" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        training <- putInHand p "18017"
        playPaying p training 1
        pass
        choose "attack" (isHeroAttack p)
        pickVillain
        pass
        villainDamageNow `shouldReturn` 3

    it "Energy (18021) plays as 01088: 2 resources" $ paysTwo "18021"
    it "Genius (18022) plays as 01089: 2 resources" $ paysTwo "18022"
    it "Strength (18023) plays as 01090: 2 resources" $ paysTwo "18023"

    it "Enhanced Reflexes (18032) plays as 05024: an energy resource from a use" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        reflexes <- putIntoPlay p "18032"
        map (.upgradeUses) <$> upgradesWith "18032" `shouldReturn` [3]
        firstHit <- putInHand p "18015"
        choose "play First Hit" (isPlay firstHit)
        choose "pay with reflexes" (isAbilityOfCard reflexes)
        chooseIf isFinishPayment
        pickVillain
        pass
        villainDamageNow `shouldReturn` 2
        map (.upgradeUses) <$> upgradesWith "18032" `shouldReturn` [2]

-- | Clobber (cost 2) paid with the one resource card.
paysTwo :: CardCode -> IO ()
paysTwo code = do
  tg <- testGame ["captain-marvel"]
  let p = player1 tg
  inGame tg do
    asHero p
    clobber <- putInHand p "18012"
    resource <- putInHand p code
    playWith clobber [resource]
    pickVillain
    pass
    villainDamageNow `shouldReturn` 3
