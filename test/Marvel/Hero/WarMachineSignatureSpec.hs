module Marvel.Hero.WarMachineSignatureSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hp (HP (..))
import Marvel.MainScheme.Types (Attrs (mainSchemeThreat))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types (overMinion)
import Marvel.SideScheme.Cards qualified as Schemes
import TestImport hiding (mainSchemeThreat)

warMachine :: IO TestGame
warMachine = testGame ["war-machine"]

ownAbility :: (IsRef a) => a -> Natural -> Choice -> Bool
ownAbility target index = \case
  UseAbility ab -> abilitySource ab == toRef target && abilityIndex ab == index
  _ -> False

asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeForm p
  choose "Locked and Loaded" (ownAbility p 1)
  pass
  clearHand p

ammoOf :: IdentityId -> GameAppT Natural
ammoOf p = playerIdentityCounters <$> playerAttrs p

setAmmo :: IdentityId -> Natural -> GameAppT ()
setAmmo p n = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentityCounters = n}

payCodes :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
payCodes p card codes = traverse (putInHand p) codes >>= playWith card

finishResources :: GameAppT ()
finishResources = whenM (offers isFinishPayment) (choose "finish paying" isFinishPayment)

mainThreat :: GameAppT Natural
mainThreat = getsGame (sum . map (mainSchemeThreat . toAttrs) . HashMap.elems . gameMainSchemes)

largeWhiplash :: IdentityId -> GameAppT MinionId
largeWhiplash p = do
  enemy <- addMinion p Minions.whiplash
  withGame_ $ entitiesL . minionsL . ix enemy %~ overMinion (\a -> a {minionHitPoints = HP 12})
  pure enemy

spec :: Spec
spec = describe "War Machine signature events" do
  it "none of the four signature actions is playable in alter ego even with resources, ammo and targets" do
    tg <- warMachine
    let p = player1 tg
    inGame tg do
      clearHand p
      setAmmo p 5
      placeMainThreat 3
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      for_ ["23008", "23009", "23010", "23011"] \code -> do
        card <- putInHand p code
        offers (isPlay card) `shouldReturn` False

  describe "Repulsor Beam (23008)" do
    it "pays one ammo and one resource, then chooses an enemy for four damage" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        beam <- putInHand p "23008"
        payCodes p beam ["01088"]
        ammoOf p `shouldReturn` 4
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4

    it "cannot be played in alter ego or without ammo and a way to generate it" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        beam <- putInHand p "23008"
        _ <- putInHand p "01088"
        offers (isPlay beam) `shouldReturn` False
        asHero p
        setAmmo p 0
        beam' <- putInHand p "23008"
        energy <- putInHand p "01088"
        offers (isPlay beam') `shouldReturn` False
        ammoOf p `shouldReturn` 0
        handOf p >>= (`shouldSatisfy` (\cs -> beam' `elem` cs && energy `elem` cs))

    it "respects guard and excludes enemies that cannot take damage" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        guard <- addMinion p Minions.hydraMercenary
        robot <- addMinion p Minions.edisonsGiantRobot
        beam <- putInHand p "23008"
        payCodes p beam ["01088"]
        vid <- villainIdNow
        offers (isTargetChoice vid) `shouldReturn` False
        offers (isTargetChoice robot) `shouldReturn` False
        choose "attack the guard" (isTargetChoice guard)
        pass
        minionDamageOf guard `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 0

    it "is an ordinary non-ranged attack and triggers retaliate if the enemy survives" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        enemy <- largeWhiplash p
        beam <- putInHand p "23008"
        payCodes p beam ["01088"]
        choose "attack Whiplash" (isTargetChoice enemy)
        pass
        minionDamageOf enemy `shouldReturn` Just 4
        damageOf p `shouldReturn` 1

    it "pays its costs before stun replaces the entire attack" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p IdentityStunned]
        beam <- putInHand p "23008"
        payCodes p beam ["01088"]
        pass
        ammoOf p `shouldReturn` 4
        isStunned p `shouldReturn` False
        villainDamageNow `shouldReturn` 0

  describe "Targeted Strike (23009)" do
    it "pays one ammo and removes three threat from the chosen scheme" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        strike <- putInHand p "23009"
        payCodes p strike ["01088"]
        chooseMainScheme
        pass
        mainThreat `shouldReturn` 2
        ammoOf p `shouldReturn` 4

    it "can remove fewer than three threat and respects crisis on the main scheme" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 4
        side <- revealNow p Schemes.crowdControl
        pass
        strike <- putInHand p "23009"
        payCodes p strike ["01088"]
        main <- mainSchemeIdNow
        offers (isTargetChoice main) `shouldReturn` False
        choose "clear the crisis scheme" (isTargetChoice $ sideSchemeOf side)
        pass
        sideSchemeThreat (sideSchemeOf side) `shouldReturn` 0
        mainThreat `shouldReturn` 4

    it "cannot thwart the main scheme through patrol but can choose a side scheme" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 4
        _ <- addMinion p Minions.headhuntersHenchman
        side <- revealNow p Schemes.bombScare
        pass
        strike <- putInHand p "23009"
        payCodes p strike ["01088"]
        main <- mainSchemeIdNow
        offers (isTargetChoice main) `shouldReturn` False
        choose "thwart a side scheme" (isTargetChoice $ sideSchemeOf side)
        pass
        mainThreat `shouldReturn` 4

    it "a confused hero may pay to remove confusion even with no threat to remove" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p IdentityConfused, IdentityMessage p IdentityStunned]
        strike <- putInHand p "23009"
        payCodes p strike ["01088"]
        pass
        isConfused p `shouldReturn` False
        isStunned p `shouldReturn` True
        mainThreat `shouldReturn` 0
        ammoOf p `shouldReturn` 4

    it "is unavailable with no threat when not confused and cannot use ammo it does not have" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        strike <- putInHand p "23009"
        _ <- putInHand p "01088"
        offers (isPlay strike) `shouldReturn` False
        placeMainThreat 3
        setAmmo p 0
        offers (isPlay strike) `shouldReturn` False

    it "can generate its missing ammo with a Gauntlet Gun during payment" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "23005"
        clearHand p
        setAmmo p 0
        placeMainThreat 4
        strike <- putInHand p "23009"
        choose "play the thwart" (isPlay strike)
        choose "pay with gun" (isAbilityOfCard gun)
        finishResources
        chooseMainScheme
        pass
        mainThreat `shouldReturn` 1
        ammoOf p `shouldReturn` 0

  describe "Scorched Earth (23010)" do
    it "damages the villain and every player's minions, ignoring guard and retaliate" do
      tg <- testGame ["war-machine", "captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        guard <- addMinion p Minions.hydraMercenary
        other <- largeWhiplash (player2 tg)
        resolve [IdentityMessage p IdentityStunned, IdentityMessage p IdentityConfused]
        earth <- putInHand p "23010"
        payCodes p earth ["01088", "01089"]
        pass
        ammoOf p `shouldReturn` 2
        minionDamageOf guard `shouldReturn` Nothing
        minionDamageOf other `shouldReturn` Just 3
        villainDamageNow `shouldReturn` 3
        damageOf p `shouldReturn` 0
        isStunned p `shouldReturn` True
        isConfused p `shouldReturn` True

    it "deals all its damage before a response to a defeated enemy" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        hall <- putIntoPlay p "06017"
        _ <- addMinion p Minions.hydraMercenary
        survivor <- addMinion p Minions.abomination
        earth <- putInHand p "23010"
        payCodes p earth ["01088", "01089"]
        offers (isAbilityOfCard hall) `shouldReturn` True
        -- The surviving enemy has already taken its share when the response is offered.
        minionDamageOf survivor `shouldReturn` Just 3
        villainDamageNow `shouldReturn` 3
        choose "place glory" (isAbilityOfCard hall)
        pass

    it "skips damage-immune enemies and lets tough prevent damage normally" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        robot <- addMinion p Minions.edisonsGiantRobot
        tough <- addMinion p Minions.armoredGuard
        earth <- putInHand p "23010"
        payCodes p earth ["01088", "01089"]
        pass
        minionDamageOf robot `shouldReturn` Just 0
        minionDamageOf tough `shouldReturn` Just 0
        villainDamageNow `shouldReturn` 3

    it "cannot pay with fewer than three ammo unless resource generation supplies the shortage" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        setAmmo p 2
        earth <- putInHand p "23010"
        energy <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay earth) `shouldReturn` False
        gun <- putIntoPlay p "23005"
        choose "play with future ammo" (isPlay earth)
        choose "generate needed ammo" (isAbilityOfCard gun)
        choose "pay remaining resources" (isPayWith energy)
        finishResources
        pass
        ammoOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 3

  describe "Full Auto (23011)" do
    it "chooses its enemy as a cost before paying resources, then deals eight damage without another choice" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        auto <- putInHand p "23011"
        energy <- putInHand p "01088"
        choose "play Full Auto" (isPlay auto)
        ammoOf p `shouldReturn` 5
        chooseVillain
        ammoOf p `shouldReturn` 5
        choose "pay two resources" (isPayWith energy)
        finishResources
        pass
        ammoOf p `shouldReturn` 1
        villainDamageNow `shouldReturn` 8

    it "does not start or spend resources with only three ammo and no counter-generating resource ability" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        setAmmo p 3
        auto <- putInHand p "23011"
        energy <- putInHand p "01088"
        offers (isPlay auto) `shouldReturn` False
        ammoOf p `shouldReturn` 3
        handOf p >>= (`shouldSatisfy` elem energy)

    it "deals excess damage to the villain through overkill" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        enemy <- addMinion p Minions.hydraMercenary
        auto <- putInHand p "23011"
        energy <- putInHand p "01088"
        playOn auto enemy [energy]
        pass
        minionDamageOf enemy `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 5
        ammoOf p `shouldReturn` 1

    it "does not offer a guarded villain or damage-immune enemy when choosing the cost target" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        guard <- addMinion p Minions.hydraMercenary
        immune <- addMinion p Minions.edisonsGiantRobot
        auto <- putInHand p "23011"
        _ <- putInHand p "01088"
        choose "play" (isPlay auto)
        vid <- villainIdNow
        offers (isTargetChoice vid) `shouldReturn` False
        offers (isTargetChoice immune) `shouldReturn` False
        offers (isTargetChoice guard) `shouldReturn` True
        ammoOf p `shouldReturn` 5

    it "does not retarget when guard appears after the enemy was chosen" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        auto <- putInHand p "23011"
        energy <- putInHand p "01088"
        choose "play" (isPlay auto)
        chooseVillain
        guard <- revealNow p Minions.hydraMercenary
        pass
        choose "finish paying" (isPayWith energy)
        finishResources
        pass
        ammoOf p `shouldReturn` 1
        villainDamageNow `shouldReturn` 0
        minionDamageOf (minionOf guard) `shouldReturn` Just 0

    it "pays for its chosen target even if that target gains tough before the attack resolves" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        auto <- putInHand p "23011"
        energy <- putInHand p "01088"
        choose "play" (isPlay auto)
        chooseVillain
        vid <- villainIdNow
        runNow [GiveStatus (toRef vid) ToughStatus]
        choose "pay" (isPayWith energy)
        finishResources
        pass
        ammoOf p `shouldReturn` 1
        villainDamageNow `shouldReturn` 0

    it "does not choose a replacement if the paid target leaves play during payment" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        target <- addMinion p Minions.abomination
        auto <- putInHand p "23011"
        energy <- putInHand p "01088"
        choose "play" (isPlay auto)
        choose "select Abomination" (isTargetChoice target)
        runNow [RemoveFromPlay $ toRef target]
        choose "pay" (isPayWith energy)
        finishResources
        pass
        ammoOf p `shouldReturn` 1
        villainDamageNow `shouldReturn` 0

    it "keeps ammo and returns spent cards when resource payment is aborted" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        auto <- putInHand p "23011"
        one <- putInHand p "23008"
        _ <- putInHand p "01088"
        choose "play" (isPlay auto)
        chooseVillain
        choose "spend one resource" (isPayWith one)
        ammoOf p `shouldReturn` 5
        Just acid <- getActiveCost
        runNow [AbortPayment acid]
        ammoOf p `shouldReturn` 5
        handOf p >>= (`shouldSatisfy` (\cs -> auto `elem` cs && one `elem` cs))
        villainDamageNow `shouldReturn` 0

    it "tough prevents the whole attack and its overkill" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        tough <- addMinion p Minions.armoredGuard
        auto <- putInHand p "23011"
        energy <- putInHand p "01088"
        playOn auto tough [energy]
        pass
        minionDamageOf tough `shouldReturn` Just 0
        villainDamageNow `shouldReturn` 0
        ammoOf p `shouldReturn` 1

    it "is not ranged, so a surviving target retaliates" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        enemy <- largeWhiplash p
        auto <- putInHand p "23011"
        energy <- putInHand p "01088"
        playOn auto enemy [energy]
        pass
        minionDamageOf enemy `shouldReturn` Just 8
        damageOf p `shouldReturn` 1

    it "pays four ammo and the chosen-target cost before stun replaces its attack" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p IdentityStunned]
        auto <- putInHand p "23011"
        energy <- putInHand p "01088"
        vid <- villainIdNow
        playOn auto vid [energy]
        pass
        ammoOf p `shouldReturn` 1
        isStunned p `shouldReturn` False
        villainDamageNow `shouldReturn` 0
