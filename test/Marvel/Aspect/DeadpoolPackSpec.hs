module Marvel.Aspect.DeadpoolPackSpec (spec) where

import Control.Monad (replicateM)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Attack (attack)
import Marvel.Damage
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Phase
import Marvel.Resource (resourcesFor)
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Villain.Types (villainStunned)
import TestImport

becomeHero :: TestGame -> GameAppT ()
becomeHero tg = changeForm (player1 tg) >> pass >> clearHand (player1 tg)

payCards :: IdentityId -> Int -> GameAppT [PlayerCard]
payCards you n = replicateM n (putInHand you "01002")

minionExists :: MinionId -> GameAppT Bool
minionExists mid = getsGame (HashMap.member mid . gameMinions)

playerSideSchemeOf :: PlayerCard -> SideSchemeId
playerSideSchemeOf = SideSchemeId . unCardId . pcCardId

spec :: Spec
spec = describe "DeadpoolPack" do
  describe "Dogpool" do
    it "has toughness, retaliate, an acceleration icon and no consequential damage" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        dog <- putIntoPlay you "44013"
        attrs <- allyIn "44013"
        Ally.allyTough attrs `shouldBe` True
        getAccelerationCount `shouldReturn` 1
        choose "Dogpool attacks" (isAllyAttack attrs)
        chooseVillain
        allyDamage <$> allyIn "44013" `shouldReturn` 0
        vid <- villainIdNow
        before <- villainDamageNow
        resolve [AllyMessage (allyIdOf dog) $ AllyWasAttacked $ attack (EnemyVillainId vid) (AllyCharacter $ allyIdOf dog) 1]
        villainDamageNow `shouldReturn` before + 1
        Ally.allyTough <$> allyIn "44013" `shouldReturn` False

    it "deals one chosen damage before leaving play when defeated" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        dog <- putIntoPlay (player1 tg) "44013"
        resolve [AllyMessage (allyIdOf dog) $ AllyDamaged GameRef (toDamage 1 FromAbility)]
        runNow [AllyMessage (allyIdOf dog) $ AllyDamaged GameRef (toDamage 4 FromAbility)]
        choose "Dogpool When Defeated" (isAbilityOfCard dog)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 1
        alliesWith "44013" `shouldReturn` []

  describe "Headpool" do
    it "makes the damaged surviving minion attack another enemy, without consequential damage" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        headpool <- putIntoPlay you "44014"
        minion <- addMinion you Minions.hydraSoldier
        attrs <- allyIn "44014"
        choose "Headpool attacks" (isAllyAttack attrs)
        choose "damage the soldier" (isTargetChoice minion)
        choose "Headpool response" (isAbilityOfCard headpool)
        offers (isTargetChoice minion) `shouldReturn` False
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        minionDamageOf minion `shouldReturn` Just 1
        allyDamage <$> allyIn "44014" `shouldReturn` 0

    it "has no response after damaging the villain or defeating a minion" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        card <- putIntoPlay you "44014"
        attrs <- allyIn "44014"
        choose "attack villain" (isAllyAttack attrs)
        chooseVillain
        offers (isAbilityOfCard card) `shouldReturn` False
        resolve [AllyMessage (allyIdOf card) ReadiedAlly]
        minion <- addMinion you Minions.weaponsRunner
        resolve [MinionMessage minion $ MinionDamaged GameRef (toDamage 1 FromAbility)]
        attrs' <- allyIn "44014"
        choose "defeat weapons runner" (isAllyAttack attrs')
        choose "runner" (isTargetChoice minion)
        offers (isAbilityOfCard card) `shouldReturn` False

  describe "Kidpool" do
    it "pierces tough and takes no consequential damage" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        _ <- putIntoPlay (player1 tg) "44015"
        vid <- villainIdNow
        resolve [VillainMessage vid VillainBecomeTough]
        attrs <- allyIn "44015"
        choose "Kidpool attack" (isAllyAttack attrs)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        allyDamage <$> allyIn "44015" `shouldReturn` 0

  describe "Lady Deadpool" do
    it "defeats a chosen non-Elite minion when she is defeated" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        lady <- putIntoPlay you "44016"
        soldier <- addMinion you Minions.hydraSoldier
        elite <- addMinion you Minions.titania
        runNow [AllyMessage (allyIdOf lady) $ AllyDamaged GameRef (toDamage 3 FromAbility)]
        choose "Lady Deadpool When Defeated" (isAbilityOfCard lady)
        offers (isTargetChoice elite) `shouldReturn` False
        choose "defeat soldier" (isTargetChoice soldier)
        pass
        minionExists soldier `shouldReturn` False
        minionExists elite `shouldReturn` True
        alliesWith "44016" `shouldReturn` []

    it "does not lose consequential damage after thwarting" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        _ <- putIntoPlay (player1 tg) "44016"
        placeMainThreat 4
        attrs <- allyIn "44016"
        choose "Lady Deadpool thwart" (isAllyThwart attrs)
        sid <- mainSchemeIdNow
        choose "main scheme" (isTargetChoice sid)
        allyDamage <$> allyIn "44016" `shouldReturn` 0

  describe "Barely a Scratch" do
    it "prevents one attack damage for each encounter icon, excluding physical acceleration tokens" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44024"
        main <- mainSchemeIdNow
        resolve [PlaceAccelerationToken main]
        card <- putInHand you "44017"
        vid <- villainIdNow
        runNow [IdentityMessage you $ IdentityWasAttacked $ attack (EnemyVillainId vid) (IdentityCharacter you) 5]
        choose "Barely a Scratch" (isPlay card)
        pass
        damageOf you `shouldReturn` 1

    it "does not answer non-attack damage" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44024"
        card <- putInHand you "44017"
        messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.damageIdentity you GameRef (toDamage 2 FromAbility)
        runNow messages
        offers (isPlay card) `shouldReturn` False
        pass
        damageOf you `shouldReturn` 2

  describe "Cutupper" do
    it "deals five attack damage and stuns the chosen enemy" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        card <- putInHand you "44018"
        payment <- payCards you 3
        playWith card payment
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5
        villainStunned . toAttrs <$> getVillain `shouldReturn` True

  describe "Da Bomb" do
    it "damages the villain then every enemy and hero by the icon count, sparing alter egos" do
      tg <- testGame ["captain-marvel", "spider-man"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
            other = player2 tg
        _ <- putIntoPlay you "44013"
        soldier <- addMinion you Minions.hydraSoldier
        card <- putInHand you "44019"
        payment <- payCards you 6
        playWith card payment
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 11
        minionDamageOf soldier `shouldReturn` Just 1
        damageOf you `shouldReturn` 1
        damageOf other `shouldReturn` 0
        allyDamage <$> allyIn "44013" `shouldReturn` 0

  describe "Get Rage-y" do
    it "readies an ally and gives it one attack until the phase ends" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        ally <- putIntoPlay you "44015"
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        card <- putInHand you "44020"
        playWith card []
        choose "Kidpool" (isTargetChoice $ allyIdOf ally)
        attrs <- allyIn "44015"
        Ally.allyExhausted attrs `shouldBe` False
        Ally.getModifiedAttack attrs `shouldReturn` 3
        resolve [EndPhase PlayerPhase]
        (Ally.getModifiedAttack =<< allyIn "44015") `shouldReturn` 2

    it "can boost a ready ally and can target another player's ally" do
      tg <- testGame ["captain-marvel", "spider-man"]
      inGame tg do
        becomeHero tg
        ally <- putIntoPlay (player2 tg) "44015"
        card <- putInHand (player1 tg) "44020"
        playWith card []
        choose "other player's ready ally" (isTargetChoice $ allyIdOf ally)
        (Ally.getModifiedAttack =<< allyIn "44015") `shouldReturn` 3

  describe "I Got This" do
    it "resolves each icon bullet once even when several matching icons are present" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44024"
        ally <- putIntoPlay you "44015"
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        placeMainThreat 4
        card <- putInHand you "44021"
        payment <- payCards you 1
        playWith card payment
        chooseVillain
        -- Crisis prevents the main scheme; Live Dangerously remains a legal scheme.
        schemes <- getsGame (HashMap.keys . gameSideSchemes)
        choose "side scheme" (isTargetChoice $ fromMaybe (error "side scheme") $ headMay schemes)
        choose "ready Kidpool" (isTargetChoice $ allyIdOf ally)
        pass
        villainDamageNow `shouldReturn` 3
        Ally.allyExhausted <$> allyIn "44015" `shouldReturn` False
        length <$> handOf you `shouldReturn` 1

    it "does not count acceleration tokens as an acceleration icon" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        main <- mainSchemeIdNow
        resolve [PlaceAccelerationToken main]
        card <- putInHand you "44021"
        _ <- payCards you 1
        offers (isPlay card) `shouldReturn` False

  describe "Not my Responsibility" do
    it "replaces threat with damage to its player's identity in alter-ego form" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        card <- putInHand you "44022"
        main <- mainSchemeIdNow
        before <- mainSchemeThreatNow
        messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.placeThreat main 2
        runNow messages
        choose "Not my Responsibility" (isPlay card)
        choose "take damage" (isTargetChoice you)
        pass
        mainSchemeThreatNow `shouldReturn` before
        damageOf you `shouldReturn` 2

    it "does not offer a second copy after the pending threat placement has been replaced" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        first <- putInHand you "44022"
        second <- putInHand you "44022"
        main <- mainSchemeIdNow
        before <- mainSchemeThreatNow
        messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.placeThreat main 2
        runNow messages
        choose "first Not my Responsibility" (isPlay first)
        choose "take damage" (isTargetChoice you)
        offers (isPlay second) `shouldReturn` False
        pass
        mainSchemeThreatNow `shouldReturn` before
        damageOf you `shouldReturn` 2
        hand <- handOf you
        (pcCardId second `elem` map pcCardId hand) `shouldBe` True

    it "can assign damage to its own ally but never another player's ally" do
      tg <- testGame ["captain-marvel", "spider-man"]
      inGame tg do
        let you = player1 tg
        clearHand you
        ours <- putIntoPlay you "44015"
        theirs <- putIntoPlay (player2 tg) "44014"
        card <- putInHand you "44022"
        main <- mainSchemeIdNow
        messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.placeThreat main 1
        runNow messages
        choose "Not my Responsibility" (isPlay card)
        offers (isTargetChoice $ allyIdOf theirs) `shouldReturn` False
        choose "our ally" (isTargetChoice $ allyIdOf ours)
        pass
        allyDamage <$> allyIn "44015" `shouldReturn` 1
        allyDamage <$> allyIn "44014" `shouldReturn` 0

  describe "Pool Inspection" do
    it "ignores crisis for its first main removal and removes icon-count threat from every legal scheme" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        crisis <- revealNow you Schemes.crowdControl
        pass
        -- Crowd Control starts at two; keep its crisis in play after the two-point sweep.
        resolve [SideSchemeMessage (sideSchemeOf crisis) $ SideSchemePlaceThreat 1]
        _ <- putIntoPlay you "44013"
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand you "44023"
        payment <- payCards you 6
        playWith card payment
        pass
        -- Five initially, but the remaining crisis blocks the second main-scheme clause.
        mainSchemeThreatNow `shouldReturn` (before - 5)
        sideSchemeThreat (sideSchemeOf crisis) `shouldReturn` 1

    it "uses zero icon count as zero for the second clause" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand you "44023"
        payment <- payCards you 6
        playWith card payment
        pass
        mainSchemeThreatNow `shouldReturn` before - 5

  describe "Live Dangerously" do
    it "adds all four icons, three threat per player and two hand size to each identity until defeated" do
      tg <- testGame ["captain-marvel", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        clearHand you
        before <- handSizeOf you
        otherBefore <- handSizeOf other
        card <- putInHand you "44024"
        playWith card []
        pass
        sideSchemeThreat (playerSideSchemeOf card) `shouldReturn` 6
        handSizeOf you `shouldReturn` before + 2
        handSizeOf other `shouldReturn` otherBefore + 2
        getEncounterIconCounts `shouldReturn` (1, 1, 1, 1)
        resolve [SideSchemeMessage (playerSideSchemeOf card) $ SideSchemeThreatRemoved GameRef 6]
        handSizeOf you `shouldReturn` before
        handSizeOf other `shouldReturn` otherBefore
        map toCardCode <$> getPlayerVictoryDisplay `shouldReturn` ["44024"]

  describe "Self resources" do
    for_ [("44025", Physical), ("44026", Mental), ("44027", Energy)] \(code, resource) ->
      it (show code <> " triples at zero damage, doubles at one through four, and generates one at five") do
        tg <- testGame ["captain-marvel"]
        inGame tg do
          let you = player1 tg
          clearHand you
          card <- putInHand you code
          for_ [(0, 3), (1, 2), (4, 2), (5, 1)] \(damage, amount) -> do
            setDamage you damage
            resourcesFor card Nothing `shouldReturn` replicate amount resource
            printedResourcesFor card `shouldReturn` [resource]

    for_ ["44025", "44026", "44027"] \code ->
      it (show code <> " pays a three-resource card on its own when undamaged") do
        tg <- testGame ["captain-marvel"]
        inGame tg do
          becomeHero tg
          let you = player1 tg
          card <- putInHand you "44018"
          payment <- putInHand you code
          playWith card [payment]
          chooseVillain
          pass
          villainDamageNow `shouldReturn` 5
          length <$> handOf you `shouldReturn` 0
