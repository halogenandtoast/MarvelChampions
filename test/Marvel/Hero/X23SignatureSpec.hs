module Marvel.Hero.X23SignatureSpec (spec) where

import Control.Monad (replicateM_)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (PlayerIdentitySide (..), currentIdentity)
import Marvel.Keyword (Keyword (Overkill))
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Minion.Cards qualified as Minions
import TestImport

asX23 :: IdentityId -> GameAppT ()
asX23 p = clearHand p >> changeForm p >> pass

powers :: IdentityId -> GameAppT (Natural, Natural)
powers p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> (,) <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h)
  _ -> pure (0, 0)

payOne :: IdentityId -> PlayerCard -> GameAppT ()
payOne p c = putInHand p "01090" >>= \r -> playWith c [r]

allyDamageNow :: AllyId -> GameAppT Natural
allyDamageNow aid = getsGame $ maybe 0 (Ally.allyDamage . toAttrs) . HashMap.lookup aid . gameAllies

setAllyDamage :: AllyId -> Natural -> GameAppT ()
setAllyDamage aid n = resolve [AllyMessage aid $ AllySetDamage n]

spec :: Spec
spec = describe "X23 signature events" do
  describe "Animal Instinct" do
    it "adds current ATK to THW for one basic thwart and then expires" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        card <- putInHand p "43004"
        choose "basic thwart" (isHeroThwart p)
        playWith card []
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 3
        powers p `shouldReturn` (2, 1)

    it "uses increased ATK from Claw Mastery" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        mastery <- putInHand p "43005"
        payOne p mastery
        placeMainThreat 6
        before <- mainSchemeThreatNow
        instinct <- putInHand p "43004"
        choose "basic thwart" (isHeroThwart p)
        playWith instinct []
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 5
        powers p `shouldReturn` (2, 3)

    it "does not offer its interrupt for a basic attack" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        card <- putInHand p "43004"
        choose "basic attack" (isHeroAttack p)
        offers (isPlay card) `shouldReturn` False
        chooseVillain
        pass

    it "grants THW only, so an Assault thwart still uses unmodified ATK" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        scheme <- putIntoPlay p "43018"
        let sid = SideSchemeId $ unCardId $ pcCardId scheme
        before <- sideSchemeThreat sid
        card <- putInHand p "43004"
        choose "basic thwart" (isHeroThwart p)
        playWith card []
        choose "assault scheme" (isTargetChoice sid)
        pass
        sideSchemeThreat sid `shouldReturn` before - 1

  describe "Claw Mastery" do
    it "raises ATK by two for the round and enforces max one across copies" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        card <- putInHand p "43005"
        payOne p card
        powers p `shouldReturn` (2, 3)
        again <- putInHand p "43005"
        _ <- putInHand p "01090"
        offers (isPlay again) `shouldReturn` False
        runNow [EndRound]
        powers p `shouldReturn` (2, 1)

    it "gains and loses overkill as Honey Badger enters and leaves play" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        card <- putInHand p "43005"
        payOne p card
        mods <- getModifiers (IdentityRef p)
        KeywordModifier Overkill `elem` mods `shouldBe` False
        gabby <- putIntoPlay p "43003"
        mods' <- getModifiers (IdentityRef p)
        KeywordModifier Overkill `elem` mods' `shouldBe` True
        resolve [RemoveFromPlay $ AllyRef $ allyIdOf gabby]
        mods'' <- getModifiers (IdentityRef p)
        KeywordModifier Overkill `elem` mods'' `shouldBe` False

    it "is unavailable in alter-ego form" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        card <- putInHand p "43005"
        _ <- putInHand p "01090"
        offers (isPlay card) `shouldReturn` False

  describe "Regenerative Longevity" do
    it "heals four damage from the identity in alter-ego form" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        clearHand p
        setDamage p 6
        card <- putInHand p "43006"
        payOne p card
        replicateM_ 4 $ choose "heal identity" (isTargetChoice $ IdentityCharacter p)
        damageOf p `shouldReturn` 2

    it "divides healing between identity and Honey Badger without overhealing" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        clearHand p
        gabby <- putIntoPlay p "43003"
        let aid = allyIdOf gabby
        setAllyDamage aid 1
        setDamage p 3
        card <- putInHand p "43006"
        payOne p card
        choose "heal Gabby" (isTargetChoice $ AllyCharacter aid)
        replicateM_ 3 $ choose "heal identity" (isTargetChoice $ IdentityCharacter p)
        damageOf p `shouldReturn` 0
        allyDamageNow aid `shouldReturn` 0

    it "does not heal unrelated allies" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        clearHand p
        ally <- putIntoPlay p "01083"
        setAllyDamage (allyIdOf ally) 1
        setDamage p 1
        card <- putInHand p "43006"
        payOne p card
        offers (isTargetChoice $ AllyCharacter $ allyIdOf ally) `shouldReturn` False
        choose "heal identity" (isTargetChoice $ IdentityCharacter p)
        damageOf p `shouldReturn` 0
        allyDamageNow (allyIdOf ally) `shouldReturn` 1

  describe "Sisterly Bond" do
    it "adds X-23's ATK to Honey Badger's attack" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        _ <- putIntoPlay p "43003"
        gabby <- allyIn "43003"
        bond <- putInHand p "43007"
        choose "Gabby attacks" (isAllyAttack gabby)
        chooseVillain
        playWith bond []
        pass
        villainDamageNow `shouldReturn` 2

    it "adds X-23's THW to Honey Badger's thwart" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        _ <- putIntoPlay p "43003"
        gabby <- allyIn "43003"
        placeMainThreat 6
        before <- mainSchemeThreatNow
        bond <- putInHand p "43007"
        choose "Gabby thwarts" (isAllyThwart gabby)
        chooseMainScheme
        playWith bond []
        pass
        mainSchemeThreatNow `shouldReturn` before - 3

    it "does not trigger on another ally's attack" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        _ <- putIntoPlay p "01083"
        ally <- allyIn "01083"
        bond <- putInHand p "43007"
        choose "ally attacks" (isAllyAttack ally)
        chooseVillain
        offers (isPlay bond) `shouldReturn` False
        pass

  describe "power and keyword interactions" do
    it "uses X-23's ATK when Honey Badger chooses ATK for a Red House thwart" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        _ <- putIntoPlay p "43003"
        _ <- putIntoPlay p "13016"
        house <- revealNow p Schemes.theRedHouse
        pass
        let sid = sideSchemeOf house
        resolve [SideSchemeMessage sid $ SideSchemePlaceThreat 5]
        before <- sideSchemeThreat sid
        bond <- putInHand p "43007"
        gabby <- allyIn "43003"
        choose "Gabby thwarts" (isAllyThwart gabby)
        choose "Red House" (isTargetChoice sid)
        choose "use ATK" (isLabel "$label.thwartWithAtk")
        playWith bond []
        pass
        sideSchemeThreat sid `shouldReturn` before - 3

    it "adds X-23's THW when Befuddle makes Honey Badger attack with THW" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        _ <- putIntoPlay p "43003"
        minion <- addMinion p Minions.hydraSoldier
        befuddle <- putInHand p "33033"
        playOn befuddle minion []
        bond <- putInHand p "43007"
        gabby <- allyIn "43003"
        choose "Gabby attacks" (isAllyAttack gabby)
        choose "minion" (isTargetChoice minion)
        choose "use THW" (isAbilityOfCard befuddle)
        playWith bond []
        pass
        minionDamageOf minion `shouldReturn` Just 3

    it "gives an attack event overkill while Honey Badger is in play" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        _ <- putIntoPlay p "43003"
        mastery <- putInHand p "43005"
        payOne p mastery
        minion <- addMinion p Minions.hydraSoldier
        attack <- putInHand p "01054"
        double <- putInHand p "01090"
        single <- putInHand p "01091"
        playWith attack [double, single]
        choose "minion" (isTargetChoice minion)
        pass
        minionDamageOf minion `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 1

    it "ends Sisterly Bond if an interrupt removes Honey Badger's attack target" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        asX23 p
        _ <- putIntoPlay p "43003"
        minion <- addMinion p Minions.hydraSoldier
        befuddle <- putInHand p "33033"
        playOn befuddle minion []
        bond <- putInHand p "43007"
        gabby <- allyIn "43003"
        choose "Gabby attacks" (isAllyAttack gabby)
        choose "minion" (isTargetChoice minion)
        playWith bond []
        -- Another interrupt removes the target before the pending attack lands.
        runNow [RemoveFromPlay $ MinionRef minion]
        pass
        current <- allyIn "43003"
        Ally.getModifiedAttack current `shouldReturn` 1
        resolve [AllyMessage (allyId current) ReadiedAlly]
        current' <- allyIn "43003"
        choose "Gabby attacks again" (isAllyAttack current')
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 1
