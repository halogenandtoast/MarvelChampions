module Marvel.Hero.DeadpoolSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.AlterEgo.Types qualified as AlterEgo
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.MainScheme.Types qualified as MainScheme
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Modifier
import Marvel.Resource (resourcesFor)
import Marvel.Status
import Marvel.Stats
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Window qualified as W
import TestImport

newtype DeadpoolTestModifiers = DeadpoolTestModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage DeadpoolTestModifiers where
  runMessage msg (DeadpoolTestModifiers attrs) = DeadpoolTestModifiers <$> runMessage msg attrs

modifyFor :: EntityMatcher -> [Modifier] -> GameAppT ()
modifyFor matcher modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (DeadpoolTestModifiers EffectAttrs
    { effectId = eid, effectCardCode = "44001a", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = modifiers, effectEnds = Nothing })

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card index = \case
  UseAbility ability -> isAbilityOfCard card (UseAbility ability) && abilityIndex ability == index
  _ -> False

identityAbility :: IdentityId -> Natural -> Choice -> Bool
identityAbility you index = \case
  UseAbility ability -> abilitySource ability == IdentityRef you && abilityIndex ability == index
  _ -> False

collectionChoice :: CardCode -> Choice -> Bool
collectionChoice code = \case
  CardLabel other _ -> code == other
  _ -> False

asHero :: IdentityId -> GameAppT ()
asHero you = clearHand you >> changeForm you >> pass

upgradeAttrsOf :: PlayerCard -> GameAppT (Attrs Upgrade.Upgrade)
upgradeAttrsOf card = getsGame $ maybe (error "missing upgrade") toAttrs . HashMap.lookup (upgradeIdOf card) . gameUpgrades

heldWeapons :: PlayerCard -> GameAppT [PlayerCard]
heldWeapons card = Upgrade.upgradeHeldCards <$> upgradeAttrsOf card

allyStats :: PlayerCard -> GameAppT (Natural, Natural)
allyStats card = do
  attrs <- getsGame $ maybe (error "missing ally") toAttrs . HashMap.lookup (allyIdOf card) . gameAllies
  (,) <$> Ally.getModifiedThwart attrs <*> Ally.getModifiedAttack attrs

thresholdNow :: GameAppT Natural
thresholdNow = do
  scheme <- getsGame $ fromMaybe (error "missing scheme") . headMay . HashMap.elems . gameMainSchemes
  MainScheme.getThreshold (toAttrs scheme)

playArmed :: IdentityId -> CardCode -> GameAppT PlayerCard
playArmed you weapon = do
  armed <- putInHand you "44009"
  payment <- putInHand you "01088"
  playWith armed [payment]
  choose "search the collection" (cardAbility armed 1)
  choose "a weapon from the collection" (collectionChoice weapon)
  pass
  pure armed

spec :: Spec
spec = describe "Deadpool hero" do
  describe "Deadpool and Wade Wilson" do
    it "has the printed stats on both identity faces" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        hitPointsOf you `shouldReturn` 9
        handSizeOf you `shouldReturn` 6
        currentIdentity <$> getPlayer you >>= \case
          AlterEgoSide a -> AlterEgo.alterEgoBaseRecovery (toAttrs a) `shouldBe` Rec 8
          _ -> error "expected Wade Wilson"
        asHero you
        handSizeOf you `shouldReturn` 5
        currentIdentity <$> getPlayer you >>= \case
          HeroSide a -> do
            Hero.getModifiedThwart (toAttrs a) `shouldReturn` 2
            Hero.getModifiedAttack (toAttrs a) `shouldReturn` 2
            Hero.getModifiedDefense (toAttrs a) `shouldReturn` 1
          _ -> error "expected Deadpool"

    it "replaces lethal damage with one hit point, alter-ego form and an acceleration token" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        runNow (damageIdentity you GameRef $ toDamage 9 FromAbility)
        choose "The Regeneratin' Degenerate" (identityAbility you 1)
        pass
        damageOf you `shouldReturn` 8
        toCardCode <$> getPlayer you `shouldReturn` "44001b"
        getMainSchemeTokenCount `shouldReturn` 1
        getsGame (HashMap.member you . gamePlayers) `shouldReturn` True

    it "also replaces defeat that is not caused by damage" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        runNow [CheckWindows [W.Window W.Would $ W.IdentityDefeat you], IdentityMessage you IdentityDefeated]
        choose "replace defeat" (identityAbility you 1)
        pass
        damageOf you `shouldReturn` 8
        toCardCode <$> getPlayer you `shouldReturn` "44001b"
        getMainSchemeTokenCount `shouldReturn` 1

    it "does not replace Wade's defeat or defeat while Deadpool's text is blank" do
      for_ [False, True] \blanked -> do
        tg <- testGame ["deadpool", "spider-man"]
        inGame tg do
          let you = player1 tg
          clearHand you
          when blanked do
            changeForm you
            modifyFor (IdentityEntity $ IdentityWithId you) [Blank]
          runNow (damageIdentity you GameRef $ toDamage 9 FromAbility)
          offers (identityAbility you 1) `shouldReturn` False
          getMainSchemeTokenCount `shouldReturn` 0

    it "discards a chosen hand card and finds a Deadpool event once per round" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        payment <- putInHand you "01088"
        [wanted, aspectEvent, resource] <- stackDeck you ["44003", "44018", "44007"]
        choose "Break the Fourth Wall" (identityAbility you 1)
        choose "discard the hand card" (isTargetChoice payment)
        offers (isTargetChoice wanted) `shouldReturn` True
        offers (isTargetChoice aspectEvent) `shouldReturn` False
        offers (isTargetChoice resource) `shouldReturn` False
        choose "find Exhausting Personality" (isTargetChoice wanted)
        pass
        handOf you >>= (`shouldSatisfy` elem wanted)
        discardOf you >>= (`shouldSatisfy` elem payment)
        offers (identityAbility you 1) `shouldReturn` False

    it "needs a card to discard and is unavailable in hero form" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        offers (identityAbility you 1) `shouldReturn` False
        asHero you
        _ <- putInHand you "01088"
        offers (isLabeledAbility "$label.breakTheFourthWall") `shouldReturn` False

  describe "Cable" do
    it "gains one THW and ATK per token, to a maximum of three bonus points" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        card <- putInHand you "44002"
        payment <- putInHand you "44025"
        playWith card [payment]
        pass
        allyStats card `shouldReturn` (1, 2)
        scheme <- mainSchemeIdNow
        resolve [PlaceAccelerationToken scheme]
        allyStats card `shouldReturn` (2, 3)
        resolve $ replicate 3 (PlaceAccelerationToken scheme)
        allyStats card `shouldReturn` (4, 5)

    it "does not count acceleration icons and loses his bonus when blanked" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        cable <- putIntoPlay you "44002"
        _ <- putIntoPlay you "44013"
        allyStats cable `shouldReturn` (1, 2)
        scheme <- mainSchemeIdNow
        resolve [PlaceAccelerationToken scheme]
        modifyFor (AllyEntity $ AllyWithId $ allyIdOf cable) [Blank]
        allyStats cable `shouldReturn` (1, 2)

  describe "Chimichanga Truck" do
    it "readies Wade after a basic recovery, exhausting the Truck" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        truck <- putInHand you "44008"
        payment <- putInHand you "01088"
        playWith truck [payment]
        pass
        setDamage you 8
        choose "recover" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
        choose "eat a chimichanga" (isAbilityOfCard truck)
        pass
        damageOf you `shouldReturn` 0
        isExhaustedPlayer you `shouldReturn` False
        supportExhausted <$> getsGame (maybe (error "missing truck") toAttrs . HashMap.lookup (supportIdOf truck) . gameSupports) `shouldReturn` True

    it "can ready another player's identity after that identity recovers" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        truck <- putIntoPlay you "44008"
        resolve [IdentityMessage other ExhaustedIdentity]
        runNow [CheckWindows [W.Window W.After $ W.UsedRecovery other]]
        choose "ready the recovered identity" (isAbilityOfCard truck)
        pass
        isExhaustedPlayer other `shouldReturn` False
        isExhaustedPlayer you `shouldReturn` False

    it "does not respond to other healing and cannot trigger when exhausted" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        truck <- putIntoPlay you "44008"
        setDamage you 3
        runNow [IdentityMessage you (IdentityHealed 1)]
        offers (isAbilityOfCard truck) `shouldReturn` False
        resolve [SupportMessage (supportIdOf truck) ExhaustedSupport]
        runNow [CheckWindows [W.Window W.After $ W.UsedRecovery you]]
        offers (isAbilityOfCard truck) `shouldReturn` False

    it "does not offer to ready an identity that cannot ready" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        truck <- putIntoPlay you "44008"
        resolve [IdentityMessage you ExhaustedIdentity]
        modifyFor (IdentityEntity $ IdentityWithId you) [CannotReady]
        runNow [CheckWindows [W.Window W.After $ W.UsedRecovery you]]
        offers (isAbilityOfCard truck) `shouldReturn` False

  describe "Armed to the Teeth" do
    it "searches the implemented aspect Weapon collection and stores the chosen card facedown" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        armed <- playArmed you "06019"
        map toCardCode <$> heldWeapons armed `shouldReturn` ["06019"]
        Upgrade.upgradeHeldCardsFacedown <$> upgradeAttrsOf armed `shouldReturn` True
        getHeldPlayableCards you `shouldReturn` []
        cards <- heldWeapons armed
        offers (isPlay $ testHead cards) `shouldReturn` False

    it "only searches after being played, and may decline its response" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        armed <- putIntoPlay you "44009"
        heldWeapons armed `shouldReturn` []
        offers (cardAbility armed 1) `shouldReturn` False
        offers (cardAbility armed 2) `shouldReturn` False
        resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf armed]
        another <- putInHand you "44009"
        payment <- putInHand you "01088"
        playWith another [payment]
        pass
        heldWeapons another `shouldReturn` []

    it "offers aspect weapons but excludes basic and identity-specific weapons" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        armed <- putInHand you "44009"
        payment <- putInHand you "01088"
        playWith armed [payment]
        choose "search collection" (cardAbility armed 1)
        offers (collectionChoice "06019") `shouldReturn` True
        offers (collectionChoice "23035") `shouldReturn` False
        offers (collectionChoice "44010") `shouldReturn` False
        choose "take Jarnbjorn" (collectionChoice "06019")

    it "offers only the newly played copy's response when another copy is already in play" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        older <- putIntoPlay you "44009"
        newer <- putInHand you "44009"
        payment <- putInHand you "01088"
        playWith newer [payment]
        offers (cardAbility older 1) `shouldReturn` False
        offers (cardAbility newer 1) `shouldReturn` True
        choose "search with only the new copy" (cardAbility newer 1)
        choose "take Jarnbjorn" (collectionChoice "06019")
        heldWeapons older `shouldReturn` []
        map toCardCode <$> heldWeapons newer `shouldReturn` ["06019"]

    it "swaps a controlled Weapon with the stored card without paying or readying it" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        armed <- playArmed you "06019"
        katana <- putIntoPlay you "44010"
        resolve [UpgradeMessage (upgradeIdOf katana) ExhaustedUpgrade]
        [incoming] <- heldWeapons armed
        choose "swap weapons" (cardAbility armed 2)
        choose "replace Katana" (isTargetChoice $ upgradeIdOf katana)
        pass
        upgradeCodes >>= (`shouldSatisfy` elem "06019")
        upgradeCodes >>= (`shouldSatisfy` notElem "44010")
        Upgrade.upgradeExhausted <$> upgradeAttrsOf incoming `shouldReturn` True
        Upgrade.upgradeExhausted <$> upgradeAttrsOf armed `shouldReturn` True
        map toCardCode <$> heldWeapons armed `shouldReturn` ["44010"]
        discardOf you >>= (`shouldSatisfy` notElem katana)
        offers (cardAbility armed 2) `shouldReturn` False

    it "preserves exhaustion on a swap between two copies of the same weapon" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        armed <- playArmed you "06019"
        outgoing <- putIntoPlay you "06019"
        resolve [UpgradeMessage (upgradeIdOf outgoing) ExhaustedUpgrade]
        [incoming] <- heldWeapons armed
        choose "swap matching weapons" (cardAbility armed 2)
        choose "the in-play Jarnbjorn" (isTargetChoice $ upgradeIdOf outgoing)
        pass
        Upgrade.upgradeExhausted <$> upgradeAttrsOf incoming `shouldReturn` True
        map pcCardId <$> heldWeapons armed `shouldReturn` [pcCardId outgoing]

    it "does not offer a swap with no legal host for the incoming ally Weapon" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        armed <- playArmed you "22032"
        _ <- putIntoPlay you "44010"
        offers (cardAbility armed 2) `shouldReturn` False

  describe "Deadpool's Katana" do
    it "takes one damage, exhausts and attacks for two with piercing" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        katana <- putInHand you "44010"
        payment <- putInHand you "01088"
        playWith katana [payment]
        pass
        villain <- villainIdNow
        resolve [VillainMessage villain VillainBecomeTough]
        choose "attack with Katana" (cardAbility katana 1)
        damageOf you `shouldReturn` 1
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        Upgrade.upgradeExhausted <$> upgradeAttrsOf katana `shouldReturn` True
        offers (cardAbility katana 1) `shouldReturn` False

    it "still pays its costs when stunned but does not attack" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        katana <- putIntoPlay you "44010"
        resolve [GiveStatus (IdentityRef you) StunnedStatus]
        choose "try the Katana attack" (cardAbility katana 1)
        pass
        damageOf you `shouldReturn` 1
        villainDamageNow `shouldReturn` 0
        playerIdentityStunned <$> playerAttrs you `shouldReturn` False
        Upgrade.upgradeExhausted <$> upgradeAttrsOf katana `shouldReturn` True

    it "cannot pay through tough, cannot be used as Wade, and aborts when the cost is prevented" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        katana <- putIntoPlay you "44010"
        offers (cardAbility katana 1) `shouldReturn` False
        changeForm you
        resolve [IdentityMessage you IdentityBecomeTough]
        offers (cardAbility katana 1) `shouldReturn` False
        resolve [RemoveStatus (IdentityRef you) ToughStatus]
        stance <- putIntoPlay you "08032"
        choose "use Katana" (cardAbility katana 1)
        choose "prevent its damage cost" (isAbilityOfCard stance)
        pass
        damageOf you `shouldReturn` 0
        villainDamageNow `shouldReturn` 0

  describe "It Ain't Over..." do
    it "attaches to the main scheme and adds two target threat per token, without counting icons" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        scheme <- mainSchemeIdNow
        card <- putInHand you "44011"
        payment <- putInHand you "01083"
        playWith card [payment]
        pass
        Upgrade.upgradeAttachedMainScheme <$> upgradeAttrsOf card `shouldReturn` Just scheme
        thresholdNow `shouldReturn` 7
        _ <- putIntoPlay you "44013"
        thresholdNow `shouldReturn` 7
        resolve [PlaceAccelerationToken scheme, PlaceAccelerationToken scheme]
        thresholdNow `shouldReturn` 11

    it "stops increasing the threshold when discarded" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        scheme <- mainSchemeIdNow
        card <- putInHand you "44011"
        payment <- putInHand you "01083"
        playWith card [payment]
        pass
        resolve [PlaceAccelerationToken scheme]
        thresholdNow `shouldReturn` 9
        resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf card]
        thresholdNow `shouldReturn` 7

  describe "Montage" do
    it "adds up to three generated wild resources for tokens while keeping a single printed icon" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        montage <- putInHand you "44007"
        resourcesFor montage Nothing `shouldReturn` [Wild]
        _ <- putIntoPlay you "44013"
        resourcesFor montage Nothing `shouldReturn` [Wild]
        scheme <- mainSchemeIdNow
        resolve [PlaceAccelerationToken scheme, PlaceAccelerationToken scheme]
        resourcesFor montage Nothing `shouldReturn` [Wild, Wild, Wild]
        resolve [PlaceAccelerationToken scheme, PlaceAccelerationToken scheme]
        resourcesFor montage Nothing `shouldReturn` [Wild, Wild, Wild, Wild]
        printedResourcesFor montage `shouldReturn` [Wild]
