module Marvel.Rules.MutantGenesisLabeledAbilitiesSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (findIndex)
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Data.Text qualified as T
import Test.Hspec qualified as Hspec
import Marvel.Ally.Types qualified as Ally
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Modifier (Modifier (CannotTakeDamage))
import Marvel.Query
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero you = changeForm you >> pass >> clearHand you

roleStillInPlay :: PlayerCard -> GameAppT Bool
roleStillInPlay card = getsGame (HashMap.member (upgradeOf card) . gameUpgrades)

rolePayment :: IdentityId -> GameAppT [PlayerCard]
rolePayment you = traverse (putInHand you) ["01088","01089"]

-- | Decline earlier response/defender windows until the requested player choice.
reachChoice :: (Choice -> Bool) -> GameAppT ()
reachChoice wanted = go (40 :: Int)
 where
  go 0 = error "requested choice did not appear"
  go n = do
    found <- offers wanted
    unless found do
      open <- asked
      case open of
        Just (_, choices) | any isNoResponse choices -> choose "pass earlier response" isNoResponse >> go (n - 1)
        Just (_, choices) | any isNoDefender choices -> choose "decline defender" isNoDefender >> go (n - 1)
        Just (_, choices) | any (isLabel "$label.declineToDefend") choices -> choose "decline another defender" (isLabel "$label.declineToDefend") >> go (n - 1)
        _ -> error $ "unexpected question before requested choice: " <> show open

spec :: Spec
spec = describe "Mutant Genesis labeled abilities" do
  it "pays Ferocious Attack's costs while stunned, cancels the entire effect, and retains the consumable upgrade" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      role <- putIntoPlay p "32179"
      money <- rolePayment p
      resolve [IdentityMessage p IdentityStunned,IdentityMessage p ExhaustedIdentity]
      choose "Ferocious Attack" (isAbilityOfCard role)
      payWith money
      pass
      isStunned p `shouldReturn` False
      villainDamageNow `shouldReturn` 0
      exhaustedNow p `shouldReturn` True
      roleStillInPlay role `shouldReturn` True
      handOf p `shouldReturn` []

  it "pays Heroic Intervention's costs while confused but neither removes threat nor grants tough nor consumes the upgrade" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      role <- putIntoPlay p "32188"
      money <- rolePayment p
      placeMainThreat 6
      resolve [IdentityMessage p IdentityConfused]
      choose "Heroic Intervention" (isAbilityOfCard role)
      payWith money
      pass
      isConfused p `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` 6
      (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False
      roleStillInPlay role `shouldReturn` True

  it "a stunned Brazen Defense cancels both its attack and defense and keeps the card" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      role <- putIntoPlay p "32178"
      money <- putInHand p "01088"
      resolve [IdentityMessage p IdentityStunned]
      _ <- stackEncounterDeck [Treacheries.imTough]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      reachChoice (isAbilityOfCard role)
      choose "Brazen Defense" (isAbilityOfCard role)
      payWith [money]
      pass
      isStunned p `shouldReturn` False
      villainDamageNow `shouldReturn` 0
      damageOf p `shouldReturn` 2
      roleStillInPlay role `shouldReturn` True

  it "Determined Defense triggers when a defense event begins, while Swagger stays basic-only" do
    tg <- testGame ["shadowcat"]
    let p = player1 tg
    inGame tg do
      asHero p
      determined <- putIntoPlay p "32189"
      swagger <- putIntoPlay p "32177"
      quick <- putInHand p "32040"
      money <- putInHand p "01088"
      placeMainThreat 5
      _ <- stackEncounterDeck [Treacheries.imTough]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      reachChoice (isPlay quick)
      playWith quick []
      offers (isAbilityOfCard swagger) `shouldReturn` False
      offers (isAbilityOfCard determined) `shouldReturn` True
      choose "Determined Defense" (isAbilityOfCard determined)
      payWith [money]
      pass
      sid <- mainSchemeIdNow
      choose "remove threat from the main scheme" (isTargetChoice sid)
      pass
      damageOf p `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` 3
      roleStillInPlay determined `shouldReturn` False
      roleStillInPlay swagger `shouldReturn` True

  it "Brazen Defense makes Phased Shadowcat a defending identity before its effects resolve" do
    tg <- testGame ["shadowcat"]
    let p = player1 tg
    inGame tg do
      -- Phase Control is an ordinary alter-ego action; changing identity leaves
      -- the selected mass form faceup.
      choose "Phase Control" (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 2; _ -> False)
      asHero p
      brazen <- putIntoPlay p "32178"
      marker <- putIntoPlay p "32189"
      money <- putInHand p "01088"
      _ <- putInHand p "01089"
      _ <- stackEncounterDeck [Treacheries.imTough]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      reachChoice (isAbilityOfCard brazen)
      choose "Brazen Defense" (isAbilityOfCard brazen)
      payWith [money]
      -- The generic when-defending window precedes the ability's damage. The
      -- marker exposes that window without triggering another defense.
      offers (isAbilityOfCard marker) `shouldReturn` True
      isIdentityDefending p `shouldReturn` True
      getModifiers (IdentityRef p) >>= (`shouldSatisfy` elem CannotTakeDamage)
      roleStillInPlay brazen `shouldReturn` True

  for_ [("32183",301),("32193",300)] \(code,power) ->
    it ("protects another player's ally, including an ally which enters after the effect: " <> show code) do
      tg <- testGame ["spider-man","captain-marvel"]
      let (p1,p2) = (player1 tg,player2 tg)
      inGame tg do
        asHero p1
        role <- putIntoPlay p1 code
        choose "campaign role" (isAbilityOfCard role)
        pass
        cat <- putIntoPlay p2 "01002"
        placeMainThreat 4
        endTurn
        choose "other player's ally basic power" (\case UseAbility ab -> abilitySource ab == toSource (allyIdOf cat) && abilityIndex ab == power; _ -> False)
        if power == 301 then chooseVillain else mainSchemeIdNow >>= \sid -> choose "main scheme" (isTargetChoice sid)
        pass
        allies <- getsGame (HashMap.lookup (allyIdOf cat) . gameAllies)
        fmap (Ally.allyDamage . toAttrs) allies `shouldBe` Just 0

  it "does not offer helmet-protected Magneto as a confused target" do
    tg <- testGameAgainst "32138" ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p Attachments.mgMagnetosHelmet
      pass
      vid <- villainIdNow
      selectAny (EnemyWithId (EnemyVillainId vid) <> EnemyCanBeConfused) `shouldReturn` False

  describe "Armor Up activation timing" do
    for_ [("explicit attack",False),("explicit scheme",True)] \(label,scheming) ->
      it (label <> " offers Armor Up once and preserves the prescribed activation type after the flip") do
        tg <- testGame ["colossus"]
        let p = player1 tg
        inGame tg do
          clearHand p
          armor <- putInHand p "32010"
          _ <- stackEncounterDeck [Treacheries.imTough]
          vid <- villainIdNow
          before <- mainSchemeThreatNow
          liftIO $ writeIORef (ran tg) []
          start [VillainMessage vid $ if scheming then VillainSchemesAgainst p else VillainAttacks p]
          offers (isPlay armor) `shouldReturn` True
          playWith armor []
          choose "Steel Skin" (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1; _ -> False)
          pass
          (.playerIdentitySide) <$> playerAttrs p `shouldReturn` A
          mainSchemeThreatNow `shouldReturn` (before + if scheming then 1 else 0)
          (.playerIdentityTough) <$> playerAttrs p `shouldReturn` scheming
          trace <- liftIO $ readIORef (ran tg)
          liftIO $ length (filter isActivationWould trace) `Hspec.shouldBe` 1

    it "standard activation offers Armor Up once and chooses an attack after the flip" do
      tg <- testGame ["colossus"]
      let p = player1 tg
      inGame tg do
        clearHand p
        armor <- putInHand p "32010"
        _ <- stackEncounterDeck [Treacheries.imTough]
        before <- mainSchemeThreatNow
        liftIO $ writeIORef (ran tg) []
        start [IdentityMessage p VillainAndMinionsActivate]
        playWith armor []
        choose "Steel Skin" (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1; _ -> False)
        pass
        mainSchemeThreatNow `shouldReturn` before
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False
        trace <- liftIO $ readIORef (ran tg)
        liftIO $ length (filter isActivationWould trace) `Hspec.shouldBe` 1

  describe "activations initiated during another activation" do
    it "Master Mold finishes its prescribed scheme before an entering Sentinel's Quickstrike" do
      tg <- testGameAgainst "32109" ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- stackEncounterDeck [Minions.mgSentinelMarkVi]
        vid <- villainIdNow
        liftIO $ writeIORef (ran tg) []
        runNow [VillainMessage vid (VillainSchemesAgainst p)]
        settleRules
        trace <- reverse <$> liftIO (readIORef $ ran tg)
        let scheme = findIndex (T.isPrefixOf $ "VillainMessage " <> tshow vid <> " VillainSchemed") trace
            quickstrike = findIndex (\msg -> T.isPrefixOf "MinionMessage " msg && " (MinionAttacks " `T.isInfixOf` msg) trace
        liftIO $ (scheme,quickstrike) `Hspec.shouldSatisfy` (\case (Just x,Just y) -> x < y; _ -> False)

    it "Fabian's replacement waits for the original villain attack and its responses before Teamwork" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        fabian <- addMinion p Minions.mgFabianCortez
        _ <- addMinion p Minions.mgAmeliaVoght
        setDamage p 0
        resolve [MinionMessage fabian (MinionDamaged (IdentityRef p) $ toDamage 3 FromAbility)]
        barrier <- putIntoPlay p "05017"
        _ <- stackEncounterDeck [Treacheries.imTough,Minions.mgSenyaka]
        vid <- villainIdNow
        liftIO $ writeIORef (ran tg) []
        runNow [VillainMessage vid (VillainAttacks p)]
        reachChoice isNoDefender
        choose "take original attack" isNoDefender
        reachChoice (isAbilityOfCard barrier)
        choose "reflect one damage" (isAbilityOfCard barrier)
        choose "defeat Fabian" (isTargetChoice fabian)
        settleRules
        trace <- reverse <$> liftIO (readIORef $ ran tg)
        let finished = findIndex (T.isPrefixOf "CheckWindows [Window {windowTiming = After, windowType = EnemyActivation (EnemyVillainId") trace
            followup = findIndex (\msg -> T.isPrefixOf "MinionMessage " msg && " (MinionAttacks " `T.isInfixOf` msg) trace
        liftIO $ (finished,followup) `Hspec.shouldSatisfy` (\case (Just x,Just y) -> x < y; _ -> False)

    it "batches two granted Quickstrikes after the current attack for first-player ordering" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- revealNow p Schemes.mgTheBrotherhood
        pass
        first <- encounterCard Minions.mgBlob
        second <- encounterCard Minions.mgToad
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacksWithoutBoost p)]
        reachChoice isNoDefender
        liftIO $ writeIORef (ran tg) []
        runNow [PutEncounterCardIntoPlay p first,PutEncounterCardIntoPlay p second]
        choose "finish original villain attack" isNoDefender
        let secondId = MinionId $ unCardId $ toCardId second
        choose "resolve Toad first" (isTargetChoice secondId)
        settleRules
        trace <- reverse <$> liftIO (readIORef $ ran tg)
        let started = filter (\msg -> T.isPrefixOf "MinionMessage " msg && " (MinionAttacks " `T.isInfixOf` msg) trace
        liftIO $ headMay started `Hspec.shouldSatisfy` maybe False (T.isPrefixOf $ "MinionMessage " <> tshow secondId <> " (MinionAttacks ")

-- Answer forced effects and decline optional responses until ordinary play resumes.
settleRules :: GameAppT ()
settleRules = go (120 :: Int)
 where
  go 0 = error "timing test did not return to ordinary play"
  go n = openQuestion >>= \case
    Nothing -> pure ()
    Just (_,QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you,_) -> answerQuestion you 0 >> go (n - 1)

isActivationWould :: Text -> Bool
isActivationWould = T.isPrefixOf "CheckWindows [Window {windowTiming = Would, windowType = VillainActivationWouldBegin"
