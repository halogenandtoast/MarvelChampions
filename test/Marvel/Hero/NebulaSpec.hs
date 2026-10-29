module Marvel.Hero.NebulaSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Marvel.Scenario.Types (Field (ScenarioEncounterDeck))
import Marvel.Attack (attack)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity)
import Marvel.Identity.Types (PlayerIdentitySide (..))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

alliesOf :: IdentityId -> GameAppT [AllyId]
alliesOf p = selectList (AllyControlledBy $ IdentityWithId p)

supportsOf :: IdentityId -> GameAppT [Attrs Support]
supportsOf p = getsGame (filter ((== p) . supportController) . map toAttrs . HashMap.elems . gameSupports)

encounterDeckNow :: GameAppT [EncounterCard]
encounterDeckNow = getsGame (field ScenarioEncounterDeck . gameScenario)

inHero :: IdentityId -> GameAppT ()
inHero p = changeForm p >> pass >> clearHand p

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code resources = do
  card <- putInHand p code
  payment <- traverse (putInHand p) resources
  playWith card payment
  pure card

beginProtocols :: IdentityId -> GameAppT ()
beginProtocols p = do
  runNow [IdentityMessage p BeginTurn]
  choose "Combat Protocols" (isNamedAbility "$label.combatProtocols")

isGamoraResponse :: PlayerCard -> Choice -> Bool
isGamoraResponse card choice = isAbilityOfCard card choice && case choice of
  UseAbility a -> abilityIndex a == 1
  _ -> False

-- | Gamora is played from hand, and her response activates the chosen Special.
gamoraSpecial :: IdentityId -> PlayerCard -> GameAppT ()
gamoraSpecial p technique = do
  gamora <- playPaying p "22002" ["01088", "01090"]
  choose "Gamora's response" (isGamoraResponse gamora)
  choose "the Technique" (isTargetChoice $ upgradeIdOf technique)

hasUpgrade :: IdentityId -> PlayerCard -> GameAppT Bool
hasUpgrade p c = selectAny (UpgradeWithId (upgradeIdOf c) <> UpgradeControlledBy (IdentityWithId p))

heroStats :: IdentityId -> GameAppT (Natural, Natural, Natural)
heroStats p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> (,,) <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
  _ -> pure (0, 0, 0)

spec :: Spec
spec = describe "Nebula" do
  describe "identity and Combat Protocols" do
    it "has the printed hero and alter-ego statistics" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        handSizeOf p `shouldReturn` 6
        hitPointsOf p `shouldReturn` 9
        inHero p
        heroStats p `shouldReturn` (2, 2, 2)
        handSizeOf p `shouldReturn` 5

    it "resolves Specials in the chosen order and discards them only after the sequence" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        cutthroat <- putIntoPlay p "22004"
        weapons <- putIntoPlay p "22007"
        m <- addMinion p Minions.hydraMercenary
        placeMainThreat 3
        beginProtocols p
        choose "Cutthroat Ambition first" (isTargetChoice $ upgradeIdOf cutthroat)
        chooseMainScheme
        hasUpgrade p cutthroat `shouldReturn` True
        choose "Weapons Master next" (isTargetChoice $ upgradeIdOf weapons)
        choose "attack the mercenary" (isTargetChoice m)
        -- Its three hit points leave one overkill damage while Cutthroat remains.
        villainDamageNow `shouldReturn` 1
        hasUpgrade p cutthroat `shouldReturn` False
        hasUpgrade p weapons `shouldReturn` False
        discarded <- map toCardCode <$> discardOf p
        liftIO $ all (`elem` discarded) ["22004", "22007"] `shouldBe` True

    it "keeps a Technique whose Special has no eligible threat target" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        cutthroat <- putIntoPlay p "22004"
        beginProtocols p
        choose "Cutthroat Ambition" (isTargetChoice $ upgradeIdOf cutthroat)
        hasUpgrade p cutthroat `shouldReturn` True

    it "keeps Unyielding Persistence when Nebula is already tough" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        persistence <- putIntoPlay p "22006"
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        beginProtocols p
        choose "Unyielding Persistence" (isTargetChoice $ upgradeIdOf persistence)
        hasUpgrade p persistence `shouldReturn` True
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True

    it "does not trigger Combat Protocols in alter-ego form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        persistence <- putIntoPlay p "22006"
        runNow [IdentityMessage p BeginTurn]
        offers (isNamedAbility "$label.combatProtocols") `shouldReturn` False
        hasUpgrade p persistence `shouldReturn` True

    it "does not resolve another player's Techniques" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        inHero p
        ours <- putIntoPlay p "22006"
        theirs <- putIntoPlay other "22007"
        beginProtocols p
        offers (isTargetChoice $ upgradeIdOf theirs) `shouldReturn` False
        choose "our Technique" (isTargetChoice $ upgradeIdOf ours)
        hasUpgrade other theirs `shouldReturn` True

  describe "Cybernetic Upgrades" do
    it "can draw after Combat Protocols resolved earlier that round and Nebula flips to alter ego" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        runNow [IdentityMessage p $ ChangedToForm A]
        persistence <- putIntoPlay p "22006"
        beginProtocols p
        choose "resolve Persistence" (isTargetChoice $ upgradeIdOf persistence)
        changeForm p
        pass
        clearHand p
        _ <- playPaying p "22005" ["01090"]
        offers (isNamedAbility "$label.cyberneticUpgrades") `shouldReturn` True
        choose "Cybernetic Upgrades remains available" (isNamedAbility "$label.cyberneticUpgrades")
        length <$> handOf p `shouldReturn` 2

    it "draws two cards after playing a Technique, once per round" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- playPaying p "22006" ["01090"]
        choose "Cybernetic Upgrades" (isNamedAbility "$label.cyberneticUpgrades")
        length <$> handOf p `shouldReturn` 2
        _ <- playPaying p "22005" ["01090"]
        offers (isNamedAbility "$label.cyberneticUpgrades") `shouldReturn` False
        length <$> handOf p `shouldReturn` 2

    it "can decline the draw without spending its once-per-round limit" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- playPaying p "22006" ["01090"]
        choose "no response" isNoResponse
        length <$> handOf p `shouldReturn` 0
        _ <- playPaying p "22005" ["01090"]
        offers (isNamedAbility "$label.cyberneticUpgrades") `shouldReturn` True

    it "refreshes its draw limit in the next round" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- playPaying p "22006" ["01090"]
        choose "Cybernetic Upgrades" (isNamedAbility "$label.cyberneticUpgrades")
        resolve [EndRound]
        _ <- playPaying p "22005" ["01090"]
        offers (isNamedAbility "$label.cyberneticUpgrades") `shouldReturn` True
        choose "Cybernetic Upgrades again" (isNamedAbility "$label.cyberneticUpgrades")
        length <$> handOf p `shouldReturn` 4

    it "does not trigger when a Technique is put into play rather than played" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "22006"
        runNow [PutCardIntoPlay p card NoPayment Nothing False]
        offers (isNamedAbility "$label.cyberneticUpgrades") `shouldReturn` False
        length <$> handOf p `shouldReturn` 0

    it "does not trigger for a non-Technique or in hero form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- playPaying p "22003" ["01090"]
        offers (isNamedAbility "$label.cyberneticUpgrades") `shouldReturn` False
        inHero p
        _ <- playPaying p "22006" ["01090"]
        offers (isNamedAbility "$label.cyberneticUpgrades") `shouldReturn` False

  describe "Gamora" do
    it "resolves a controlled Technique once without discarding it" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        technique <- putIntoPlay p "22006"
        clearHand p
        gamoraSpecial p technique
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
        hasUpgrade p technique `shouldReturn` True
        length <$> alliesOf p `shouldReturn` 1

    it "does not offer a response without a Technique under your control" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        gamora <- playPaying p "22002" ["01088", "01090"]
        offers (isGamoraResponse gamora) `shouldReturn` False

    it "does not offer her response when every controlled Special is inert" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        technique <- putIntoPlay p "22006"
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        clearHand p
        gamora <- playPaying p "22002" ["01088", "01090"]
        offers (isGamoraResponse gamora) `shouldReturn` False
        hasUpgrade p technique `shouldReturn` True
        selectList UpgradeWithResolvableSpecial `shouldReturn` []

    it "can resolve a confusion cancellation even when Cutthroat has no threat target" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        technique <- putIntoPlay p "22004"
        resolve [GiveStatus (IdentityRef p) ConfusedStatus]
        clearHand p
        gamoraSpecial p technique
        isConfused p `shouldReturn` False
        hasUpgrade p technique `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` 0

    it "can resolve a stun cancellation instead of Weapons Master's damage" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        technique <- putIntoPlay p "22007"
        resolve [GiveStatus (IdentityRef p) StunnedStatus]
        clearHand p
        gamoraSpecial p technique
        isStunned p `shouldReturn` False
        villainDamageNow `shouldReturn` 0
        hasUpgrade p technique `shouldReturn` True

    it "does not consider ordinary upgrades to have a resolvable Special" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "01065"
        selectList UpgradeWithResolvableSpecial `shouldReturn` []

    it "does not offer her response when merely put into play" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22006"
        gamora <- putInHand p "22002"
        runNow [PutCardIntoPlay p gamora NoPayment Nothing False]
        offers (isGamoraResponse gamora) `shouldReturn` False
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False

    it "does not offer another player's Technique as her target" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        ours <- putIntoPlay p "22006"
        theirs <- putIntoPlay other "22007"
        gamora <- playPaying p "22002" ["01088", "01090"]
        choose "Gamora" (isGamoraResponse gamora)
        offers (isTargetChoice $ upgradeIdOf theirs) `shouldReturn` False
        choose "our Technique" (isTargetChoice $ upgradeIdOf ours)

  describe "Nebula's Ship" do
    it "pays a wild resource in alter-ego form and exhausts" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        ship <- putIntoPlay p "22003"
        clearHand p
        technique <- putInHand p "22006"
        choose "play the Technique" (isPlay technique)
        choose "Ship generates a resource" (isAbilityOfCard ship)
        whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
        pass
        hasUpgrade p technique `shouldReturn` True
        attrs <- supportsOf p
        liftIO $ any (\a -> supportId a == supportIdOf ship && supportExhausted a) attrs `shouldBe` True

    it "pays a wild resource in hero form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        ship <- putIntoPlay p "22003"
        clearHand p
        technique <- putInHand p "22006"
        choose "play the Technique" (isPlay technique)
        choose "Ship generates a resource" (isAbilityOfCard ship)
        whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
        hasUpgrade p technique `shouldReturn` True
        offers (isAbilityOfCard ship) `shouldReturn` False

  describe "Cutthroat Ambition" do
    it "thwarts three threat with its Special" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        technique <- putIntoPlay p "22004"
        placeMainThreat 5
        before <- mainSchemeThreatNow
        gamoraSpecial p technique
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` before - 3
        hasUpgrade p technique `shouldReturn` True

    it "confusion replaces its thwart Special and keeps the Technique during Protocols" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        technique <- putIntoPlay p "22004"
        placeMainThreat 4
        before <- mainSchemeThreatNow
        resolve [GiveStatus (IdentityRef p) ConfusedStatus]
        beginProtocols p
        choose "Cutthroat" (isTargetChoice $ upgradeIdOf technique)
        mainSchemeThreatNow `shouldReturn` before
        isConfused p `shouldReturn` False
        hasUpgrade p technique `shouldReturn` True

    it "grants piercing and overkill to a Special attack in hero form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "22004"
        weapon <- putIntoPlay p "22007"
        m <- addMinion p Minions.hydraMercenary
        resolve [GiveStatus (MinionRef m) ToughStatus]
        gamoraSpecial p weapon
        choose "the tough mercenary" (isTargetChoice m)
        villainDamageNow `shouldReturn` 1
        selectAny (MinionWithId m) `shouldReturn` False

    it "does not grant its passive attack keywords in alter-ego form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22004"
        mods <- getModifiers p
        liftIO $ KeywordModifier Piercing `elem` mods `shouldBe` False
        liftIO $ KeywordModifier Overkill `elem` mods `shouldBe` False

  describe "Evasive Maneuvering" do
    it "ignores guard, patrol and crisis only in hero form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22005"
        _ <- addMinion p Minions.hydraMercenary
        _ <- addMinion p Minions.headhuntersHenchman
        _ <- revealNow p SideSchemes.blockade
        placeMainThreat 1
        vid <- villainIdNow
        mid <- mainSchemeIdNow
        before <- getModifiers p
        liftIO $ all (`notElem` before) [IgnoresGuard, IgnoresPatrol, IgnoresCrisis] `shouldBe` True
        inHero p
        enemies <- selectList (AttackableBy $ IdentityWithId p)
        schemes <- selectList SchemeYouCanThwart
        liftIO $ EnemyVillainId vid `elem` enemies `shouldBe` True
        liftIO $ SchemeMainSchemeId mid `elem` schemes `shouldBe` True

    it "lets Surveillance Team remove main-scheme threat while Evasive ignores a crisis icon" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "22005"
        team <- putIntoPlay p "01064"
        crisis <- sideSchemeOf <$> revealNow p SideSchemes.blockade
        pass
        placeMainThreat 3
        before <- mainSchemeThreatNow
        sideBefore <- sideSchemeThreat crisis
        main <- mainSchemeIdNow
        choose "Surveillance Team" (isAbilityOf $ supportIdOf team)
        offers (isTargetChoice main) `shouldReturn` True
        choose "remove main threat" (isTargetChoice main)
        pass
        mainSchemeThreatNow `shouldReturn` (before - 1)
        sideSchemeThreat crisis `shouldReturn` sideBefore
        selectList CrisisSideScheme >>= (`shouldSatisfy` elem crisis)

    for_ [(False, True), (True, False)] \(hero, evasive) ->
      it (if hero then "keeps Surveillance Team off the main scheme without Evasive" else "keeps Surveillance Team off the main scheme while Nebula is alter-ego") do
        tg <- testGame ["nebula"]
        let p = player1 tg
        inGame tg do
          when hero $ inHero p
          when evasive $ void $ putIntoPlay p "22005"
          team <- putIntoPlay p "01064"
          crisis <- sideSchemeOf <$> revealNow p SideSchemes.blockade
          pass
          placeMainThreat 3
          before <- mainSchemeThreatNow
          main <- mainSchemeIdNow
          choose "Surveillance Team" (isAbilityOf $ supportIdOf team)
          offers (isTargetChoice main) `shouldReturn` False
          choose "the crisis scheme" (isTargetChoice crisis)
          pass
          mainSchemeThreatNow `shouldReturn` before

    for_ [("stun", StunnedEnemy), ("confuse", ConfusedEnemy)] \(mode, status) ->
      it ("can " <> mode <> " an enemy with its Special") do
        tg <- testGame ["nebula"]
        let p = player1 tg
        inGame tg do
          technique <- putIntoPlay p "22005"
          gamoraSpecial p technique
          choose "status choice" (isLabel $ "$label." <> T.pack mode)
          chooseVillain
          vid <- villainIdNow
          selectAny (EnemyWithId (EnemyVillainId vid) <> status) `shouldReturn` True
          hasUpgrade p technique `shouldReturn` True

    it "keeps Evasive Maneuvering when no enemy can receive either status" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        technique <- putIntoPlay p "22005"
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) StunnedStatus, GiveStatus (VillainRef vid) ConfusedStatus]
        beginProtocols p
        choose "Evasive Maneuvering" (isTargetChoice $ upgradeIdOf technique)
        hasUpgrade p technique `shouldReturn` True

  describe "Unyielding Persistence" do
    it "raises hero THW and ATK and prevents stun and confusion through stalwart" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "22006"
        heroStats p `shouldReturn` (3, 3, 2)
        resolve [GiveStatus (IdentityRef p) StunnedStatus, GiveStatus (IdentityRef p) ConfusedStatus]
        isStunned p `shouldReturn` False
        isConfused p `shouldReturn` False

    it "has no passive bonus or stalwart in alter-ego form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22006"
        mods <- getModifiers p
        liftIO $ all (`notElem` mods) [ThwartModifier 1, AttackModifier 1, KeywordModifier Stalwart] `shouldBe` True
        resolve [GiveStatus (IdentityRef p) ConfusedStatus]
        isConfused p `shouldReturn` True

    it "gives tough through its Special, then is discarded by Protocols" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        technique <- putIntoPlay p "22006"
        beginProtocols p
        choose "Unyielding Persistence" (isTargetChoice $ upgradeIdOf technique)
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
        hasUpgrade p technique `shouldReturn` False

  describe "Weapons Master" do
    it "deals four damage with its Special" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        technique <- putIntoPlay p "22007"
        gamoraSpecial p technique
        chooseVillain
        villainDamageNow `shouldReturn` 4
        hasUpgrade p technique `shouldReturn` True

    it "a stun replaces its attack Special and the Technique is kept" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        technique <- putIntoPlay p "22007"
        resolve [GiveStatus (IdentityRef p) StunnedStatus]
        beginProtocols p
        choose "Weapons Master" (isTargetChoice $ upgradeIdOf technique)
        villainDamageNow `shouldReturn` 0
        isStunned p `shouldReturn` False
        hasUpgrade p technique `shouldReturn` True

    it "grants retaliate in hero form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "22007"
        vid <- villainIdNow
        resolve [IdentityMessage p $ IdentityWasAttacked $ attack (EnemyVillainId vid) (IdentityCharacter p) 1]
        villainDamageNow `shouldReturn` 1

    it "does not grant retaliate in alter-ego form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22007"
        vid <- villainIdNow
        resolve [IdentityMessage p $ IdentityWasAttacked $ attack (EnemyVillainId vid) (IdentityCharacter p) 1]
        villainDamageNow `shouldReturn` 0

  describe "Wide Stance" do
    it "reduces each hero attack by one and leaves other damage unchanged" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "22008"
        vid <- villainIdNow
        resolve [IdentityMessage p $ IdentityWasAttacked $ attack (EnemyVillainId vid) (IdentityCharacter p) 2]
        damageOf p `shouldReturn` 1
        resolve [IdentityMessage p $ IdentityDamaged GameRef (toDamage 2 FromAbility)]
        damageOf p `shouldReturn` 3

    it "stacks the reduction from both Wide Stance upgrades" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "22008"
        _ <- putIntoPlay p "22008"
        vid <- villainIdNow
        resolve [IdentityMessage p $ IdentityWasAttacked $ attack (EnemyVillainId vid) (IdentityCharacter p) 2]
        damageOf p `shouldReturn` 0

    it "does not reduce attacks against alter-ego Nebula" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22008"
        vid <- villainIdNow
        resolve [IdentityMessage p $ IdentityWasAttacked $ attack (EnemyVillainId vid) (IdentityCharacter p) 2]
        damageOf p `shouldReturn` 2

    it "looks at three, discards the chosen card and returns the others in chosen order" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        technique <- putIntoPlay p "22008"
        cards <- stackEncounterDeck [Treacheries.assault, Treacheries.advance, Treacheries.gangUp]
        gamoraSpecial p technique
        let [a, b, c] = cards
        choose "discard the middle card" (isTargetChoice b)
        choose "return the last card first" (isTargetChoice c)
        deck <- encounterDeckNow
        liftIO $ take 2 deck `shouldBe` [c, a]
        discard <- encounterDiscardNow
        liftIO $ b `elem` discard `shouldBe` True
        hasUpgrade p technique `shouldReturn` True

    it "handles a one-card encounter deck without an empty ordering question" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        technique <- putIntoPlay p "22008"
        (cards, _) <- setEncounterDeck [Treacheries.assault] [Treacheries.advance]
        gamoraSpecial p technique
        choose "discard the only card" (isTargetChoice $ head cards)
        offers (isTargetChoice $ upgradeIdOf technique) `shouldReturn` False
        hasUpgrade p technique `shouldReturn` True
