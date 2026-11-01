module Marvel.Hero.X23Spec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hero.Runner qualified as Hero
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Keyword (Keyword (Retaliate))
import Marvel.Modifier (Modifier (KeywordModifier, CannotReady))
import Marvel.Identity (currentIdentity, PlayerIdentitySide (..))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Query
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Phase
import Marvel.Window qualified as W
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero you = changeForm you >> pass >> clearHand you

attackValue :: IdentityId -> GameAppT Natural
attackValue you = currentIdentity <$> getPlayer you >>= \case
  HeroSide h -> Hero.getModifiedAttack $ toAttrs h
  _ -> pure 0

clawsInPlay :: GameAppT PlayerCard
clawsInPlay = do
  claws <- upgradesWith "43002"
  case claws of
    [attrs] -> pure $ MkPlayerCard (CardId $ unUpgradeId $ upgradeId attrs) (getCardDef attrs) (Just $ upgradeOwner attrs) (Just $ upgradeController attrs)
    _ -> error "expected one X-23's Claws"

honeyResponse :: PlayerCard -> Choice -> Bool
honeyResponse card choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

newtype X23TestModifiers = X23TestModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage X23TestModifiers where
  runMessage msg (X23TestModifiers attrs) = X23TestModifiers <$> runMessage msg attrs

preventReady :: IdentityId -> GameAppT EffectId
preventReady you = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (X23TestModifiers EffectAttrs
    { effectId = eid
    , effectCardCode = "01039"
    , effectSource = GameRef
    , effectMatcher = IdentityEntity $ IdentityWithId you
    , effectModifiers = [CannotReady]
    , effectEnds = Nothing
    })
  pure eid

spec :: Spec
spec = describe "X-23" do
  it "starts with Laura's 10 hit points, REC 6 and permanent Claws already in play" do
    tg <- testGame ["x-23"]
    inGame tg do
      hitPointsOf (player1 tg) `shouldReturn` 10
      handSizeOf (player1 tg) `shouldReturn` 6
      length <$> upgradesWith "43002" `shouldReturn` 1
      setDamage (player1 tg) 7
      choose "basic recovery" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
      damageOf (player1 tg) `shouldReturn` 1

  it "readies X-23 after damage once in each phase" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      runNow [IdentityMessage you ExhaustedIdentity]
      start (damageIdentity you GameRef $ toDamage 1 FromAbility)
      choose "Living Weapon" (isNamedAbility "$label.livingWeapon")
      exhaustedNow you `shouldReturn` False
      runNow [IdentityMessage you ExhaustedIdentity]
      start (damageIdentity you GameRef $ toDamage 1 FromAbility)
      offers (isNamedAbility "$label.livingWeapon") `shouldReturn` False
      pass
      exhaustedNow you `shouldReturn` True
      runNow [BeginPhase VillainPhase]
      start (damageIdentity you GameRef $ toDamage 1 FromAbility)
      choose "Living Weapon next phase" (isNamedAbility "$label.livingWeapon")
      exhaustedNow you `shouldReturn` False

  it "does not ready Laura or trigger Living Weapon for damage prevented by tough" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      runNow [IdentityMessage you ExhaustedIdentity]
      start (damageIdentity you GameRef $ toDamage 1 FromAbility)
      offers (isNamedAbility "$label.livingWeapon") `shouldReturn` False
      pass
      asHero you
      runNow [IdentityMessage you IdentityBecomeTough]
      start (damageIdentity you GameRef $ toDamage 2 FromAbility)
      offers (isNamedAbility "$label.livingWeapon") `shouldReturn` False
      damageOf you `shouldReturn` 1

  it "Laura shuffles Honey Badger from discard as a cost, then draws once per round" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      clearHand you
      honey <- putInHand you "43003"
      runNow [DiscardedCard $ PlayerCard honey]
      before <- length <$> handOf you
      choose "Laura's action" (isNamedAbility "$label.lauraKinneyShuffle")
      choose "shuffle Honey Badger" (isTargetChoice honey)
      length <$> handOf you `shouldReturn` (before + 1)
      map pcCardId <$> discardOf you `shouldReturn` []
      offers (isNamedAbility "$label.lauraKinneyShuffle") `shouldReturn` False

  it "Laura may shuffle Sisterly Bond but no other discarded card" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      clearHand you
      unrelated <- putInHand you "43004"
      runNow [DiscardedCard $ PlayerCard unrelated]
      offers (isNamedAbility "$label.lauraKinneyShuffle") `shouldReturn` False
      bond <- putInHand you "43007"
      runNow [DiscardedCard $ PlayerCard bond]
      choose "Laura's action" (isNamedAbility "$label.lauraKinneyShuffle")
      offers (isTargetChoice unrelated) `shouldReturn` False
      choose "shuffle Sisterly Bond" (isTargetChoice bond)
      map pcCardId <$> discardOf you `shouldReturn` [pcCardId unrelated]

  it "Claws take two damage, can ready X-23 during payment, and add two ATK until round end" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      claws <- clawsInPlay
      runNow [IdentityMessage you ExhaustedIdentity]
      choose "Claws" (isAbilityOfCard claws)
      choose "Living Weapon while paying Claws" (isNamedAbility "$label.livingWeapon")
      pass
      damageOf you `shouldReturn` 2
      exhaustedNow you `shouldReturn` False
      upgradeIsExhausted (upgradeOf claws) `shouldReturn` True
      attackValue you `shouldReturn` 3
      runNow [EndRound]
      attackValue you `shouldReturn` 1

  it "Claws are hero-only and cannot pay their damage cost through tough" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      claws <- clawsInPlay
      offers (isAbilityOfCard claws) `shouldReturn` False
      asHero you
      runNow [IdentityMessage you IdentityBecomeTough]
      offers (isAbilityOfCard claws) `shouldReturn` False

  it "Honey Badger's nonlethal consequential damage readies X-23" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      honey <- putIntoPlay you "43003"
      h <- allyIn "43003"
      runNow [IdentityMessage you ExhaustedIdentity]
      choose "Honey Badger attacks" (isAllyAttack h)
      chooseVillain
      choose "Honey Badger response" (isAbilityOfCard honey)
      exhaustedNow you `shouldReturn` False
      a <- allyIn "43003"
      allyDamage a `shouldBe` 1

  it "Honey Badger cannot respond after lethal damage or in alter-ego" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      honey <- putIntoPlay you "43003"
      runNow [IdentityMessage you ExhaustedIdentity]
      start [AllyMessage (allyIdOf honey) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      offers (honeyResponse honey) `shouldReturn` False
      pass
      asHero you
      runNow [IdentityMessage you ExhaustedIdentity]
      start [AllyMessage (allyIdOf honey) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      offers (honeyResponse honey) `shouldReturn` False
      selectAny (AllyWithId $ allyIdOf honey) `shouldReturn` False
      exhaustedNow you `shouldReturn` True

  it "Adamantium Lacing adds identity hit points in both forms and basic piercing in hero form" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "43009"
      hitPointsOf you `shouldReturn` 12
      asHero you
      hitPointsOf you `shouldReturn` 12
      villain <- villainIdNow
      runNow [VillainMessage villain VillainBecomeTough]
      choose "basic piercing attack" (isHeroAttack you)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 1

  it "Adamantium Lacing grants retaliate only to X-23's hero form" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "43009"
      mods <- getModifiers $ IdentityRef you
      (KeywordModifier (Retaliate 1) `elem` mods) `shouldBe` False
      asHero you
      mods' <- getModifiers $ IdentityRef you
      (KeywordModifier (Retaliate 1) `elem` mods') `shouldBe` True

  it "Grim Resolve generates a wild resource in alter-ego and takes one damage" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      clearHand you
      grim <- putIntoPlay you "43010"
      lacing <- putInHand you "43009"
      choose "play Adamantium Lacing" (isPlay lacing)
      choose "Grim Resolve resource" (isAbilityOfCard grim)
      payWith []
      damageOf you `shouldReturn` 1
      upgradeIsExhausted (upgradeOf grim) `shouldReturn` True
      length <$> upgradesWith "43009" `shouldReturn` 1

  it "Pain Tolerance can heal for its own play and other X-23 card plays" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      clearHand you
      setDamage you 4
      pain <- putInHand you "43011"
      strength <- putInHand you "01090"
      playWith pain [strength]
      choose "heal for Pain Tolerance" (isAbilityOfCard pain)
      damageOf you `shouldReturn` 3
      grim <- putInHand you "43010"
      energy <- putInHand you "01088"
      playWith grim [energy]
      choose "heal for Grim Resolve" (isAbilityOfCard pain)
      damageOf you `shouldReturn` 2

  it "Pain Tolerance ignores basic and aspect cards and cards merely put into play" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      clearHand you
      pain <- putIntoPlay you "43011"
      setDamage you 4
      _ <- putIntoPlay you "43010"
      damageOf you `shouldReturn` 4
      endurance <- putInHand you "36026"
      energy <- putInHand you "01088"
      playOn endurance you [energy]
      offers (isAbilityOfCard pain) `shouldReturn` False
      damageOf you `shouldReturn` 4

  it "Sisterhood discards an X-23 card and searches deck and discard for Honey Badger" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      clearHand you
      sisterhood <- putIntoPlay you "43008"
      honey <- putInHand you "43003"
      runNow [DiscardedCard $ PlayerCard honey]
      bond <- putInHand you "43007"
      unrelated <- putInHand you "01088"
      choose "Sisterhood" (isAbilityOfCard sisterhood)
      offers (isTargetChoice unrelated) `shouldReturn` False
      choose "discard Sisterly Bond" (isTargetChoice bond)
      choose "find Honey Badger" (isTargetChoice honey)
      hand <- handOf you
      map pcCardId hand `shouldSatisfy` elem (pcCardId honey)
      offers (isAbilityOfCard sisterhood) `shouldReturn` False

  it "Puncture Wound requires an enemy X-23 attacked this turn, weakens it, and bursts next player phase" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      wound <- putInHand you "43012"
      offers (isPlay wound) `shouldReturn` False
      villain <- villainIdNow
      choose "basic attack" (isHeroAttack you)
      chooseVillain
      pass
      playOn wound villain []
      attack <- getVillainAttackValue villain
      attack `shouldBe` 1
      start [BeginPhase PlayerPhase]
      choose "Puncture Wound forced response" (isAbilityOfCard wound)
      villainDamageNow `shouldReturn` 4
      length <$> upgradesWith "43012" `shouldReturn` 0

  it "Grim Resolve cannot generate a resource if tough would prevent its damage cost" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      clearHand you
      grim <- putIntoPlay you "43010"
      runNow [IdentityMessage you IdentityBecomeTough]
      lacing <- putInHand you "43009"
      energy <- putInHand you "01088"
      choose "play Adamantium Lacing" (isPlay lacing)
      offers (isAbilityOfCard grim) `shouldReturn` False
      payWith [energy]
      damageOf you `shouldReturn` 0

  it "Honey Badger readies X-23 after positive ability damage without a per-phase limit" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      honey <- putIntoPlay you "43003"
      runNow [IdentityMessage you ExhaustedIdentity]
      start [AllyMessage (allyIdOf honey) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      choose "Honey Badger" (isAbilityOfCard honey)
      exhaustedNow you `shouldReturn` False
      runNow [AllyMessage (allyIdOf honey) $ AllyHealed 1, IdentityMessage you ExhaustedIdentity]
      start [AllyMessage (allyIdOf honey) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      choose "Honey Badger again" (isAbilityOfCard honey)
      exhaustedNow you `shouldReturn` False

  it "Puncture Wound can target an enemy Honey Badger attacked even after she leaves play" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      honey <- putIntoPlay you "43003"
      h <- allyIn "43003"
      wound <- putInHand you "43012"
      choose "Honey Badger attacks" (isAllyAttack h)
      chooseVillain
      pass
      runNow [RemoveFromPlay $ AllyRef $ allyIdOf honey]
      villain <- villainIdNow
      playOn wound villain []
      length <$> upgradesWith "43012" `shouldReturn` 1

  it "Puncture Wound forgets attacks when a new turn begins" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      wound <- putInHand you "43012"
      choose "X-23 attacks" (isHeroAttack you)
      chooseVillain
      pass
      offers (isPlay wound) `shouldReturn` True
      runNow [IdentityMessage you BeginTurn]
      offers (isPlay wound) `shouldReturn` False

  it "Grim Resolve permits Living Weapon during resource payment and then completes the card play" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      grim <- putIntoPlay you "43010"
      lacing <- putInHand you "43009"
      runNow [IdentityMessage you ExhaustedIdentity]
      choose "play Adamantium Lacing" (isPlay lacing)
      choose "Grim Resolve" (isAbilityOfCard grim)
      choose "Living Weapon during resource payment" (isNamedAbility "$label.livingWeapon")
      payWith []
      exhaustedNow you `shouldReturn` False
      damageOf you `shouldReturn` 1
      length <$> upgradesWith "43009" `shouldReturn` 1

  it "Sisterhood is unavailable without an X-23 card to discard" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      clearHand you
      sisterhood <- putIntoPlay you "43008"
      _ <- putInHand you "01088"
      offers (isAbilityOfCard sisterhood) `shouldReturn` False

  it "Pain Tolerance is unavailable when there is no damage to heal" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      clearHand you
      pain <- putIntoPlay you "43011"
      grim <- putInHand you "43010"
      energy <- putInHand you "01088"
      playWith grim [energy]
      offers (isAbilityOfCard pain) `shouldReturn` False

  it "Honey Badger controlled by another hero still readies X-23" do
    tg <- testGame ["x-23", "spider-man"]
    inGame tg do
      let you = player1 tg
          other = player2 tg
      runNow [IdentityMessage you $ ChangedToForm A, IdentityMessage other $ ChangedToForm A]
      honey <- putIntoPlay other "43003"
      runNow [IdentityMessage you ExhaustedIdentity, IdentityMessage other ExhaustedIdentity]
      start [AllyMessage (allyIdOf honey) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      choose "Honey Badger controlled by Spider-Man" (isAbilityOfCard honey)
      exhaustedNow you `shouldReturn` False
      exhaustedNow other `shouldReturn` True

  it "Adamantium Lacing gives its controller hit points while its named bonuses apply to X-23" do
    tg <- testGame ["x-23", "spider-man"]
    inGame tg do
      let you = player1 tg
          other = player2 tg
      runNow [IdentityMessage you $ ChangedToForm A, IdentityMessage other $ ChangedToForm A]
      _ <- putIntoPlay other "43009"
      hitPointsOf you `shouldReturn` 10
      hitPointsOf other `shouldReturn` 12
      ours <- getModifiers $ IdentityRef you
      theirs <- getModifiers $ IdentityRef other
      (KeywordModifier (Retaliate 1) `elem` ours) `shouldBe` True
      (KeywordModifier (Retaliate 1) `elem` theirs) `shouldBe` False


  it "Pain Tolerance responds when an X-23 card is played from discard" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      clearHand you
      setDamage you 3
      pain <- putIntoPlay you "43011"
      grim <- putInHand you "43010"
      runNow [DiscardedCard $ PlayerCard grim]
      energy <- putInHand you "01088"
      -- An effect granting permission to play this discarded card begins its normal play.
      start [IdentityMessage you $ PlayingCard grim Nothing]
      payWith [energy]
      choose "Pain Tolerance after play from discard" (isAbilityOfCard pain)
      damageOf you `shouldReturn` 2
      length <$> upgradesWith "43010" `shouldReturn` 1


  it "does not offer Living Weapon while X-23 cannot ready or consume its phase limit" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      runNow [IdentityMessage you ExhaustedIdentity]
      eid <- preventReady you
      start (damageIdentity you GameRef $ toDamage 1 FromAbility)
      offers (isNamedAbility "$label.livingWeapon") `shouldReturn` False
      pass
      exhaustedNow you `shouldReturn` True
      runNow [DisabledEffect eid]
      start (damageIdentity you GameRef $ toDamage 1 FromAbility)
      choose "Living Weapon after readiness restriction ends" (isNamedAbility "$label.livingWeapon")
      exhaustedNow you `shouldReturn` False

  it "does not offer Honey Badger's response while X-23 cannot ready" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      honey <- putIntoPlay you "43003"
      runNow [IdentityMessage you ExhaustedIdentity]
      eid <- preventReady you
      start [AllyMessage (allyIdOf honey) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      offers (honeyResponse honey) `shouldReturn` False
      pass
      exhaustedNow you `shouldReturn` True
      runNow [DisabledEffect eid, AllyMessage (allyIdOf honey) $ AllyHealed 1]
      start [AllyMessage (allyIdOf honey) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      choose "Honey Badger after readiness restriction ends" (honeyResponse honey)
      exhaustedNow you `shouldReturn` False

  it "Puncture Wound counts Honey Badger's attack while Beguiled treats her as a minion" do
    tg <- testGame ["x-23"]
    inGame tg do
      let you = player1 tg
      asHero you
      honey <- putIntoPlay you "43003"
      _ <- revealNow you Attachments.beguiled
      pass
      let mid = MinionId $ unAllyId $ allyIdOf honey
      selectAny (MinionWithId mid) `shouldReturn` True
      moon <- putIntoPlay you "19013"
      wound <- putInHand you "43012"
      offers (isPlay wound) `shouldReturn` False
      choose "Moondragon controls the minion" \case
        UseAbility ability -> abilitySource ability == AllyRef (allyIdOf moon) && abilityIndex ability == 1
        _ -> False
      choose "Honey Badger minion" (isTargetChoice mid)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 1
      offers (isPlay wound) `shouldReturn` True
      vid <- villainIdNow
      playOn wound vid []
      upgrades <- upgradesWith "43012"
      map upgradeAttachedEnemy upgrades `shouldBe` [Just $ EnemyVillainId vid]
