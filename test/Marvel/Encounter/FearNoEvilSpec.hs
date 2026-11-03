module Marvel.Encounter.FearNoEvilSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Message.Lifted qualified as L
import Marvel.Modifier
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Types qualified as Side
import Marvel.Trait qualified as Trait
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as Set
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import Marvel.Phase (Phase(VillainPhase))
import Marvel.Window qualified as W
import Marvel.Ref
import TestImport

heldAt :: SideSchemeId -> GameAppT [EncounterCard]
heldAt sid = getsGame $ Side.sideSchemeHeldEncounterCards . toAttrs . (HashMap.! sid) . gameSideSchemes

spec :: Spec
spec = describe "Fear No Evil encounter cards" do
  it "contains five physical cards" do
    tg <- testGame ["spider-man"]
    inGame tg $ length <$> gatherEncounterSet Set.FneDisasters >>= (`shouldBe` 5)

  it "a disaster starts with two civilians and leaves after the last is rescued" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Cards.street60177
      pass
      eid <- selectJust (EnvironmentIs Cards.street60177)
      getCountersOn eid `shouldReturn` 2
      resolve [RemoveCounters (EnvironmentRef eid) 1]
      getCountersOn eid `shouldReturn` 1
      resolve [RemoveCounters (EnvironmentRef eid) 1]
      selectAny (EnvironmentIs Cards.street60177) `shouldReturn` False

  it "Bystanders in Peril adds a civilian to a chosen disaster" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Cards.street60178
      pass
      eid <- selectJust (EnvironmentIs Cards.street60178)
      _ <- revealNow (player1 tg) Cards.street60181
      choose "the disaster" (isTargetChoice eid)
      pass
      getCountersOn eid `shouldReturn` 3

  it "Cops has five cards, including both ordinary cops" do
    tg <- testGame ["spider-man"]
    inGame tg do
      cards <- gatherEncounterSet Set.FneCops
      length cards `shouldBe` 5
      length (filter ((== "60182") . toCardCode) cards) `shouldBe` 2

  it "a cop schemes by attacking the villain instead" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60182
      before <- mainSchemeThreatNow
      resolve [MinionMessage mid MinionSchemes]
      mainSchemeThreatNow `shouldReturn` before

  it "Police Cordon gives Police minions guard and patrol" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60183
      _ <- revealNow (player1 tg) Cards.street60185
      pass
      minionMatches (MinionWithKeyword Guard <> MinionWithKeyword Patrol) mid `shouldReturn` True

  it "The Owl and Tombstone each contain five physical encounter cards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      length <$> gatherEncounterSet Set.FneTheOwl `shouldReturn` 5
      length <$> gatherEncounterSet Set.FneTombstone `shouldReturn` 5

  it "Flight Serum attaches to an enemy without Aerial and increases its basic powers" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard Cards.street60191
      resolve [RevealEncounterCard (player1 tg) card]
      getModifiers vid >>= (`shouldSatisfy` elem (AttackModifier 1))
      getModifiers vid >>= (`shouldSatisfy` elem (SchemeModifier 1))
      enemyMatches (EnemyWithTrait Trait.Aerial) (EnemyVillainId vid) `shouldReturn` True

  it "The Owl has piercing even when his quickstrike does not trigger" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60193
      minionMatches (MinionWithKeyword Piercing) mid `shouldReturn` True
      minionMatches (MinionWithKeyword Quickstrike) mid `shouldReturn` True

  it "Mister Fish engages the player with the fewest remaining hit points" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      setDamage (player2 tg) 8
      mid <- addMinion (player1 tg) Cards.street60192
      minionMatches (MinionEngagedWith $ IdentityWithId $ player2 tg) mid `shouldReturn` True
      minionMatches (MinionWithKeyword Ranged <> MinionWithKeyword Piercing) mid `shouldReturn` True

  it "Cold as Ice toughens the highest printed-HP minion and confuses the first player after it schemes" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60198
      card <- encounterCard Cards.street60195
      resolve [RevealEncounterCard (player1 tg) card]
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 1
      getModifiers mid >>= (`shouldSatisfy` elem (SchemeModifier 2))
      resolve [MinionMessage mid MinionSchemes]
      identityMatches ConfusedIdentity (player1 tg) `shouldReturn` True
      selectAny (AttachmentIs Cards.street60195) `shouldReturn` False

  it "Hard as Marble stuns the character the attached minion attacked and discards itself" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.street60198
      card <- encounterCard Cards.street60196
      resolve [RevealEncounterCard p card]
      getModifiers mid >>= (`shouldSatisfy` elem (AttackModifier 2))
      resolve [MinionMessage mid $ MinionAttacks p]
      identityMatches StunnedIdentity p `shouldReturn` True
      selectAny (AttachmentIs Cards.street60196) `shouldReturn` False

  it "Beetle chooses a minion attachment and attaches it to himself without its original host-selection instruction" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- addMinion (player1 tg) Cards.street60198
      _ <- setEncounterDeck [Cards.street60195] []
      mid <- addMinion (player1 tg) Cards.street60197
      getModifiers mid >>= (`shouldSatisfy` elem (SchemeModifier 2))
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 0
      getEncounterDeck `shouldReturn` []

  it "Swoop Down gives Aerial enemies two extra scheme during their activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60192
      flight <- encounterCard Cards.street60191
      resolve =<< L.runQueueT (L.capture $ L.attachEncounterCard flight mid)
      _ <- setEncounterDeck [Cards.street60193,Cards.street60191] []
      before <- mainSchemeThreatNow
      card <- encounterCard Cards.street60194
      resolve [RevealEncounterCard (player1 tg) card]
      mainSchemeThreatNow `shouldReturn` before + 5
      a <- getsGame $ toAttrs . (HashMap.! mid) . gameMinions
      Minion.minionBoost a `shouldBe` 0

  it "Tracksuit Mafia contains seven cards including three Tracksuit Bros" do
    tg <- testGame ["spider-man"]
    inGame tg do
      cards <- gatherEncounterSet Set.FneTracksuitMafia
      length cards `shouldBe` 7
      length (filter ((== "60203") . toCardCode) cards) `shouldBe` 3

  it "Ivan's acceleration tokens increase when he schemes and leave play with him" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60200
      getAccelerationTokenCount `shouldReturn` 1
      getAccelerationCount `shouldReturn` 1
      resolve [MinionMessage mid MinionSchemes]
      getAccelerationTokenCount `shouldReturn` 2
      getAccelerationCount `shouldReturn` 2
      resolve [RemoveFromPlay $ MinionRef mid]
      getAccelerationTokenCount `shouldReturn` 0

  it "The Clown retains a facedown boost card after scheming for his next activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [Cards.street60191,Cards.street60191,Cards.street60191] []
      mid <- addMinion (player1 tg) Cards.street60201
      resolve [MinionMessage mid MinionSchemes]
      cards <- getsGame $ Minion.minionBoostCards . toAttrs . (HashMap.! mid) . gameMinions
      length cards `shouldBe` 1

  it "a defeated Tracksuit Bro shuffles back when the side scheme is absent" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60203
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 3 FromAbility]
      selectAny (MinionWithId mid) `shouldReturn` False
      deck <- getEncounterDeck
      length (filter ((== CardId (unMinionId mid)) . ecCardId) deck) `shouldBe` 1
      discard <- getEncounterDiscard
      liftIO $ map ecCardId discard `shouldNotContain` [CardId (unMinionId mid)]

  it "Tracksuit Mafia tucks the actual discarded minion, persists it, and discards it when leaving play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [Cards.street60191,Cards.street60203,Cards.street60191] []
      card <- encounterCard Cards.street60204
      resolve [RevealEncounterCard (player1 tg) card]
      sid <- selectJust (SideSchemeIs Cards.street60204)
      held <- heldAt sid
      map toCardCode held `shouldBe` ["60203"]
      mainSchemeThreatNow `shouldReturn` 0
      game <- getGame
      case fromJSON @Game (toJSON game) of
        Error err -> liftIO $ expectationFailure err
        Success saved -> toJSON saved `shouldBe` toJSON game
      discard <- getEncounterDiscard
      liftIO $ map ecCardId discard `shouldNotContain` map ecCardId held
      resolve [RemoveFromPlay $ SideSchemeRef sid]
      after <- getEncounterDiscard
      liftIO $ map ecCardId after `shouldContain` map ecCardId held

  it "a defeated Tracksuit minion tucks under Tracksuit Mafia with its physical card id" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [Cards.street60200,Cards.street60191] []
      card <- encounterCard Cards.street60204
      resolve [RevealEncounterCard (player1 tg) card]
      sid <- selectJust (SideSchemeIs Cards.street60204)
      broCard <- encounterCard Cards.street60203
      resolve [RevealEncounterCard (player1 tg) broCard]
      let mid = minionOf broCard
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 3 FromAbility]
      selectAny (MinionWithId mid) `shouldReturn` False
      held <- heldAt sid
      liftIO $ map ecCardId held `shouldContain` [CardId $ unMinionId mid]
      removed <- getRemovedFromGameEncounterCards
      liftIO $ map ecCardId removed `shouldNotContain` [CardId $ unMinionId mid]

  it "revealing a Tracksuit from the deck releases one tucked minion without recursively releasing another" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [Cards.street60200,Cards.street60191] []
      card <- encounterCard Cards.street60204
      resolve [RevealEncounterCard (player1 tg) card]
      sid <- selectJust (SideSchemeIs Cards.street60204)
      bro <- encounterCard Cards.street60203
      resolve [RevealEncounterCardFrom W.RevealedFromSetAside (player1 tg) bro]
      let mid = minionOf bro
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 3 FromAbility]
      length <$> heldAt sid `shouldReturn` 2
      newBro <- encounterCard Cards.street60203
      resolve [RevealEncounterCard (player1 tg) newBro]
      length <$> heldAt sid `shouldReturn` 1

  it "Tracksuit Mafioso stuns and confuses the actual ally that defeated him when no Mafia scheme is present" do
    tg <- testGame ["spider-man"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01067"
      let aid = allyIdOf ally
      mid <- addMinion (player1 tg) Cards.street60202
      resolve [MinionMessage mid $ MinionDamaged (AllyRef aid) $ toDamage 7 $ FromAllyAttack aid]
      getCharacterStatusCount (AllyCharacter aid) StunnedStatus `shouldReturn` 1
      getCharacterStatusCount (AllyCharacter aid) ConfusedStatus `shouldReturn` 1

  it "Hit List removes one threat per player for an ally actually defeated by its indirect damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01067"
      let aid = allyIdOf ally
      card <- encounterCard Cards.street60199
      resolve [RevealEncounterCard (player1 tg) card]
      sid <- selectJust (SideSchemeIs Cards.street60199)
      runNow [CheckWindows [W.Window W.After $ W.PhaseBegan VillainPhase]]
      whenM (offers $ isAbilityOf sid) $ choose "Hit List's forced response" (isAbilityOf sid)
      choose "assign the first damage to Black Cat" (isTargetChoice aid)
      choose "assign lethal damage to Black Cat" (isTargetChoice aid)
      pass
      whenM (offers $ isTargetChoice sid) $ choose "remove threat from Hit List" (isTargetChoice sid)
      pass
      selectAny (AllyWithId aid) `shouldReturn` False
      getsGame (Side.sideSchemeThreat . toAttrs . (HashMap.! sid) . gameSideSchemes) `shouldReturn` 2

  it "Tombstone's boost gives the villain tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard Cards.street60198
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1

  it "Swoop Down's extra attack does not carry over when stun cancels the Aerial minion's activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.street60192
      flight <- encounterCard Cards.street60191
      resolve =<< L.runQueueT (L.capture $ L.attachEncounterCard flight mid >> L.giveStatus mid StunnedStatus)
      resolve [IdentityMessage p $ ChangedToForm A]
      _ <- setEncounterDeck [Cards.street60193,Cards.street60191] []
      card <- encounterCard Cards.street60194
      resolve [RevealEncounterCard p card]
      getCharacterStatusCount (MinionCharacter mid) StunnedStatus `shouldReturn` 0
      a <- getsGame $ toAttrs . (HashMap.! mid) . gameMinions
      Minion.minionBoost a `shouldBe` 0
      before <- damageOf p
      resolve [MinionMessage mid $ MinionAttacks p]
      damageOf p `shouldReturn` before + 3
