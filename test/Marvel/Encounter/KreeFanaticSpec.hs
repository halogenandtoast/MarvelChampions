module Marvel.Encounter.KreeFanaticSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as A
import Marvel.Attachment.Types qualified as Attachment
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet (EncounterSet (KreeFanatic))
import Marvel.Matchers
import Marvel.Query
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Minion.Cards qualified as M
import Marvel.Minion.Runner qualified as Minion
import Marvel.SideScheme.Cards qualified as S
import Marvel.Treachery.Cards qualified as T
import TestImport

spec :: Spec
spec = describe "Kree Fanatic" do
  it "contains all seven printed encounter cards" do
    tg <- testGame ["spider-man"]
    inGame tg $ length <$> gatherEncounterSet KreeFanatic >>= (`shouldBe` 7)

  it "gives Ronan toughness, nine hit points per player, and immunity to stun" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      mid <- addMinion (player1 tg) M.ronanKreeFanatic
      minions <- getsGame gameMinions
      Minion.getModifiedHitPoints (toAttrs $ fromMaybe (error "missing Ronan") $ HashMap.lookup mid minions) `shouldReturn` 18
      liftIO $ Minion.minionTough (toAttrs $ fromMaybe (error "missing Ronan") $ HashMap.lookup mid minions) `shouldBe` True
      resolve [GiveStatus (MinionRef mid) StunnedStatus]
      minionMatches StunnedMinion mid `shouldReturn` False

  it "puts Ronan into play when revealed as a boost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard M.ronanKreeFanatic
      resolve [RevealBoostCard card (EnemyVillainId vid), ClearBoosts]
      selectListCount (MinionIs M.ronanKreeFanatic) `shouldReturn` 1

  it "puts Judge, Jury, Executioner into play as a boost, with its threat and acceleration" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard S.judgeJuryExecutioner
      resolve [RevealBoostCard card (EnemyVillainId vid), ClearBoosts]
      selectListCount (SideSchemeIs S.judgeJuryExecutioner) `shouldReturn` 1

  it "attaches The Accused to the player and discards it after Ronan is defeated" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p M.ronanKreeFanatic
      _ <- revealNow p A.theAccused
      pass
      attached <- getsGame $ map (Attachment.attachmentIdentity . toAttrs) . HashMap.elems . gameAttachments
      liftIO $ attached `shouldSatisfy` elem (Just p)
      resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 1 FromAbility)]
      resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 9 FromAbility)]
      codes <- getsGame $ map (toCardCode . getCardDef) . HashMap.elems . gameAttachments
      liftIO $ codes `shouldNotContain` ["90003"]

  it "makes Ronan scheme against his engaged alter-ego" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- addMinion p M.ronanKreeFanatic
      before <- mainSchemeThreat
      _ <- revealNow p T.bringTheHammerDown
      pass
      mainSchemeThreat `shouldReturn` (before + 3)

  it "surges when Bring the Hammer Down finds no Ronan" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [S.bombScare, M.hydraMercenary] []
      _ <- revealNow p T.bringTheHammerDown
      pass
      map toCardCode . playerIdentityEncounterCards <$> playerAttrs p >>= (`shouldSatisfy` elem "01109")

  it "deals the discarded Kree Fanatic cards facedown and leaves other cards discarded" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [M.ronanKreeFanatic, S.bombScare, A.theAccused, T.bringTheHammerDown, M.hydraMercenary, M.hydraSoldier] []
      _ <- revealNow p T.youDareOpposeMe
      pass
      codes <- map toCardCode . playerIdentityEncounterCards <$> playerAttrs p
      liftIO $ codes `shouldContain` ["90001", "90003", "90004"]
      encounterDiscardCodes >>= (`shouldSatisfy` elem "01109")

  it "keeps its engagement when the villain phase begins with no hero in play" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      mid <- addMinion (player2 tg) M.ronanKreeFanatic
      resolve [BeginPhase VillainPhase]
      getMinionEngagedPlayer mid `shouldReturn` Just (player2 tg)

  it "moves to the hero with the fewest remaining hit points" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        q = player2 tg
    inGame tg do
      changeForm p
      pass
      endTurn
      pass
      changeForm q
      pass
      setDamage q 5
      mid <- addMinion p M.ronanKreeFanatic
      selectList (IdentityWithFewestRemainingHitPoints HeroIdentity) `shouldReturn` [q]
      endTurn
      choose "resolve Ronan’s engagement" (isAbilityOf mid)
      getMinionEngagedPlayer mid `shouldReturn` Just q

  it "makes an enemy initiating an attack against The Accused hit one harder" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      mid <- addMinion p M.ronanKreeFanatic
      _ <- revealNow p A.theAccused
      pass
      start [MinionMessage mid (MinionAttacks p)]
      choose "resolve The Accused" isAnyAbility
      pass
      damageOf p `shouldReturn` 4

  it "places two threat when an enemy attack defeats a friendly ally" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01084"
      mid <- addMinion p M.ronanKreeFanatic
      _ <- revealNow p S.judgeJuryExecutioner
      pass
      before <- mainSchemeThreat
      resolve [MinionMessage mid (MinionAttacksAlly p $ AllyId $ unCardId $ pcCardId ally)]
      mainSchemeThreat `shouldReturn` (before + 2)

  it "deals the first player an encounter card when Bring the Hammer Down boosts a lethal attack" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01084"
      _ <- setEncounterDeck [T.bringTheHammerDown, S.bombScare, M.hydraSoldier, M.hydraMercenary] []
      before <- length . playerIdentityEncounterCards <$> playerAttrs p
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainAttacksAlly p $ AllyId $ unCardId $ pcCardId ally)]
      length . playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` (before + 1)

  it "gives an attack overkill when You Dare Oppose Me is its boost" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01084"
      _ <- setEncounterDeck [T.youDareOpposeMe, S.bombScare, M.hydraSoldier] []
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainAttacksAlly p $ AllyId $ unCardId $ pcCardId ally)]
      damageOf p `shouldReturn` 1
