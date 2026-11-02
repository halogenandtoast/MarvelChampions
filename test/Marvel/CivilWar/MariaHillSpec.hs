module Marvel.CivilWar.MariaHillSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers qualified as CW
import Marvel.CivilWar.LeaderSpec (attachmentIdOf,withLeader,leaderHp)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Damage
import Marvel.Keyword qualified as K
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import TestImport

attachDecoy :: IdentityId -> VillainId -> GameAppT EncounterCard
attachDecoy you vid = do
  card <- encounterCard C.cw56084
  resolve [RevealEncounterCard you card]
  getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)
  pure card

spec :: Spec
spec = describe "Civil War Maria Hill modular set" do
  for_ [False,True] $ \fromDiscard ->
    it ("Maria Hill searches for and attaches a decoy from the correct encounter pile: discard " <> show fromDiscard) do
      tg <- testGame ["spider-man"]
      inGame tg do
        decoy <- encounterCard C.cw56084
        filler <- encounterCard M.hydraMercenary
        messages <- L.runQueueT $ L.capture $ L.createDeck L.EncounterDeckKey (if fromDiscard then [filler] else [decoy,filler])
        resolve messages
        when fromDiscard $ resolve [DiscardedCard $ EncounterCard decoy]
        mid <- addMinion (player1 tg) C.cw56083
        getAttachedCharacter (attachmentIdOf decoy) `shouldReturn` Just (MinionCharacter mid)
        getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem K.Stalwart)
        getEncounterDiscard >>= (`shouldSatisfy` notElem decoy)

  it "a revealed decoy attaches to the enemy leader, grants stalwart, and prevents one attack's damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      before <- leaderHp vid
      decoy <- attachDecoy you vid
      getEnemyKeywords (EnemyVillainId vid) >>= (`shouldSatisfy` elem K.Stalwart)
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) (toDamage 3 $ FromPlayerAttack you)]
      leaderHp vid `shouldReturn` before
      getAttachedCharacter (attachmentIdOf decoy) `shouldReturn` Nothing
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) (toDamage 3 $ FromPlayerAttack you)]
      leaderHp vid `shouldReturn` (before - 3)

  it "keeps a decoy against non-attack damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      before <- leaderHp vid
      decoy <- attachDecoy you vid
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) (toDamage 2 FromAbility)]
      leaderHp vid `shouldReturn` (before - 2)
      getAttachedCharacter (attachmentIdOf decoy) `shouldReturn` Just (VillainCharacter vid)

  it "keeps a decoy when tough already prevents the attack's damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      before <- leaderHp vid
      decoy <- attachDecoy you vid
      resolve [VillainMessage vid VillainBecomeTough,VillainMessage vid $ VillainDamaged (IdentityRef you) (toDamage 2 $ FromPlayerAttack you)]
      leaderHp vid `shouldReturn` before
      getAttachedCharacter (attachmentIdOf decoy) `shouldReturn` Just (VillainCharacter vid)

  it "cannot prevent unpreventable attack damage but still discards the decoy" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      before <- leaderHp vid
      decoy <- attachDecoy you vid
      let damage = (toDamage 3 $ FromPlayerAttack you) {damageUnpreventable = True}
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) damage]
      leaderHp vid `shouldReturn` (before - 3)
      getAttachedCharacter (attachmentIdOf decoy) `shouldReturn` Nothing

  for_ [False,True] $ \present ->
    it ("Executive Order places threat on every scheme and surges only with the Maria Hill minion: " <> show present) do
      tg <- testGame [if present then "spider-man" else "maria-hill"]
      inGame tg do
        let you = player1 tg
        when present $ do
          maria <- encounterCard C.cw56083
          resolve [PutEncounterCardIntoPlay you maria]
        side <- encounterCard C.cw56077
        resolve [RevealEncounterCard you side]
        [next] <- stackEncounterDeck [M.hydraSoldier]
        before <- mainSchemeThreat
        card <- encounterCard C.cw56085
        resolve [RevealEncounterCard you card]
        mainSchemeThreat `shouldReturn` (before + 2)
        sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId side) `shouldReturn` 6
        playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` if present then [next] else []

  for_ [False,True] $ \present ->
    it ("Executive Order's boost gains three icons only with Maria Hill in play: " <> show present) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        vid <- withLeader "57040"
        when present $ do
          maria <- encounterCard C.cw56083
          resolve [PutEncounterCardIntoPlay you maria]
        _ <- stackEncounterDeck [C.cw56085]
        before <- mainSchemeThreat
        resolve [VillainMessage vid VillainSchemes]
        mainSchemeThreat `shouldReturn` (before + if present then 4 else 1)

  it "a decoy protects the inactive team's leader against another leader's attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      attacker <- withLeader "57040"
      defender <- addSecondBoard tg
      decoy <- encounterCard C.cw56084
      attach <- L.runQueueT $ L.capture $ L.attachEncounterCard decoy defender
      resolve attach
      before <- leaderHp defender
      _ <- stackEncounterDeck [M.hydraMercenary]
      attack <- L.runQueueT $ L.capture $ CW.leaderAttacksLeader attacker defender
      resolve attack
      leaderHp defender `shouldReturn` before
      getAttachedCharacter (attachmentIdOf decoy) `shouldReturn` Nothing
      damageOf you `shouldReturn` 0

  it "Helicarrier damages only the defeater's characters and engaged minions" do
    tg <- testGame ["captain-marvel","spider-man"]
    inGame tg do
      let first = player1 tg
          you = player2 tg
      cat <- putInHand you "01002"
      putAlly <- L.runQueueT $ L.capture $ L.putIntoPlay you cat
      resolve putAlly
      own <- addMinion you M.hydraMercenary
      other <- addMinion first M.hydraMercenary
      side <- encounterCard C.cw56086
      resolve [RevealEncounterCard first side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      sideSchemeThreat sid `shouldReturn` 5
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef you) 5]
      damageOf you `shouldReturn` 1
      damageOf first `shouldReturn` 0
      getsGame (fmap (allyDamage . toAttrs) . lookup (allyIdOf cat) . gameAllies) `shouldReturn` Just 1
      minionDamageOf own `shouldReturn` Just 1
      minionDamageOf other `shouldReturn` Just 0


  it "limits decoys to one per character and prevents a minion's attack damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      maria <- encounterCard C.cw56083
      first <- encounterCard C.cw56084
      resolve [PutEncounterCardIntoPlay you maria]
      let mid = MinionId $ unCardId $ ecCardId maria
      attach <- L.runQueueT $ L.capture $ L.attachEncounterCard first mid
      resolve attach
      second <- encounterCard C.cw56084
      start [RevealEncounterCard you second]
      offers (isTargetChoice mid) `shouldReturn` False
      choose "attach the second decoy to the leader" (isTargetChoice vid)
      getAttachedCharacter (attachmentIdOf first) `shouldReturn` Just (MinionCharacter mid)
      getAttachedCharacter (attachmentIdOf second) `shouldReturn` Just (VillainCharacter vid)
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef you) (toDamage 3 $ FromPlayerAttack you)]
      minionDamageOf mid `shouldReturn` Just 0
      getAttachedCharacter (attachmentIdOf first) `shouldReturn` Nothing
