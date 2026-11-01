module Marvel.Rules.PsylockeRulesSpec (spec) where

import Marvel.Choice qualified as Choice
import Marvel.Matchers (IdentityMatcher (IdentityWithId))
import Marvel.Keyword (Keyword (Overkill, Piercing, Ranged))
import Marvel.Message.Lifted qualified as L
import Marvel.Window qualified as W
import TestImport

spec :: Spec
spec = describe "Psylocke attack rules" do
  it "recognizes every supported attack keyword and rejects an attack without one" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      let you = player1 tg
          matcher = W.MakesKeywordAttack W.When (IdentityWithId you) [Overkill, Piercing, Ranged]
          matches keywords = W.windowMatches matcher (W.Window W.When $ W.MakingKeywordAttack you (IdentityRef you) (EnemyVillainId vid) keywords False) GameRef
      matches [Piercing] `shouldReturn` True
      matches [Overkill] `shouldReturn` True
      matches [Ranged] `shouldReturn` True
      matches [] `shouldReturn` False

  it "opens an event attack interrupt for each new target and not a repeated target" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putInHand you "05030"
      let event = createEvent you card
          eid = toId event
          source = EventRef eid
      withGame_ $ entitiesL . eventsL . at eid ?~ event
      first <- EnemyVillainId <$> villainIdNow
      second <- EnemyMinionId <$> getRandom
      resolve [EventMessage eid $ EventAttacked you first False]
      sourceHasAttackedTarget source first `shouldReturn` True
      sourceHasAttackedTarget source second `shouldReturn` False
      let hasAttack target = any (\case CheckWindows windows -> W.Window W.When (W.IdentityAttackTarget you target) `elem` windows; _ -> False)
      firstMessages <- Choice.choiceMessages you $ DamageEnemy (enemyRef first) source (toDamage 2 $ FromPlayerAttack you)
      secondMessages <- Choice.choiceMessages you $ DamageEnemy (enemyRef second) source (toDamage 2 $ FromPlayerAttack you)
      liftIO $ hasAttack first firstMessages `shouldBe` False
      liftIO $ hasAttack second secondMessages `shouldBe` True

  it "redirects a confusion-only attack to a friendly identity without inventing damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putInHand you "05030"
      let event = createEvent you card
          eid = toId event
          source = EventRef eid
      withGame_ $ entitiesL . eventsL . at eid ?~ event
      vid <- villainIdNow
      withQueue_ $ const
        [ EventMessage eid (EventAttacked you (EnemyVillainId vid) False)
        , VillainMessage vid (VillainConfused source)
        , EventMessage eid ResolvedEvent
        ]
      L.runQueueT $ L.redirectCurrentPlayerAttack you (EnemyVillainId vid) (IdentityCharacter you)
      pending <- withQueue $ \queue -> ([], queue)
      resolve pending
      isConfused you `shouldReturn` True
      damageOf you `shouldReturn` 0
      villainDamageNow `shouldReturn` 0
      getsGame (elem you . gameHeroAttackedThisPhase) `shouldReturn` True

  it "preserves piercing when an attack is redirected to a tough identity" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putInHand you "05030"
      let event = createEvent you card
          eid = toId event
          source = EventRef eid
      withGame_ $ entitiesL . eventsL . at eid ?~ event
      vid <- villainIdNow
      resolve [GiveStatus (IdentityRef you) ToughStatus]
      withQueue_ $ const
        [ VillainMessage vid (VillainDamaged source $ withPiercing $ toDamage 3 $ FromPlayerAttack you)
        , EventMessage eid (EventAttacked you (EnemyVillainId vid) False)
        , EventMessage eid ResolvedEvent
        ]
      L.runQueueT $ L.redirectCurrentPlayerAttack you (EnemyVillainId vid) (IdentityCharacter you)
      pending <- withQueue $ \queue -> ([], queue)
      resolve pending
      damageOf you `shouldReturn` 3
      villainDamageNow `shouldReturn` 0


  it "increases only one simultaneous target while preserving damage before target bonuses" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
          source = IdentityRef you
      vid <- villainIdNow
      mid <- getRandom
      let enemy = EnemyVillainId vid
          damage = toDamage 2 (FromPlayerAttack you)
          metadata = CardSpecific GameRef "playerAttackDamageBeforeTarget" (toJSON (you, source, enemy, damage))
      withQueue_ $ const
        [ Simultaneously [VillainMessage vid (VillainDamaged source damage), MinionMessage mid (MinionDamaged source damage)]
        , metadata
        ]
      L.runQueueT $ L.addToCurrentAttackAgainst you source enemy 1
      withQueueM pure `shouldReturn`
        [ Simultaneously [VillainMessage vid (VillainDamaged source damage {damageAmount = 3}), MinionMessage mid (MinionDamaged source damage)]
        , CardSpecific GameRef "playerAttackDamageBeforeTarget" (toJSON (you, source, enemy, damage))
        ]

  it "drops a keyword supplied only by the old target when determining friendly retaliation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
          source = IdentityRef you
      vid <- villainIdNow
      let enemy = EnemyVillainId vid
          raw = toDamage 2 (FromPlayerAttack you)
      withQueue_ $ const
        [ CardSpecific GameRef "playerAttackDamageBeforeTarget" (toJSON (you, source, enemy, raw))
        , VillainMessage vid (VillainDamaged source $ withRanged raw)
        , CheckWindows [W.Window W.After $ W.PlayerAttackResolved source you [enemy]]
        ]
      L.runQueueT $ L.redirectCurrentPlayerAttack you enemy (IdentityCharacter you)
      queue <- withQueueM pure
      let flags = [ranged | CardSpecific GameRef "psylockeFriendlyAttackResolved" (fromJSON -> Success (_ :: IdentityId, _ :: Source, _ :: CharacterId, ranged :: Bool)) <- queue]
      liftIO $ flags `shouldBe` [False]

  it "preserves a confusion-only event's ranged state when redirecting its target" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      eid <- getRandom
      vid <- villainIdNow
      let enemy = EnemyVillainId vid
      withQueue_ $ const
        [ EventMessage eid (EventAttacked you enemy True)
        , VillainMessage vid (VillainConfused $ EventRef eid)
        , EventMessage eid ResolvedEvent
        ]
      L.runQueueT $ L.redirectCurrentPlayerAttack you enemy (IdentityCharacter you)
      queue <- withQueueM pure
      let flags = [ranged | CardSpecific GameRef "psylockeFriendlyAttackResolved" (fromJSON -> Success (_ :: IdentityId, _ :: Source, _ :: CharacterId, ranged :: Bool)) <- queue]
      liftIO $ flags `shouldBe` [True]


  it "increases every pending target of a globally keyword-bearing attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
          source = IdentityRef you
      vid <- villainIdNow
      mid <- getRandom
      let damage = withRanged $ toDamage 2 (FromPlayerAttack you)
      withGame_ $ windowsL .~ [[W.Window W.When $ W.MakingKeywordAttack you source (EnemyVillainId vid) [Ranged] False]]
      withQueue_ $ const [Simultaneously [VillainMessage vid (VillainDamaged source damage), MinionMessage mid (MinionDamaged source damage)]]
      L.runQueueT $ L.addToKeywordAttackDamage you 2
      withQueueM pure `shouldReturn` [Simultaneously [VillainMessage vid (VillainDamaged source damage {damageAmount = 4}), MinionMessage mid (MinionDamaged source damage {damageAmount = 4})]]


  it "opens a keyword interrupt for an attack whose damage can be increased from zero" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      enemy <- EnemyVillainId <$> villainIdNow
      messages <- Choice.choiceMessages you $ DamageEnemy (enemyRef enemy) (IdentityRef you) (withPiercing $ toDamage 0 $ FromPlayerAttack you)
      let keywordWindow = any (\case CheckWindows windows -> any (\case W.Window W.When (W.MakingKeywordAttack _ _ _ keywords _) -> Piercing `elem` keywords; _ -> False) windows; _ -> False) messages
      liftIO $ keywordWindow `shouldBe` True
