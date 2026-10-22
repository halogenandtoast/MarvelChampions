-- | The engine mechanics the Green Goblin, Wrecking Crew, Rise of Red Skull and Once and
-- Future Kang sets build on, each as the rules have it.
module Marvel.Rules.SetMechanicsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (singleton)
import Data.Text qualified as T
import Marvel.EncounterCard (isPlayerObligation)
import Marvel.Attack (Attack (..), attack)
import Marvel.Hp
import Marvel.Keyword (Keyword (Incite, Villainous))
import Marvel.MainScheme.Types (Field (MainSchemeCounters))
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types (overMinion)
import Marvel.Obligation.Obligations.EvictionNotice (EvictionNotice (..), evictionNotice)
import Marvel.Obligation.Types (Obligation (..), heldCardsL, toObligationAttrs)
import Marvel.Scenario.Types (Field (ScenarioEncounterDeck))
import Marvel.Villain.Types (flipVillainTo, hpL, maxHpL, stageL, stunnedL, toVillainAttrs, toughL)
import Marvel.Villain.Villains.Rhino (rhino2)
import TestImport

spec :: Spec
spec = describe "set mechanics" do
  it "counters are placed on and removed from the main scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- mainSchemeId
      resolve [PlaceCounters (toTarget mid) 3, RemoveCounters (toTarget mid) 1]
      counters <- getsGame (map (field MainSchemeCounters) . HashMap.elems . gameMainSchemes)
      liftIO $ counters `shouldBe` [2]

  it "a flipped villain keeps its damage, stage and status cards, its card turned over" do
    tg <- testGame ["spider-man"]
    inGame tg do
      v <- getVillain
      let attrs = toAttrs v & hpL .~ HP 5 & stunnedL .~ True
          flipped = toVillainAttrs (flipVillainTo rhino2 attrs)
      liftIO do
        unHp (flipped ^. hpL) `shouldBe` 5
        unHp (flipped ^. maxHpL) `shouldBe` unHp (attrs ^. maxHpL)
        flipped ^. stunnedL `shouldBe` True
        flipped ^. stageL `shouldBe` 1
        toCardCode (getCardDef flipped) `shouldBe` "01095"

  it "incite places its threat on the main scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      before <- mainSchemeThreatNow
      msgs <- Lifted.runQueueT $ Lifted.capture $ Lifted.incite Cards.hydraMercenary {cdKeywords = singleton (Incite 2)}
      resolve msgs
      after <- mainSchemeThreatNow
      liftIO $ after `shouldBe` before + 2

  it "a setup removes each player's obligations from the encounter deck" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let obligations = getsGame (filter isPlayerObligation . field ScenarioEncounterDeck . gameScenario)
      before <- obligations
      resolve [RemovePlayerObligationsFromGame]
      after <- obligations
      liftIO $ (length before, length after) `shouldBe` (1, 0)

  it "the cards under an obligation go to their owner's discard pile with it" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      cardId <- getRandom
      oid <- getRandom
      let card = MkPlayerCard cardId (lookupPlayerCard "01088") (Just you) (Just you)
          held = toObligationAttrs (buildCard evictionNotice oid) & heldCardsL .~ [card]
      withGame_ $ entitiesL . obligationsL . at oid ?~ Obligation (EvictionNotice held)
      resolve [RemoveFromPlay (ObligationRef oid)]
      discard <- discardOf you
      liftIO $ discard `shouldContain` [card]

  it "counters and the removal of players' obligations are told in the log" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- mainSchemeId
      runNow [PlaceCounters (toTarget mid) 2, RemoveCounters (toTarget mid) 1, RemovePlayerObligationsFromGame]
    text <- T.concat . map entryText <$> readIORef tg.logged
    for_ ["$log.countersPlaced", "$log.countersRemoved", "$log.playerObligationsRemoved"] \key ->
      text `shouldSatisfy` T.isInfixOf key

  it "a card with incite, revealed, places its threat on the main scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      before <- mainSchemeThreatNow
      _ <- revealNow (player1 tg) Cards.hydraMercenary {cdKeywords = singleton (Incite 2)}
      after <- mainSchemeThreatNow
      liftIO $ after `shouldBe` before + 2

  it "indirect damage is divided among the player's characters, none past defeat" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      hellcat <- allyIdOf <$> putIntoPlay you "01020"
      runNow [AssignIndirectDamage you GameRef 5 []]
      offered <- offers (isTargetChoice you)
      liftIO $ offered `shouldBe` True
      -- Hellcat has 3 hit points: once she is assigned 3, the rest goes to the hero
      for_ [1 .. 3 :: Int] \_ -> choose "assign to Hellcat" (isTargetChoice hellcat)
      damage <- damageOf you
      hellcats <- alliesWith "01020"
      liftIO $ (damage, length hellcats) `shouldBe` (2, 0)
    text <- T.concat . map entryText <$> readIORef tg.logged
    text `shouldSatisfy` T.isInfixOf "$log.takesIndirectDamage"

  it "an enemy's piercing attack discards the tough status card and deals its damage; a ranged one ignores retaliate" do
    tg <- testGame ["black-panther"]
    let you = player1 tg
    inGame tg do
      changeForm you
      vid <- villainIdNow
      resolve [IdentityMessage you IdentityBecomeTough]
      let blow = (attack (EnemyVillainId vid) (IdentityCharacter you) 3) {attackPiercing = True, attackRanged = True}
      resolve [IdentityMessage you (IdentityWasAttacked blow)]
      tough <- (.playerIdentityTough) <$> playerAttrs you
      damage <- damageOf you
      villainDamage' <- villainDamageNow
      liftIO $ (tough, damage, villainDamage') `shouldBe` (False, 3, 0)

  it "a player's piercing attack discards the enemy's tough status card and deals its damage; a ranged one ignores retaliate" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainBecomeTough, VillainMessage vid (VillainDamaged (toSource you) (withPiercing $ toDamage 2 FromAbility))]
      tough <- (^. toughL) . toAttrs <$> getVillain
      villainDamage' <- villainDamageNow
      liftIO $ (tough, villainDamage') `shouldBe` (False, 2)
      -- Whiplash has retaliate 1: War Machine takes his consequential damage from each attack, and the retaliate only from the one not ranged
      warMachine <- allyIdOf <$> putIntoPlay you "01030"
      whiplash <- addMinion you Cards.whiplash
      let hit = toDamage 1 (FromAllyAttack warMachine)
      resolve [AllyMessage warMachine (AllyAttackLands (EnemyMinionId whiplash) (withRanged hit))]
      afterRanged <- (.allyDamage) <$> allyIn "01030"
      resolve [AllyMessage warMachine (AllyAttackLands (EnemyMinionId whiplash) hit)]
      afterPlain <- (.allyDamage) <$> allyIn "01030"
      liftIO $ (afterRanged, afterPlain) `shouldBe` (1, 3)

  it "a villainous minion is given a boost card as it schemes, its icons added" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      mercenary <- addMinion you Cards.hydraMercenary
      withGame_ $ entitiesL . minionsL . ix mercenary %~ overMinion \a -> a {minionCardDef = Cards.hydraMercenary {cdKeywords = singleton Villainous}}
      -- Whiplash has two boost icons; the mercenary schemes for 0
      [boost] <- stackEncounterDeck [Cards.whiplash]
      before <- mainSchemeThreatNow
      resolve [MinionMessage mercenary MinionSchemes]
      after <- mainSchemeThreatNow
      discarded <- encounterDiscardNow
      liftIO do
        after `shouldBe` before + 2
        discarded `shouldContain` [boost]
