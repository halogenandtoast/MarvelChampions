module Marvel.CivilWar.PaladinSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attack (Attack (..))
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers qualified as CW
import Marvel.CivilWar.LeaderSpec (attachmentIdOf,withLeader,leaderHp)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import TestImport

spec :: Spec
spec = describe "Civil War Paladin" do
  it "Paladin enters tough and lets a registered player choose a hand discard" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      kept <- putInHand you "01088"
      discarded <- putInHand you "01086"
      card <- encounterCard C.cw56117
      start [RevealEncounterCard you card]
      choose "discard First Aid" (isTargetChoice discarded)
      pass
      let mid = MinionId $ unCardId $ ecCardId card
      minionTough . toAttrs . fromMaybe (error "missing Paladin") . lookup mid <$> getsGame gameMinions `shouldReturn` True
      handOf you `shouldReturn` [kept]
      fmap (map pcCardId) (discardOf you) `shouldReturn` [pcCardId discarded]

  it "Paladin uses Quickstrike against a hero" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- addMinion you C.cw56117
      damageOf you `shouldReturn` 2

  it "Paladin randomly discards an Unregistered player's card without a choice" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      obligation <- encounterCard C.cw56114
      resolve [RevealEncounterCard you obligation]
      clearHand you
      discarded <- putInHand you "01088"
      _ <- addMinion you C.cw56117
      handOf you `shouldReturn` []
      fmap (map pcCardId) (discardOf you) `shouldReturn` [pcCardId discarded]
      offers (== EndTurn) `shouldReturn` True

  it "the pistol attaches to Paladin in preference to the leader" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) C.cw56117
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard (player1 tg) card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (MinionCharacter mid)

  it "the pistol attaches to the enemy leader without Paladin" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard (player1 tg) card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)

  it "the pistol uses otherwise when Paladin cannot have attached encounter cards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you C.cw56117
      block <- L.runQueueT $ L.capture $ L.lastingModifiers GameRef (SourceEntity $ MinionRef mid) [CannotHaveAttachedEncounterCards] False
      resolve block
      vid <- villainIdNow
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard you card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)

  it "the pistol does not attach to a Paladin on the other board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      own <- villainIdNow
      _ <- addSecondBoard tg
      _ <- addMinion q C.cw56117
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard p card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter own)

  it "the pistol grants ranged and overkill for one minion attack then discards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you C.cw56117
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard you card]
      retaliation <- L.runQueueT $ L.capture $ L.lastingModifiers GameRef (SourceEntity $ IdentityRef you) [KeywordModifier $ K.Retaliate 1] False
      resolve retaliation
      changeForm you
      start [MinionMessage mid $ MinionAttacks you]
      whenM (offers $ isAbilityOf $ attachmentIdOf card) $ choose "resolve the pistol" (isAbilityOf $ attachmentIdOf card)
      let decline = whenM (offers isNoResponse) $ choose "decline a response" isNoResponse >> decline
      decline
      attack <- getEnemyAttack $ EnemyMinionId mid
      fmap attackRanged attack `shouldBe` Just True
      fmap attackOverkill attack `shouldBe` Just True
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (MinionCharacter mid)
      choose "do not defend" isNoDefender
      pass
      damageOf you `shouldReturn` 4
      minionTough . toAttrs . fromMaybe (error "missing Paladin") . lookup mid <$> getsGame gameMinions `shouldReturn` True
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      resolve [MinionMessage mid $ MinionAttacks you]
      damageOf you `shouldReturn` 6
      minionTough . toAttrs . fromMaybe (error "missing Paladin") . lookup mid <$> getsGame gameMinions `shouldReturn` False

  it "the pistol's overkill spills past an ally defender" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you C.cw56117
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard you card]
      cat <- putInHand you "01002"
      put <- L.runQueueT $ L.capture $ L.putIntoPlay you cat
      resolve put
      changeForm you
      start [MinionMessage mid $ MinionAttacks you]
      whenM (offers $ isAbilityOf $ attachmentIdOf card) $ choose "resolve the pistol" (isAbilityOf $ attachmentIdOf card)
      let decline = whenM (offers isNoResponse) $ choose "decline a response" isNoResponse >> decline
      decline
      choose "Black Cat defends" (\case AllyDefend aid _ -> aid == allyIdOf cat; _ -> False)
      pass
      damageOf you `shouldReturn` 2
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing

  it "the pistol stays attached through a scheme activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard (player1 tg) card]
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      resolve [VillainMessage vid VillainSchemes]
      mainSchemeThreat `shouldReturn` 2
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)

  it "the pistol supports its leader's attack and discards afterward" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard you card]
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary,M.hydraSoldier]
      changeForm you
      resolve [VillainMessage vid $ VillainAttacks you]
      damageOf you `shouldReturn` 4
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      resolve [VillainMessage vid $ VillainAttacks you]
      damageOf you `shouldReturn` 6

  it "the pistol still modifies a Paladin controlled as an ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you C.cw56117
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard you card]
      control <- L.runQueueT $ L.capture $ L.takeControlOfMinionWithConsequential you mid 0
      resolve control
      let aid = AllyId $ unMinionId mid
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (AllyCharacter aid)
      target <- addMinion you M.hydraMercenary
      vid <- villainIdNow
      before <- leaderHp vid
      start [AllyMessage aid AllyAttacked]
      choose "Paladin attacks the guard" (isTargetChoice target)
      whenM (offers $ isAbilityOf $ attachmentIdOf card) $ choose "resolve the pistol" (isAbilityOf $ attachmentIdOf card)
      pass
      minionDamageOf target `shouldReturn` Nothing
      leaderHp vid `shouldReturn` (before - 1)
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      getsGame (HashMap.member aid . gameAllies) `shouldReturn` True


  it "a stunned minion does not spend its pistol" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you C.cw56117
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard you card]
      changeForm you
      resolve [GiveStatus (MinionRef mid) StunnedStatus,MinionMessage mid $ MinionAttacks you]
      damageOf you `shouldReturn` 0
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (MinionCharacter mid)


  it "the pistol grants ranged during a cross-board leader attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      attacker <- withLeader "57040"
      defender <- addSecondBoard tg
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard you card]
      retaliation <- L.runQueueT $ L.capture $ L.lastingModifiers GameRef (SourceEntity $ VillainRef defender) [KeywordModifier $ K.Retaliate 2] False
      resolve retaliation
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      beforeAttacker <- leaderHp attacker
      beforeDefender <- leaderHp defender
      attack <- L.runQueueT $ L.capture $ CW.leaderAttacksLeader attacker defender
      resolve attack
      leaderHp attacker `shouldReturn` beforeAttacker
      leaderHp defender `shouldReturn` (beforeDefender - 4)
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing

  it "the pistol is discarded without an attachment question when neither host can accept it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      block <- L.runQueueT $ L.capture $ L.lastingModifiers GameRef (SourceEntity $ VillainRef vid) [CannotHaveAttachedEncounterCards] False
      resolve block
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard you card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      fmap (any ((== ecCardId card) . ecCardId)) getEncounterDiscard `shouldReturn` True
      offers (== EndTurn) `shouldReturn` True


  it "a newly revealed pistol recognizes the encounter Paladin already controlled as an ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you C.cw56117
      control <- L.runQueueT $ L.capture $ L.takeControlOfMinionWithConsequential you mid 0
      resolve control
      card <- encounterCard C.cw56118
      resolve [RevealEncounterCard you card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (AllyCharacter $ AllyId $ unMinionId mid)
