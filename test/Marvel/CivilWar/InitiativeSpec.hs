module Marvel.CivilWar.InitiativeSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (attachmentIdOf)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Damage
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers
import Marvel.Minion.Cards qualified as M
import Marvel.Trait (Trait (Avenger))
import TestImport

spec :: Spec
spec = describe "Civil War Initiative" do
  it "Drafted reveals a discarded minion, then adds two hit points and Avenger" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      [found] <- stackEncounterDeck [M.hydraBomber]
      drafted <- encounterCard C.cw56080
      resolve [RevealEncounterCard you drafted]
      let mid = MinionId $ unCardId $ ecCardId found
      getAttachedCharacter (attachmentIdOf drafted) `shouldReturn` Just (MinionCharacter mid)
      damageOf you `shouldReturn` 2
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 4
      minionMatches (MinionWithTrait Avenger) mid `shouldReturn` True

  it "the opponent chooses a minion on their board for Drafted without repeating its reveal ability" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      leader <- villainIdNow
      opponent <- addSecondBoard tg
      recruit <- encounterCard C.cw56078
      drafted <- encounterCard C.cw56080
      ownFiller <- encounterCard M.hydraMercenary
      otherFiller <- encounterCard M.hydraSoldier
      messages <- L.runQueueT $ L.capture $ do
        L.createDeck (L.VillainDeckKey leader) [drafted,ownFiller]
        L.createDeck (L.VillainDeckKey opponent) [recruit,otherFiller]
      resolve messages
      resolve [RevealEncounterCard (player2 tg) recruit]
      let mid = MinionId $ unCardId $ ecCardId recruit
      start [RevealEncounterCard (player1 tg) drafted]
      fmap fst <$> asked `shouldReturn` Just (player2 tg)
      choose "take Doc Samson" $ \case TargetLabel target _ -> target == MinionRef mid; _ -> False
      getMinionEngagedPlayer mid `shouldReturn` Just (player1 tg)
      getAttachedCharacter (attachmentIdOf drafted) `shouldReturn` Just (MinionCharacter mid)
      identityMatches ConfusedIdentity (player1 tg) `shouldReturn` False
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 6
      defeat <- L.runQueueT $ L.capture $ L.dealDamage mid (player1 tg) (toDamage 10 FromAbility)
      resolve defeat
      asYou (player1 tg) getEncounterDiscard >>= (`shouldSatisfy` elem drafted)
      asYou (player1 tg) getEncounterDiscard >>= (`shouldSatisfy` notElem recruit)
      asYou (player2 tg) getEncounterDiscard >>= (`shouldSatisfy` elem recruit)
      asYou (player2 tg) getEncounterDiscard >>= (`shouldSatisfy` notElem drafted)

  it "The Fifty State Initiative shuffles only Avenger minions from the encounter discard" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      avenger <- encounterCard C.cw56078
      other <- encounterCard M.hydraSoldier
      side <- encounterCard C.cw56082
      resolve [DiscardedCard (EncounterCard avenger),DiscardedCard (EncounterCard other),RevealEncounterCard you side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      sideSchemeThreat sid `shouldReturn` 4
      resolve [SideSchemeMessage sid DefeatSideScheme]
      deck <- getEncounterDeck
      discard <- getEncounterDiscard
      deck `shouldSatisfy` elem avenger
      discard `shouldSatisfy` notElem avenger
      discard `shouldSatisfy` elem other

  it "Drafted discards itself when the cooperative search finds no minion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      onlyCard <- encounterCard C.cw56076
      deck <- L.runQueueT $ L.capture $ L.createDeck L.EncounterDeckKey [onlyCard]
      resolve deck
      drafted <- encounterCard C.cw56080
      resolve [RevealEncounterCard (player1 tg) drafted]
      getAttachedCharacter (attachmentIdOf drafted) `shouldReturn` Nothing
      getEncounterDiscard >>= (`shouldSatisfy` elem drafted)
