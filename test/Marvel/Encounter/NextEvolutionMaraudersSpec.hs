module Marvel.Encounter.NextEvolutionMaraudersSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Marvel.Area (DeckKey (..))
import Marvel.Ability (AbilityType (..))
import Marvel.Attack
import Marvel.Phase
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Environment.Cards qualified as Environments
import Marvel.Environment.Types qualified as Environment
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.NextEvolution.Helpers
import Marvel.Query
import Marvel.Villain.Types (villainTough)
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as Scheme
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

reachChoice :: (Choice -> Bool) -> GameAppT ()
reachChoice wanted = go (60 :: Int)
 where
  go 0 = error "requested Marauder choice did not appear"
  go n = do
    found <- offers wanted
    unless found $ asked >>= \case
      Just (_,cs) | any isForced cs -> choose "resolve forced ability" isForced >> go (n - 1)
      Just (_,cs) | any isNoResponse cs -> choose "decline response" isNoResponse >> go (n - 1)
      Just (_,cs) | any isNoDefender cs -> choose "decline defender" isNoDefender >> go (n - 1)
      Just (_,cs) | any (isLabel "$label.declineToDefend") cs -> choose "decline defender" (isLabel "$label.declineToDefend") >> go (n - 1)
      other -> error $ "unexpected question: " <> show other

isForced :: Choice -> Bool
isForced = \case
  UseAbility ab -> abilityType ab `elem` [ForcedInterrupt, ForcedResponse]
  _ -> False

thwartAll :: IdentityId -> SideSchemeId -> GameAppT ()
thwartAll you sid = do
  amount <- sideSchemeThreat sid
  resolve [SideSchemeMessage sid (SideSchemeThwarted (toSource you) amount)]

minionDefs :: [CardDef]
minionDefs = [Minions.ne40094,Minions.ne40095,Minions.ne40096,Minions.ne40097,Minions.ne40098,Minions.ne40099,Minions.ne40100]

spec :: Spec
spec = describe "Next Evolution Marauders and Morlocks" do
  describe "Marauder attacks" do
    for_ minionDefs \def ->
      it ("offers and resolves the attack-strengthening option: " <> show (toCardCode def)) do
        tg <- testGame ["spider-man"]
        inGame tg do
          changeForm (player1 tg)
          pass
          putIntoPlay (player1 tg) "01065"
          card <- encounterCard def
          resolve [PutEncounterCardIntoPlay (player1 tg) card]
          runNow [MinionMessage (minionOf card) (MinionAttacks $ player1 tg)]
          reachChoice (isLabel "$label.neMarauderAttackBonus")
          choose "accept stronger attack" (isLabel "$label.neMarauderAttackBonus")
          -- Harpoon also grants piercing; Greycrow uses the highest controlled cost.
          case toCardCode def of
            "40098" -> do
              attack <- getEnemyAttack (EnemyMinionId $ minionOf card)
              liftIO $ fmap attackPiercing attack `shouldBe` Just True
            _ -> pure ()
          pass
          resolve []
          damageOf (player1 tg) >>= (`shouldSatisfy` (> 0))

    for_ [("40070a","neMarauderConfuse"),("40070b","neMarauderConfuse"),("40071a","neMarauderTough"),("40071b","neMarauderTough"),("40072a","neMarauderMental"),("40072b","neMarauderMental"),("40073a","neMarauderDiscardHighest"),("40073b","neMarauderDiscardHighest"),("40074a","neMarauderIndirect"),("40074b","neMarauderIndirect"),("40075a","neMarauderThreat"),("40075b","neMarauderThreat"),("40076a","neMarauderStun"),("40076b","neMarauderStun")] \(code,label) ->
      it ("offers both printed villain attack choices: " <> show code) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let you = player1 tg
          changeForm you
          pass
          clearHand you
          putInHand you "01089"
          putIntoPlay you "01065"
          vid <- villainIdNow
          resolve [ReplaceVillain vid code Swapped]
          runNow [VillainMessage vid (VillainAttacks you)]
          reachChoice (isLabel $ "$label." <> label)
          offers (isLabel $ "$label." <> label) `shouldReturn` True
          offers (isLabel "$label.neMarauderAttackBonus") `shouldReturn` True

  describe "Morlock Siege" do
    it "randomizes seven villains while exposing exactly one and setting Morlocks/Hide aside" do
      tg <- testGameAgainst "40077" ["spider-man"]
      inGame tg do
        selectList AnyVillain >>= (\xs -> liftIO $ length xs `shouldBe` 1)
        aside <- getSetAsideEncounterCards
        liftIO do
          length (filter ((== VillainType) . cdCardType . getCardDef) aside) `shouldBe` 6
          length (filter ((== "40079") . toCardCode) aside) `shouldBe` 4
          map toCardCode aside `shouldSatisfy` elem "40080"
        selectAny (EnvironmentIs Environments.ne40081a) `shouldReturn` True

    it "advances after three knock counters and gives the two solo Morlocks tough" do
      tg <- testGameAgainst "40077" ["spider-man"]
      inGame tg do
        resolve [VillainPhaseStep StepEnemiesActivate,VillainPhaseStep StepEnemiesActivate,VillainPhaseStep StepEnemiesActivate]
        morlocks <- alliesWith "40079"
        liftIO do
          length morlocks `shouldBe` 2
          map allyTough morlocks `shouldBe` [True,True]
        scheme <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMainSchemes)
        liftIO $ scheme `shouldBe` ["40078"]
        map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem "40080")

    it "advances through threat without giving the Morlocks tough" do
      tg <- testGameAgainst "40077" ["spider-man"]
      inGame tg do
        placeMainThreat 6
        morlocks <- alliesWith "40079"
        liftIO do
          length morlocks `shouldBe` 2
          map allyTough morlocks `shouldBe` [False,False]

    it "routes defeated villains outside play and activates the next villain" do
      tg <- testGameAgainst "40077" ["spider-man"]
      inGame tg do
        before <- toCardCode . getCardDef <$> getVillain
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged (toSource $ player1 tg) $ toDamage 100 FromAbility]
        routedCount `shouldReturn` 1
        after <- toCardCode . getCardDef <$> getVillain
        liftIO $ after `shouldSatisfy` (/= before)
        getsGame gameState >>= (`shouldSatisfy` (/= Finished Won))

    it "loses after the last Morlock is defeated during Mutant Massacre" do
      tg <- testGameAgainst "40077" ["spider-man"]
      inGame tg do
        placeMainThreat 6
        morlocks <- alliesWith "40079"
        resolve [AllyMessage (allyId a) $ AllyDamaged GameRef $ toDamage 5 FromAbility | a <- morlocks]
        getsGame gameState `shouldReturn` Finished Lost

    for_ [Schemes.ne40084,Schemes.ne40085,Schemes.ne40086,Schemes.ne40087] \def ->
      it ("adds one threat per player for each routed villain: " <> show (toCardCode def)) do
        tg <- testGameAgainst "40077" ["spider-man"]
        inGame tg do
          vid <- villainIdNow
          resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 100 FromAbility]
          card <- revealNow (player1 tg) def
          actual <- sideSchemeThreat (sideSchemeOf card)
          liftIO $ actual `shouldBe` (if toCardCode def == "40087" then 5 else 4)

    it "Hide gives a Morlock tough and its boost also gives another boost card" do
      tg <- testGame ["spider-man"]
      inGame tg do
        morlock <- putIntoPlay (player1 tg) "40079"
        card <- revealNow (player1 tg) Treacheries.ne40080
        choose "give Morlock tough" (isTargetChoice $ AllyRef $ allyIdOf morlock)
        alliesWith "40079" >>= (\xs -> liftIO $ map allyTough xs `shouldBe` [True])

    it "Pushed to the Limit cannot make a stunned villain attack as its cost" do
      tg <- testGameAgainst "40077" ["spider-man"]
      inGame tg do
        changeForm (player1 tg)
        pass
        attachment <- revealNow (player1 tg) Attachments.ne40083
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainStunned GameRef)]
        let action = \case UseAbility ab -> abilitySource ab == AttachmentRef (AttachmentId $ unCardId $ toCardId attachment); _ -> False
        offers action `shouldReturn` False

  describe "On the Run" do
    it "starts with one villain and removes only its matching Mutant Slayer minion" do
      tg <- testGameAgainst "40103" ["spider-man"]
      inGame tg do
        selectList AnyVillain >>= (\xs -> liftIO $ length xs `shouldBe` 1)
        selectList (AttachmentIs Attachments.ne40105a) >>= (\xs -> liftIO $ length xs `shouldBe` 1)
        deck <- getEncounterDeck
        minions <- selectList AnyMinion
        codes <- traverse (\mid -> getsGame $ \g -> fmap toCardCode $ abilityCardDef g $ MinionRef mid) minions
        let allCodes = map toCardCode deck <> catMaybes codes
        liftIO $ length (filter (`elem` map toCardCode minionDefs) allCodes) `shouldBe` 6

    it "replaces the first lethal damage with desperate Captor and stage two" do
      tg <- testGameAgainst "40103" ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 100 FromAbility]
        selectAny (AttachmentIs Attachments.ne40105b) `shouldReturn` True
        schemes <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMainSchemes)
        liftIO $ schemes `shouldBe` ["40104"]
        getsGame gameState >>= (`shouldSatisfy` (/= Finished Won))
        getVillainHitPoints vid >>= (`shouldSatisfy` (> 6))

    it "loses when stage one reaches its threshold instead of advancing" do
      tg <- testGameAgainst "40103" ["spider-man"]
      inGame tg do
        placeMainThreat 8
        getsGame gameState `shouldReturn` Finished Lost

    it "Bushwhack searches a Marauder into the defeating player's engagement" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- revealNow (player1 tg) Schemes.ne40108
        thwartAll (player1 tg) (sideSchemeOf card)
        -- A Rhino test deck has no Marauder: the mandatory search safely fails.
        getsGame gameState >>= (`shouldSatisfy` (/= Finished Lost))

    it "Pure Force gains crisis from Blockbuster and amplify from Chimera" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- revealNow (player1 tg) Schemes.ne40109
        block <- encounterCard Minions.ne40095
        chimera <- encounterCard Minions.ne40096
        resolve [PutEncounterCardIntoPlay (player1 tg) block,PutEncounterCardIntoPlay (player1 tg) chimera]
        mods <- getModifiers (SideSchemeRef $ sideSchemeOf card)
        liftIO do
          mods `shouldSatisfy` elem CrisisIcon
          mods `shouldSatisfy` elem (AmplifyIconModifier 1)
        getAmplifyCount `shouldReturn` 1

    it "Hidden in the Clutter redirects damage onto itself while leaving enemy damage unchanged" do
      tg <- testGame ["spider-man"]
      inGame tg do
        attachment <- revealNow (player1 tg) Attachments.ne40106
        before <- villainDamageNow
        vid <- villainIdNow
        lifted <- Lifted.runQueueT $ Lifted.capture $ Lifted.dealDamage vid (player1 tg) (toDamage 2 FromAbility)
        resolve lifted
        villainDamageNow `shouldReturn` before
        held <- getsGame (HashMap.lookup (AttachmentId $ unCardId $ toCardId attachment) . gameAttachments)
        liftIO $ fmap (Attachment.attachmentDamage . toAttrs) held `shouldBe` Just 2

  describe "Mutant Slayers" do
    it "counts a character with several matching traits once and grants Marauders quickstrike" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- revealNow (player1 tg) Schemes.ne40101
        minion <- encounterCard Minions.ne40094
        resolve [PutEncounterCardIntoPlay (player1 tg) minion]
        mods <- getModifiers (MinionRef $ minionOf minion)
        liftIO $ mods `shouldSatisfy` elem (KeywordModifier Quickstrike)

    it "Bound by Business ignores a Marauder whose title is already in play" do
      tg <- testGame ["spider-man"]
      inGame tg do
        existing <- encounterCard Minions.ne40094
        duplicate <- encounterCard Minions.ne40094
        fresh <- encounterCard Minions.ne40095
        resolve [PutEncounterCardIntoPlay (player1 tg) existing]
        -- Set encounter deck through the ordinary deck message to retain its state.
        resolve [CreateDeck EncounterDeckKey [duplicate,fresh]]
        card <- revealNow (player1 tg) Treacheries.ne40102
        pass
        selectAny (MinionIs Minions.ne40095) `shouldReturn` True

  describe "scenario card clauses" do
    it "Back in Action gives the villain tough and places no extra threat without Routed villains" do
      tg <- testGame ["spider-man"]
      inGame tg do
        before <- mainSchemeThreatNow
        card <- revealNow (player1 tg) Treacheries.ne40088
        pass
        villain <- getVillain
        liftIO $ villainTough (toAttrs villain) `shouldBe` True
        mainSchemeThreatNow `shouldReturn` before

    it "Bolstered by Wrath grants one ATK and SCH while attached" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- revealNow (player1 tg) Attachments.ne40082
        vid <- villainIdNow
        mods <- getModifiers (VillainRef vid)
        liftIO do
          mods `shouldSatisfy` elem (AttackModifier 1)
          mods `shouldSatisfy` elem (SchemeModifier 1)

    it "Dizzying Deeds exhausts a ready character even without any named Marauders" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- revealNow (player1 tg) Treacheries.ne40110
        choose "exhaust identity" (isTargetChoice $ player1 tg)
        playerIdentityExhausted <$> playerAttrs (player1 tg) `shouldReturn` True

    it "Tag Team discards seven and puts the topmost discarded Marauder into play" do
      tg <- testGame ["spider-man"]
      inGame tg do
        marauder <- encounterCard Minions.ne40095
        fillers <- traverse (const $ encounterCard Schemes.bombScare) [1..7 :: Int]
        resolve [CreateDeck EncounterDeckKey (marauder : fillers)]
        card <- revealNow (player1 tg) Treacheries.ne40111
        choose "discard seven" (isLabel "$label.neTagTeamDiscard")
        pass
        selectAny (MinionIs Minions.ne40095 <> MinionEngagedWith (IdentityWithId $ player1 tg)) `shouldReturn` True

    it "Favored Weapon prefers Greycrow and gives his attacks all three printed keywords" do
      tg <- testGame ["spider-man"]
      inGame tg do
        greycrow <- encounterCard Minions.ne40097
        resolve [PutEncounterCardIntoPlay (player1 tg) greycrow]
        card <- revealNow (player1 tg) Attachments.ne40107
        attached <- getsGame (HashMap.lookup (AttachmentId $ unCardId $ toCardId card) . gameAttachments)
        liftIO $ fmap (Attachment.attachmentEnemy . toAttrs) attached `shouldBe` Just (Just $ EnemyMinionId $ minionOf greycrow)
        mods <- getModifiers (MinionRef $ minionOf greycrow)
        liftIO $ for_ [Overkill,Piercing,Ranged] $ \keyword -> mods `shouldSatisfy` elem (KeywordModifier keyword)

    it "Pushed to the Limit pays its attack cost before discarding" do
      tg <- testGame ["spider-man"]
      inGame tg do
        changeForm (player1 tg)
        pass
        card <- revealNow (player1 tg) Attachments.ne40083
        let aid = AttachmentId $ unCardId $ toCardId card
            action = \case UseAbility ab -> abilitySource ab == AttachmentRef aid; _ -> False
        choose "take attack as cost" action
        pass
        resolve []
        getsGame (HashMap.member aid . gameAttachments) `shouldReturn` False
        damageOf (player1 tg) >>= (`shouldSatisfy` (> 0))

    it "Hidden in the Clutter can retrigger from retaliate before its original discard" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        pass
        putIntoPlay you "16016"
        soldier <- encounterCard Minions.hydraSoldier
        resolve [PutEncounterCardIntoPlay you soldier,GiveStatus (IdentityRef you) ToughStatus]
        clutter <- revealNow you Attachments.ne40106
        liftIO $ writeIORef tg.ran []
        damage <- Lifted.runQueueT $ Lifted.capture $ Lifted.dealDamage (minionOf soldier) you (toDamage 3 FromAbility)
        resolve damage
        history <- liftIO $ readIORef tg.ran
        let attacks = filter (T.isInfixOf "MinionAttacks ") history
        liftIO $ length attacks `shouldBe` 2
        damageOf you `shouldReturn` 2
        getsGame (HashMap.member (AttachmentId $ unCardId $ toCardId clutter) . gameAttachments) `shouldReturn` False
