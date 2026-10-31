module Marvel.Rules.PhoenixControlSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Modifier (Modifier (Blank))
import Marvel.Keyword (Keyword (Steady))
import Marvel.Ally.Cards qualified as Allies
import Marvel.Matchers (EntityMatcher (UpgradeEntity), UpgradeMatcher (UpgradeWithId))
import Marvel.Ally.Types qualified as Ally
import Marvel.Card qualified as Card
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Minion.Minions.MutantGenesis (MutantGenesisMinion (..))
import Marvel.Trait (Trait (Controlled))
import Marvel.Hp (HP (..))
import Marvel.Stats
import Marvel.Window qualified as W
import TestImport

spec :: Spec
spec = describe "Phoenix controlled minions" do
  it "keeps the physical card, damage, status, and printed powers while changing type" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.weaponsRunner
      resolve [MinionMessage mid (MinionDamaged (toSource p) (toDamage 1 FromAbility)), GiveStatus (MinionRef mid) ToughStatus]
      resolve [TakeControlOfMinion p mid]
      a <- allyIn "01121"
      (Ally.allyThwart a, Ally.allyAttack a, Ally.allyHitPoints a, Ally.allyDamage a, Ally.allyTough a)
        `shouldBe` (Thw 1, Atk 1, HP 2, 1, True)
      cdKeywords (Ally.allyCardDef a) `shouldBe` mempty
      cdTraits (Ally.allyCardDef a) `shouldSatisfy` member Controlled
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
      case Card.toCard a of
        EncounterCard c -> toCardCode c `shouldBe` "01121"
        _ -> error "controlled encounter minion lost its encounter provenance"
      Just ally <- getsGame (HashMap.lookup (Ally.allyId a) . gameAllies)
      case fromJSON (toJSON ally) of
        Success (loaded :: Ally) -> loaded `shouldBe` ally
        Error err -> error err

  it "uses its printed SCH to thwart and takes one consequential damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.weaponsRunner
      resolve [TakeControlOfMinion p mid]
      placeMainThreat 2
      a <- allyIn "01121"
      before <- mainSchemeThreatNow
      choose "thwart with the controlled minion" (isAllyThwart a)
      chooseMainScheme
      mainSchemeThreatNow `shouldReturn` before - 1
      (Ally.allyDamage <$> allyIn "01121") `shouldReturn` 1

  it "discards defeated controlled minions to the encounter discard pile" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraMercenary
      resolve [TakeControlOfMinion p mid]
      let aid = AllyId $ unMinionId mid
      resolve [AllyMessage aid AllyDefeated]
      discarded <- encounterDiscardCodes
      discarded `shouldSatisfy` elem (toCardCode Minions.hydraMercenary)
      cards <- discardOf p
      map pcCardDef cards `shouldSatisfy` notElem Minions.hydraMercenary

  it "restores the original minion and its current damage when control ends" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraMercenary
      resolve [TakeControlOfMinion p mid]
      let aid = AllyId $ unMinionId mid
      resolve [AllyMessage aid (AllyDamaged (toSource p) (toDamage 1 FromAbility)), RestoreControlledMinion aid]
      Just m <- getsGame (fmap toAttrs . HashMap.lookup mid . gameMinions)
      Minion.minionDamage m `shouldBe` 1
      Minion.minionCardDef m `shouldBe` Minions.hydraMercenary
      getsGame (HashMap.member aid . gameAllies) `shouldReturn` False

  it "plays Mind Control on a non-Elite minion and restores it when the upgrade is discarded" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraMercenary
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      a <- allyIn (toCardCode Minions.hydraMercenary)
      Ally.allyThwart a `shouldBe` Thw 0
      resolve [RemoveFromPlay (UpgradeRef $ upgradeIdOf control)]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` True
      getsGame (HashMap.member (Ally.allyId a) . gameAllies) `shouldReturn` False

  it "does not recreate a defeated host when Mind Control leaves with it" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraMercenary
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      resolve [AllyMessage (AllyId $ unMinionId mid) AllyDefeated]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
      discardedPlayerCards <- map toCardCode <$> discardOf p
      discardedPlayerCards `shouldSatisfy` elem "34009"
      discarded <- encounterDiscardCodes
      discarded `shouldSatisfy` elem (toCardCode Minions.hydraMercenary)

  it "returns a controlled encounter minion to the encounter discard rather than a player hand" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraMercenary
      resolve [TakeControlOfMinion p mid]
      before <- length <$> handOf p
      resolve [ReturnToHand (AllyRef $ AllyId $ unMinionId mid)]
      (length <$> handOf p) `shouldReturn` before
      discarded <- encounterDiscardCodes
      discarded `shouldSatisfy` elem (toCardCode Minions.hydraMercenary)

  it "preserves extra status cards through control and removes all stun cards on an attack attempt" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraMercenary
      withGame_ $ entitiesL . minionsL . ix mid %~ Minion.overMinion
        (\m -> m {minionCardDef = (minionCardDef m) {cdKeywords = fromList [Steady]}})
      resolve [GiveStatus (MinionRef mid) StunnedStatus, GiveStatus (MinionRef mid) StunnedStatus]
      resolve [TakeControlOfMinion p mid]
      a <- allyIn (toCardCode Minions.hydraMercenary)
      Ally.allyAdditionalStun a `shouldBe` True
      choose "attempt a stunned ally attack" (isAllyAttack a)
      a' <- allyIn (toCardCode Minions.hydraMercenary)
      (Ally.allyStunned a', Ally.allyAdditionalStun a') `shouldBe` (False, False)
      resolve [RestoreControlledMinion (Ally.allyId a)]
      Just m <- getsGame (fmap toAttrs . HashMap.lookup mid . gameMinions)
      (minionStunned m, minionAdditionalStun m) `shouldBe` (False, False)

  it "uses the original SCH when a controlled encounter minion is Beguiled" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraMercenary
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      _ <- revealNow p Attachments.beguiled
      pass
      Just m <- getsGame (fmap toAttrs . HashMap.lookup mid . gameMinions)
      Minion.minionScheme m `shouldBe` Sch 0
      Minion.minionCardDef m `shouldBe` Minions.hydraMercenary
      getModifiers (MinionRef mid) >>= \mods -> mods `shouldSatisfy` elem Blank
      Just beguiled <- getsGame (headMay . HashMap.keys . gameAttachments)
      resolve [RemoveFromPlay (AttachmentRef beguiled)]
      a <- allyIn (toCardCode Minions.hydraMercenary)
      Ally.allyController a `shouldBe` p

  it "keeps Beguiled attached when Mind Control wins the later type change" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01067"
      _ <- revealNow p Attachments.beguiled
      pass
      let mid = MinionId $ unAllyId $ allyIdOf ally
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      a <- allyIn "01067"
      (Ally.allyThwart a, Ally.allyAttack a) `shouldBe` (Thw 2, Atk 1)
      attached <- getsGame (filter ((== Just (Ally.allyId a)) . Attachment.attachmentAlly . toAttrs) . HashMap.elems . gameAttachments)
      length attached `shouldBe` 1
      cdTraits (Ally.allyCardDef a) `shouldSatisfy` member Controlled

  it "defeats a reverted minion when its damage exceeds its original HP after losing an ally Training bonus" do
    tg <- testGame ["phoenix"]
    let p = player1 tg
    inGame tg do
      cyclops <- putIntoPlay p "34003"
      _ <- revealNow p Attachments.beguiled
      pass
      let mid = MinionId $ unAllyId $ allyIdOf cyclops
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      training <- putInHand p "34016"
      payment <- putInHand p "01090"
      playOn training (allyIdOf cyclops) [payment]
      pass
      resolve [AllyMessage (allyIdOf cyclops) (AllyDamaged (toSource p) (toDamage 4 FromAbility))]
      a <- allyIn "34003"
      Ally.allyDamage a `shouldBe` 4
      resolve [RemoveFromPlay (UpgradeRef $ upgradeIdOf control)]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
      getsGame (HashMap.member (allyIdOf cyclops) . gameAllies) `shouldReturn` False
      discarded <- map toCardCode <$> discardOf p
      discarded `shouldSatisfy` elem "34003"
      discarded `shouldSatisfy` elem "34016"

  it "preserves exhaustion when a controlled ally is Beguiled and then becomes controlled again" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraMercenary
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      a <- allyIn (toCardCode Minions.hydraMercenary)
      choose "attack with controlled ally" (isAllyAttack a)
      chooseVillain
      _ <- revealNow p Attachments.beguiled
      pass
      Just m <- getsGame (fmap toAttrs . HashMap.lookup mid . gameMinions)
      Minion.minionExhausted m `shouldBe` True
      Just beguiled <- getsGame (headMay . HashMap.keys . gameAttachments)
      resolve [RemoveFromPlay (AttachmentRef beguiled)]
      a' <- allyIn (toCardCode Minions.hydraMercenary)
      Ally.allyExhausted a' `shouldBe` True
      offers (isAllyAttack a') `shouldReturn` False

  it "ends control while Mind Control is blank and reapplies it when the blanking effect expires" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraMercenary
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      a <- allyIn (toCardCode Minions.hydraMercenary)
      choose "attack before control is suppressed" (isAllyAttack a)
      chooseVillain
      -- Supply the normal temporary-blank effect with this upgrade as its target.
      resolve [CreatedEffect Allies.vivianIronheart (toSource p) (UpgradeEntity $ UpgradeWithId $ upgradeIdOf control)]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` True
      alliesWith (toCardCode Minions.hydraMercenary) `shouldReturn` []
      resolve [EndRound]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
      a' <- allyIn (toCardCode Minions.hydraMercenary)
      (Ally.allyExhausted a', Ally.allyDamage a') `shouldBe` (True, 1)

  it "keeps a controlled player ally's original owner and returns its original printed card to that owner's hand" do
    tg <- testGame ["phoenix", "spider-man"]
    inGame tg do
      [phoenix, owner] <- players
      cyclops <- putIntoPlay owner "34003"
      _ <- revealNow phoenix Attachments.beguiled
      pass
      let mid = MinionId $ unAllyId $ allyIdOf cyclops
      control <- putInHand phoenix "34009"
      energy <- putInHand phoenix "01088"
      genius <- putInHand phoenix "01089"
      playOn control mid [energy, genius]
      pass
      a <- allyIn "34003"
      (Ally.allyController a, Ally.allyOwner a) `shouldBe` (phoenix, owner)
      before <- length <$> handOf phoenix
      resolve [ReturnToHand (AllyRef $ allyIdOf cyclops)]
      (length <$> handOf phoenix) `shouldReturn` before
      returned <- filter ((== pcCardId cyclops) . pcCardId) <$> handOf owner
      map pcCardDef returned `shouldBe` [pcCardDef cyclops]
      map pcOwner returned `shouldBe` [Just owner]

  it "never resumes a removed Mind Control when its old blanking effect expires or a queued sync runs" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraMercenary
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      let uid = upgradeIdOf control
      resolve [CreatedEffect Allies.vivianIronheart (toSource p) (UpgradeEntity $ UpgradeWithId uid)]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` True
      resolve [RemoveFromPlay (UpgradeRef uid), EndRound, CardSpecific (UpgradeRef uid) "syncMindControl" (toJSON ())]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` True
      alliesWith (toCardCode Minions.hydraMercenary) `shouldReturn` []
      upgradesWith "34009" `shouldReturn` []

  it "a restored quickstrike minion engages its hero controller and attacks without entering play again" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.vulture
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      changeForm p
      pass
      liftIO $ writeIORef tg.ran []
      resolve [RemoveFromPlay (UpgradeRef $ upgradeIdOf control)]
      damageOf p `shouldReturn` 3
      ran <- liftIO $ readIORef tg.ran
      ran `shouldSatisfy` elem (tshow $ CheckWindows [W.Window W.When $ W.EngagedMinion p mid])
      ran `shouldSatisfy` elem (tshow $ CheckWindows [W.Window W.After $ W.EngagedMinion p mid])
      ran `shouldSatisfy` notElem (tshow $ CheckWindows [W.Window W.After $ W.MinionEnteredPlay mid])

  it "a restored quickstrike minion does not attack an alter-ego controller" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.vulture
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      resolve [RemoveFromPlay (UpgradeRef $ upgradeIdOf control)]
      damageOf p `shouldReturn` 0
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` True

  it "restoring a minion does not resolve its When Revealed ability again" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraBomber
      beforeDamage <- damageOf p
      beforeThreat <- mainSchemeThreatNow
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      resolve [RemoveFromPlay (UpgradeRef $ upgradeIdOf control)]
      damageOf p `shouldReturn` beforeDamage
      mainSchemeThreatNow `shouldReturn` beforeThreat
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` True

  it "a restored quickstrike minion already defeated by lost ally hit points never makes a ghost attack" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.vulture
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      suit <- putInHand p "12018"
      payment <- putInHand p "01090"
      playOn suit (AllyId $ unMinionId mid) [payment]
      pass
      resolve [AllyMessage (AllyId $ unMinionId mid) (AllyDamaged (toSource p) (toDamage 5 FromAbility))]
      changeForm p
      pass
      resolve [RemoveFromPlay (UpgradeRef $ upgradeIdOf control)]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
      getsGame (HashMap.member (AllyId $ unMinionId mid) . gameAllies) `shouldReturn` False
      damageOf p `shouldReturn` 0

  it "saves and restores a Mind Controlled Sentinel without losing its tuple-shaped minion state" do
    tg <- testGame ["phoenix"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      clearHand p
      mid <- addMinion p Minions.mgSentinelMarkIv
      Just original <- getsGame (HashMap.lookup mid . gameMinions)
      -- This ordinary Sentinel has tuple metadata (Nothing, Nothing), which must
      -- serialize before Phoenix captures its original entity for restoration.
      case fromJSON (toJSON original) of
        Success (loaded :: Minion) -> loaded `shouldBe` original
        Error err -> error err
      let metadata = (Just p, Just (p, 2 :: Natural, 3 :: Natural))
      withGame_ $ entitiesL . minionsL . at mid ?~
        Minion.Minion (MutantGenesisMinion (toAttrs original `With` metadata))
      control <- putInHand p "34009"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playOn control mid [energy, genius]
      pass
      Just controlled <- getsGame (HashMap.lookup (AllyId $ unMinionId mid) . gameAllies)
      -- Save/reload the controlled ally too: its snapshot embeds the complete
      -- original wrapper rather than only its public minion attributes.
      case fromJSON (toJSON controlled) of
        Success (loaded :: Ally) -> withGame_ $ entitiesL . alliesL . at (AllyId $ unMinionId mid) ?~ loaded
        Error err -> error err
      resolve [RemoveFromPlay (UpgradeRef $ upgradeIdOf control)]
      Just restored <- getsGame (HashMap.lookup mid . gameMinions)
      case toJSON restored of
        Object o -> KeyMap.lookup "_withMetadata" o `shouldBe` Just (toJSON metadata)
        _ -> error "restored minion must retain its attribute object"
      case fromJSON (toJSON restored) of
        Success (loaded :: Minion) -> loaded `shouldBe` restored
        Error err -> error err
      let a = toAttrs restored
      (minionScheme a, minionAttack a, minionHitPoints a) `shouldBe` (Sch 2, Atk 2, HP 4)
