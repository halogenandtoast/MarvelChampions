module Marvel.Encounter.CyclopsNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Card.Side (Side (A))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types qualified as Obligation
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Status
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = resolve [IdentityMessage p (ChangedToForm A)] >> clearHand p

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

attachmentEnemyOf :: EncounterCard -> GameAppT (Maybe EnemyId)
attachmentEnemyOf c = getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentOf c) . gameAttachments) >>= pure . join

heldAt :: EncounterCard -> GameAppT [PlayerCard]
heldAt c = getsGame (maybe [] (Obligation.obligationHeldCards . toAttrs) . HashMap.lookup (ObligationId $ unCardId $ ecCardId c) . gameObligations)

clearVisors :: IdentityId -> GameAppT ()
clearVisors p = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  let other = filter ((/= "33003") . toCardCode)
   in PlayerIdentity a { playerIdentityHand = Hand (other $ unHand a.playerIdentityHand)
                       , playerIdentityDeck = Deck (other $ unDeck a.playerIdentityDeck)
                       , playerIdentityDiscard = Discard (other $ unDiscard a.playerIdentityDiscard) }

spec :: Spec
spec = describe "Cyclops nemesis and obligation" do
  it "Mister Sinister enters tough, is stalwart, and has his printed stats" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Minions.misterSinister
      resolve [GiveStatus (MinionRef mid) StunnedStatus, GiveStatus (MinionRef mid) ConfusedStatus]
      a <- getsGame (toAttrs . fromMaybe (error "missing Sinister") . HashMap.lookup mid . gameMinions)
      minionTough a `shouldBe` True
      minionStunned a `shouldBe` False
      minionConfused a `shouldBe` False
      getEnemyPrintedAttack (EnemyMinionId mid) `shouldReturn` 1
      cdKeywords (getCardDef a) `shouldSatisfy` elem Villainous

  it "Mister Sinister receives a boost when scheming" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Minions.misterSinister
      _ <- setEncounterDeck [Treacheries.explosion, Treacheries.explosion] []
      before <- mainSchemeThreatNow
      resolve =<< L.runQueueT (L.capture $ L.minionSchemes mid)
      mainSchemeThreatNow `shouldReturn` (before + 3)

  it "Mister Sinister's boost stuns, then deals ability damage if already stunned" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      vid <- villainIdNow
      card <- encounterCard Minions.misterSinister
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      isStunned p `shouldReturn` True
      damageOf p `shouldReturn` 0
      other <- encounterCard Minions.misterSinister
      resolve [RevealBoostCard other (EnemyVillainId vid)]
      damageOf p `shouldReturn` 2

  it "Genetic Manipulation starts with two threat per player and one amplify" do
    tg <- testGame ["cyclops", "spider-man"]
    inGame tg do
      card <- revealNow (player1 tg) Schemes.geneticManipulation
      pass
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 4
      getAmplifyCount `shouldReturn` 1

  it "Genetic Manipulation finds and reveals Gene Therapy from encounter discard on defeat" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Treacheries.assault] [Attachments.geneTherapy]
      card <- revealNow p Schemes.geneticManipulation
      pass
      resolve =<< L.runQueueT (L.capture $ L.removeThreat p p 2 $ SchemeWithId $ SchemeSideSchemeId $ sideSchemeOf card)
      getsGame (any ((== "33030") . toCardCode . getCardDef) . HashMap.elems . gameAttachments) `shouldReturn` True

  it "Gene Therapy chooses the lowest printed ATK without an existing copy and adds two ATK" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      weak <- addMinion p Minions.misterSinister
      _ <- addMinion p Minions.abomination
      card <- revealNow p Attachments.geneTherapy
      whenM (offers $ isTargetChoice weak) $ choose "Mister Sinister" (isTargetChoice weak)
      pass
      attachmentEnemyOf card `shouldReturn` Just (EnemyMinionId weak)
      other <- revealNow p Attachments.geneTherapy
      pass
      vid <- villainIdNow
      attachmentEnemyOf other `shouldReturn` Just (EnemyVillainId vid)
      modifiers <- getModifiers (MinionRef weak)
      modifiers `shouldSatisfy` elem (AttackModifier 2)

  it "Gene Therapy grants piercing and overkill to an attack and is discarded afterward" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.misterSinister
      card <- revealNow p Attachments.geneTherapy
      whenM (offers $ isTargetChoice mid) $ choose "Sinister" (isTargetChoice mid)
      pass
      runNow [GiveStatus (IdentityRef p) ToughStatus]
      _ <- setEncounterDeck [Treacheries.explosion, Treacheries.explosion] []
      resolve =<< L.runQueueT (L.capture $ L.enemyAttacks mid p)
      (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False
      damageOf p >>= (`shouldSatisfy` (> 0))
      getsGame (HashMap.member (attachmentOf card) . gameAttachments) `shouldReturn` False

  it "Concussive Force places two main threat in alter ego without Sinister" do
    tg <- testGame ["cyclops"]
    inGame tg do
      before <- mainSchemeThreatNow
      _ <- revealNow (player1 tg) Treacheries.concussiveForce
      pass
      mainSchemeThreatNow `shouldReturn` (before + 2)

  it "Concussive Force deals two nonattack damage in hero form without Sinister" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- revealNow p Treacheries.concussiveForce
      pass
      damageOf p `shouldReturn` 2

  it "Concussive Force makes Sinister scheme in alter ego" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Minions.misterSinister
      _ <- setEncounterDeck [Treacheries.explosion, Treacheries.explosion] []
      before <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.concussiveForce
      pass
      mainSchemeThreatNow `shouldReturn` (before + 3)

  it "Concussive Force makes Sinister attack the revealing hero" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- addMinion p Minions.misterSinister
      _ <- setEncounterDeck [Treacheries.explosion, Treacheries.explosion] []
      _ <- revealNow p Treacheries.concussiveForce
      pass
      damageOf p >>= (`shouldSatisfy` (> 0))

  it "Lost Visor takes the physical visor from hand and prevents Cyclops attacks" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      clearHand p
      visor <- putInHand p "33003"
      card <- revealNow p Obligations.lostVisor
      pass
      map pcCardId <$> heldAt card `shouldReturn` [pcCardId visor]
      handOf p >>= (`shouldSatisfy` all ((/= pcCardId visor) . pcCardId))
      asHero p
      offers (isHeroAttack p) `shouldReturn` False
      modifiers <- getModifiers (IdentityRef p)
      modifiers `shouldSatisfy` elem CannotAttack

  it "Lost Visor takes the visor from play without discarding it" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      clearVisors p
      visor <- putIntoPlay p "33003"
      card <- revealNow p Obligations.lostVisor
      pass
      getsGame (HashMap.member (upgradeOf visor) . gameUpgrades) `shouldReturn` False
      map pcCardId <$> heldAt card `shouldReturn` [pcCardId visor]
      discardOf p >>= (`shouldSatisfy` all ((/= pcCardId visor) . pcCardId))

  it "Lost Visor's alter-ego action exhausts Scott, returns visor, and removes itself" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      clearHand p
      visor <- putInHand p "33003"
      card <- revealNow p Obligations.lostVisor
      pass
      choose "recover visor" (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId card)
      isExhaustedNow p `shouldReturn` True
      handOf p >>= (`shouldSatisfy` any ((== pcCardId visor) . pcCardId))
      getsGame (HashMap.member (ObligationId $ unCardId $ ecCardId card) . gameObligations) `shouldReturn` False
      discardOf p >>= (`shouldSatisfy` all ((/= pcCardId visor) . pcCardId))

  it "Lost Visor searches the deck and discard pile as well as the hand" do
    for_ [True, False] $ \inDeck -> do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearVisors p
        visor <- putInHand p "33003"
        clearHand p
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity $
          if inDeck then a {playerIdentityDeck = Deck (visor : unDeck a.playerIdentityDeck)}
          else a {playerIdentityDiscard = Discard [visor]}
        card <- revealNow p Obligations.lostVisor
        pass
        map pcCardId <$> heldAt card `shouldReturn` [pcCardId visor]
        discardOf p >>= (`shouldSatisfy` all ((/= pcCardId visor) . pcCardId))
        unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` all ((/= pcCardId visor) . pcCardId))

  it "Gene Therapy surges when every enemy already has a copy" do
    tg <- testGame ["cyclops"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Attachments.geneTherapy
      pass
      _ <- setEncounterDeck [Treacheries.hardToKeepDown, Treacheries.assault] []
      card <- revealNow p Attachments.geneTherapy
      pass
      getsGame (HashMap.member (attachmentOf card) . gameAttachments) `shouldReturn` False
      dealtTo p >>= (`shouldSatisfy` any ((== "01104") . toCardCode))
