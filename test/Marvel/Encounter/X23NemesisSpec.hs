module Marvel.Encounter.X23NemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types qualified as Obligation
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Scenario.Types (Attrs (scenarioEncounterDeck))
import Marvel.Keyword
import Marvel.Modifier
import Marvel.Window qualified as W
import TestImport

settle :: GameAppT ()
settle = go (150 :: Int)
 where
  go 0 = error "X-23 nemesis fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let cs = choicesOf q
          passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i, c) <- zip [0 ..] cs, passing c])
      go (n - 1)
    Nothing -> pure ()

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

heldAt :: EncounterCard -> GameAppT [PlayerCard]
heldAt c = getsGame (maybe [] (Obligation.obligationHeldCards . toAttrs) . HashMap.lookup (ObligationId $ unCardId $ ecCardId c) . gameObligations)

clearHoneyBadger :: IdentityId -> GameAppT ()
clearHoneyBadger you = do
  withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity a) -> PlayerIdentity a
    { playerIdentityHand = Hand $ filter ((/= "43003") . toCardCode) $ unHand a.playerIdentityHand
    , playerIdentityDeck = Deck $ filter ((/= "43003") . toCardCode) $ unDeck a.playerIdentityDeck
    , playerIdentityDiscard = Discard $ filter ((/= "43003") . toCardCode) $ unDiscard a.playerIdentityDiscard
    }
  alliesWith "43003" >>= traverse_ (\a -> resolve [RemoveFromGame $ AllyRef $ allyId a])

boostNow :: CardDef -> GameAppT ()
boostNow def = do
  vid <- villainIdNow
  card <- encounterCard def
  runNow [RevealBoostCard card (EnemyVillainId vid)]
  settle

spec :: Spec
spec = describe "X23 nemesis and obligation" do
  describe "Self-Isolation" do
    it "holds the physical Honey Badger from hand facedown and stays in play" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        clearHoneyBadger p
        badger <- putInHand p "43003"
        obligation <- revealNow p Obligations.selfIsolation
        settle
        map pcCardId <$> heldAt obligation `shouldReturn` [pcCardId badger]
        handOf p >>= (`shouldSatisfy` all ((/= pcCardId badger) . pcCardId))
        discardOf p >>= (`shouldSatisfy` all ((/= pcCardId badger) . pcCardId))
        obligationsInPlay `shouldReturn` 1

    it "takes Honey Badger from play without discarding her" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        clearHoneyBadger p
        badger <- putIntoPlay p "43003"
        obligation <- revealNow p Obligations.selfIsolation
        settle
        alliesWith "43003" `shouldReturn` []
        map pcCardId <$> heldAt obligation `shouldReturn` [pcCardId badger]
        discardOf p >>= (`shouldSatisfy` all ((/= pcCardId badger) . pcCardId))

    for_ [True, False] $ \inDeck ->
      it (if inDeck then "searches the deck for Honey Badger" else "searches the discard pile for Honey Badger") do
        tg <- testGame ["x-23"]
        inGame tg do
          let p = player1 tg
          clearHoneyBadger p
          badger <- putInHand p "43003"
          clearHand p
          withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity $
            if inDeck then a {playerIdentityDeck = Deck $ badger : unDeck a.playerIdentityDeck}
            else a {playerIdentityDiscard = Discard [badger]}
          obligation <- revealNow p Obligations.selfIsolation
          settle
          map pcCardId <$> heldAt obligation `shouldReturn` [pcCardId badger]
          unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` all ((/= pcCardId badger) . pcCardId))
          discardOf p >>= (`shouldSatisfy` all ((/= pcCardId badger) . pcCardId))

    it "discards itself and deals a facedown encounter card when Honey Badger cannot be found" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        clearHoneyBadger p
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- length <$> dealtTo p
        _ <- revealNow p Obligations.selfIsolation
        settle
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "43028")
        length <$> dealtTo p `shouldReturn` before + 1

    it "offers its response after basic recovery and discards both cards" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        clearHoneyBadger p
        badger <- putInHand p "43003"
        obligation <- revealNow p Obligations.selfIsolation
        settle
        setDamage p 5
        choose "basic recovery" $ \case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 200; _ -> False
        choose "discard Self-Isolation" (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId obligation)
        settle
        damageOf p `shouldReturn` 0
        obligationsInPlay `shouldReturn` 0
        discardOf p >>= (`shouldSatisfy` any ((== pcCardId badger) . pcCardId))
        encounterDiscardCodes >>= (`shouldSatisfy` elem "43028")

    it "does not discard after another player recovers or after ordinary healing" do
      tg <- testGame ["x-23", "spider-man"]
      inGame tg do
        let p = player1 tg
        clearHoneyBadger p
        _ <- putInHand p "43003"
        obligation <- revealNow p Obligations.selfIsolation
        settle
        runNow [CheckWindows [W.Window W.After $ W.UsedRecovery (player2 tg)]]
        offers (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId obligation) `shouldReturn` False
        settle
        setDamage p 2
        resolve [IdentityMessage p $ IdentityHealed 1]
        obligationsInPlay `shouldReturn` 1

  describe "Lady Deathstrike" do
    it "makes the defeating player take indirect damage from printed boost icons" do
      tg <- testGame ["x-23", "spider-man"]
      inGame tg do
        let p = player1 tg
            q = player2 tg
        mid <- addMinion p Minions.ladyDeathstrikeX23
        _ <- setEncounterDeck [Attachments.cybermods, Treacheries.assault] []
        runNow [MinionMessage mid $ MinionDamaged (IdentityRef q) (toDamage 5 $ FromPlayerAttack q)]
        settle
        damageOf p `shouldReturn` 0
        damageOf q `shouldReturn` 2
        encounterDiscardCodes >>= (`shouldSatisfy` elem "43031")
        minionDamageOf mid `shouldReturn` Nothing

    it "counts boost stars as zero and still discards the encounter card" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.ladyDeathstrikeX23
        _ <- setEncounterDeck [Treacheries.hackNSlashX23, Treacheries.assault] []
        runNow [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 5 $ FromPlayerAttack p)]
        settle
        damageOf p `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "43033")

  describe "In the Name of Vengeance" do
    it "has six threat total and gives both villains and minions retaliate" do
      tg <- testGame ["x-23", "spider-man"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.hydraMercenary
        scheme <- revealNow p Schemes.inTheNameOfVengeance
        settle
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 6
        vid <- villainIdNow
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))
        getModifiers (MinionRef mid) >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))
        getModifiers (IdentityRef p) >>= (`shouldSatisfy` notElem (KeywordModifier $ Retaliate 1))
        resolve [RemoveFromPlay $ SideSchemeRef $ sideSchemeOf scheme]
        getModifiers (MinionRef mid) >>= (`shouldSatisfy` notElem (KeywordModifier $ Retaliate 1))

    it "retaliates after an attack and stacks with an enemy's existing retaliate" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Schemes.inTheNameOfVengeance
        settle
        mid <- addMinion p Minions.whiplash
        changeForm p
        pass
        choose "basic attack" (isHeroAttack p)
        choose "attack Whiplash" (isTargetChoice mid)
        pass
        damageOf p `shouldReturn` 2

  describe "Cybermods" do
    it "attaches to Lady Deathstrike already in play" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.ladyDeathstrikeX23
        card <- revealNow p Attachments.cybermods
        settle
        getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments) `shouldReturn` Just (Just $ EnemyMinionId mid)

    it "finds a minion by discarding encounter cards and puts it into play engaged with the revealer" do
      tg <- testGame ["x-23", "spider-man"]
      inGame tg do
        let p = player1 tg
        ([_, minion, _], _) <- setEncounterDeck [Treacheries.advance, Minions.hydraMercenary, Treacheries.assault] []
        card <- revealNow p Attachments.cybermods
        settle
        getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments) `shouldReturn` Just (Just $ EnemyMinionId $ minionOf minion)
        getsGame (fmap (minionEngagedIdentity . toAttrs) . HashMap.lookup (minionOf minion) . gameMinions) `shouldReturn` Just p
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01186")

    for_ [False, True] $ \defeated ->
      it (if defeated then "shuffles a defeated host into the encounter deck and discards Cybermods" else "shuffles a host discarded from play into the encounter deck") do
        tg <- testGame ["x-23"]
        inGame tg do
          let p = player1 tg
          mid <- addMinion p Minions.hydraMercenary
          _ <- setEncounterDeck [Minions.hydraMercenary, Treacheries.assault] []
          card <- revealNow p Attachments.cybermods
          settle
          -- The first mercenary already in play is not Lady Deathstrike: Cybermods found a second physical copy.
          Just (EnemyMinionId host) <- getsGame (Attachment.attachmentEnemy . toAttrs . fromMaybe (error "missing Cybermods") . HashMap.lookup (attachmentOf card) . gameAttachments)
          runNow [if defeated then MinionMessage host $ MinionDamaged (IdentityRef p) (toDamage 3 $ FromPlayerAttack p) else RemoveFromPlay (MinionRef host)]
          settle
          minionDamageOf host `shouldReturn` Nothing
          minionDamageOf mid `shouldReturn` Just 0
          deck <- getsGame (scenarioEncounterDeck . toAttrs . gameScenario)
          map ecCardId deck `shouldSatisfy` elem (CardId $ unMinionId host)
          encounterDiscardCodes >>= (`shouldSatisfy` elem "43031")
          getsGame (HashMap.member (attachmentOf card) . gameAttachments) `shouldReturn` False

    it "discards itself if there is no Lady Deathstrike and no minion in the encounter deck" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        card <- revealNow p Attachments.cybermods
        settle
        getsGame (HashMap.member (attachmentOf card) . gameAttachments) `shouldReturn` False
        encounterDiscardCodes >>= (`shouldSatisfy` elem "43031")

    it "does not replace the discard of another minion with the same title" do
      tg <- testGame ["x-23"]
      inGame tg do
        let p = player1 tg
        other <- addMinion p Minions.hydraMercenary
        ([cardHost, _], _) <- setEncounterDeck [Minions.hydraMercenary, Treacheries.assault] []
        card <- revealNow p Attachments.cybermods
        settle
        runNow [RemoveFromPlay $ MinionRef other]
        settle
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01101")
        getsGame (HashMap.member (minionOf cardHost) . gameMinions) `shouldReturn` True
        getsGame (HashMap.member (attachmentOf card) . gameAttachments) `shouldReturn` True

  describe "Critical Wound" do
    it "attaches to the revealed player's identity and deals four damage as that player's turn ends" do
      tg <- testGame ["x-23", "spider-man"]
      inGame tg do
        let p = player1 tg
        card <- revealNow p Attachments.criticalWound
        settle
        runNow [CheckWindows [W.Window W.When $ W.TurnEnded (player2 tg)]]
        settle
        damageOf p `shouldReturn` 0
        getsGame (HashMap.member (attachmentOf card) . gameAttachments) `shouldReturn` True
        endTurn
        settle
        damageOf p `shouldReturn` 4
        encounterDiscardCodes >>= (`shouldSatisfy` elem "43032")
        getsGame (HashMap.member (attachmentOf card) . gameAttachments) `shouldReturn` False

  describe "Hack 'n' Slash" do
    for_ [False, True] $ \boosted -> do
      it (if boosted then "its boost counts the random discarded card's printed resources" else "its reveal counts the random discarded card's printed resources") do
        tg <- testGame ["x-23"]
        inGame tg do
          let p = player1 tg
          clearHand p
          card <- putInHand p "01088"
          if boosted then boostNow Treacheries.hackNSlashX23 else void (revealNow p Treacheries.hackNSlashX23) >> settle
          handOf p >>= (`shouldSatisfy` notElem card)
          damageOf p `shouldReturn` 2

      it (if boosted then "its boost does no damage with an empty hand" else "its reveal does no damage with an empty hand") do
        tg <- testGame ["x-23"]
        inGame tg do
          let p = player1 tg
          clearHand p
          if boosted then boostNow Treacheries.hackNSlashX23 else void (revealNow p Treacheries.hackNSlashX23) >> settle
          damageOf p `shouldReturn` 0
