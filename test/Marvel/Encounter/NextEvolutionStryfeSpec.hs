module Marvel.Encounter.NextEvolutionStryfeSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (AbilityType (..))
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Ally.Types qualified as Ally
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Support.Types qualified as Support
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as Scheme
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Modifier
import Marvel.NextEvolution.Stryfe
import Marvel.Query
import TestImport

resolveForced :: GameAppT ()
resolveForced = do
  pass
  found <- offers forced
  when found $ choose "resolve forced ability" forced >> resolveForced
 where
  forced = \case
    UseAbility ab -> abilityType ab `elem` [ForcedInterrupt,ForcedResponse]
    _ -> False

spec :: Spec
spec = describe "Next Evolution Stryfe" do
  it "sets up one physical Hope and Grasp with its fixed four threat plus hinder" do
    tg <- testGameAgainst "40163" ["spider-man"]
    inGame tg do
      selectListCount (AllyWithTitle "Hope Summers") `shouldReturn` 1
      grasp <- selectJust (SideSchemeIs Schemes.ne40168a)
      sideSchemeThreat grasp `shouldReturn` 10
      deck <- getEncounterDeck
      liftIO $ map toCardCode deck `shouldSatisfy` all (`notElem` ["40168a","40168b"])

  it "flips the same Grasp to Living Bomb and advances to Left to Your Fate" do
    tg <- testGameAgainst "40163" ["spider-man"]
    inGame tg do
      grasp <- selectJust (SideSchemeIs Schemes.ne40168a)
      resolve [SideSchemeMessage grasp (SideSchemeThreatRemoved (toSource $ player1 tg) 10)]
      selectList (SideSchemeIs Schemes.ne40168b) `shouldReturn` [grasp]
      sideSchemeThreat grasp `shouldReturn` 3
      main <- mainSchemeIdNow
      getMainSchemeCode main `shouldReturn` Just "40167"
      villain <- villainIdNow
      mods <- getModifiers (VillainRef villain)
      liftIO $ mods `shouldSatisfy` elem CannotBeDefeated
      resolve [SideSchemeMessage grasp (SideSchemeThreatRemoved (toSource $ player1 tg) 3)]
      modsAfter <- getModifiers (VillainRef villain)
      liftIO $ modsAfter `shouldSatisfy` notElem CannotBeDefeated

  it "counts the largest group of types rather than the hand size" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      for_ ["01002","01002","01003","01065","01065","01065"] (putInHand you)
      handTypeCount you `shouldReturn` 3

  it "Mental Transferal mirrors actual villain damage and then discards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid0 <- villainIdNow
      resolve [ReplaceVillain vid0 "40163" Swapped]
      transfer <- revealNow you Attachments.ne40169
      before <- damageOf you
      vid <- villainIdNow
      messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.dealDamage vid you (toDamage 2 FromAbility)
      resolve messages
      damageOf you `shouldReturn` before + 2
      getsGame (HashMap.member (AttachmentId $ unCardId $ toCardId transfer) . gameAttachments) `shouldReturn` False

  it "Mind Alteration deals damage after playing an upgrade" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      pass
      _ <- revealNow you Attachments.ne40170
      resolve []
      upgrade <- putInHand you "01065"
      resource <- putInHand you "01088"
      before <- damageOf you
      playWith upgrade [resource]
      resolveForced
      damageOf you `shouldReturn` before + 1

  it "Cerebral Erasure returns a controlled support to its owner's hand" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      support <- putIntoPlay you "01006"
      _ <- revealNow you Schemes.ne40175
      choose "return support" (isTargetChoice $ supportOf support)
      resolve []
      cards <- handOf you
      liftIO $ map pcCardId cards `shouldSatisfy` elem (pcCardId support)

  it "Psionic Surge deals only the Psionic cards it actually discarded" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      for_ ["01002","01002"] (putInHand you)
      (deck, _) <- setEncounterDeck [Treacheries.ne40178,Treacheries.ne40177,Treacheries.ne40179] []
      _ <- revealNow you Treacheries.ne40177
      resolve []
      dealt <- dealtTo you
      liftIO $ map ecCardId dealt `shouldBe` [ecCardId $ testHead deck]

  it "Psychic Override counts chosen cards after drawing up to hand size" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      original <- putInHand you "01088"
      _ <- putInHand you "01002"
      before <- mainSchemeThreatNow
      _ <- revealNow you Treacheries.ne40178
      choose "keep resources" (isLabel "$label.resource")
      resolve []
      cards <- handOf you
      let resources = fromIntegral $ length $ filter ((== ResourceType) . cdCardType . getCardDef) cards
      liftIO $ map pcCardId cards `shouldSatisfy` elem (pcCardId original)
      mainSchemeThreatNow `shouldReturn` before + resources

  it "Mind Trap exhausts an ally, support, and upgrade on entry" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Attachments.ne40171
      ally <- putIntoPlay you "01002"
      support <- putIntoPlay you "01006"
      upgrade <- putIntoPlay you "01065"
      getsGame (fmap (Ally.allyExhausted . toAttrs) . HashMap.lookup (allyIdOf ally) . gameAllies) `shouldReturn` Just True
      getsGame (fmap (Support.supportExhausted . toAttrs) . HashMap.lookup (supportOf support) . gameSupports) `shouldReturn` Just True
      getsGame (fmap (Upgrade.upgradeExhausted . toAttrs) . HashMap.lookup (upgradeOf upgrade) . gameUpgrades) `shouldReturn` Just True

  for_ [(True,True),(False,False)] $ \(trapOnRecipient, expectedExhausted) ->
    it ("Mind Trap checks the receiving ally's controller, recipient trapped=" <> show trapOnRecipient) do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        let payer = player1 tg
            recipient = player2 tg
        ally <- putIntoPlay recipient "01002"
        _ <- revealNow (if trapOnRecipient then recipient else payer) Attachments.ne40171
        resolve []
        sidearm <- putInHand payer "23035"
        resource <- putInHand payer "01088"
        playOn sidearm (allyIdOf ally) [resource]
        pass
        getsGame (fmap (Upgrade.upgradeExhausted . toAttrs) . HashMap.lookup (upgradeOf sidearm) . gameUpgrades) `shouldReturn` Just expectedExhausted

  it "Mind Trap can be paid with a player side scheme and two other controlled cards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      support <- putIntoPlay you "01006"
      scheme <- putIntoPlay you "40190a"
      trap <- revealNow you Attachments.ne40171
      resolve []
      let aid = AttachmentId $ unCardId $ toCardId trap
          sid = SideSchemeId $ unCardId $ pcCardId scheme
      choose "remove Mind Trap" (isAbilityOf aid)
      choose "exhaust the side scheme" (isTargetChoice sid)
      choose "exhaust the support" (isTargetChoice $ supportOf support)
      choose "exhaust identity" (isTargetChoice you)
      resolve []
      getsGame (HashMap.member aid . gameAttachments) `shouldReturn` False
      getsGame (fmap (Scheme.sideSchemeExhausted . toAttrs) . HashMap.lookup sid . gameSideSchemes) `shouldReturn` Just True

  it "Psionic Amnesia adds only ally and support resource costs" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Attachments.ne40172
      mods <- getModifiers (toRef $ player1 tg)
      liftIO $ mods `shouldSatisfy` elem (AdditionalResourceCost (CardMatchesAny $ map CardWithType [AllyType,SupportType]) 2)

  it "Psychic Inertia penalizes attack and thwart" do
    tg <- testGame ["spider-man"]
    inGame tg do
      changeForm (player1 tg)
      pass
      _ <- revealNow (player1 tg) Attachments.ne40173
      mods <- getModifiers (toRef $ player1 tg)
      liftIO do
        mods `shouldSatisfy` elem (AttackPenalty 1)
        mods `shouldSatisfy` elem (ThwartPenalty 1)

  it "Psychic Inertia recognizes attacks and thwarts made before it entered play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      pass
      placeMainThreat 2
      choose "basic attack" (isHeroAttack you)
      chooseVillain
      pass
      resolve [IdentityMessage you ReadiedIdentity]
      choose "basic thwart" (isHeroThwart you)
      chooseMainScheme
      pass
      getIdentityAttackedAndThwartedThisPhase you `shouldReturn` True
      inertia <- revealNow you Attachments.ne40173
      resolve []
      let aid = AttachmentId $ unCardId $ toCardId inertia
      offers (isAbilityOf aid) `shouldReturn` True
      choose "discard Psychic Inertia" (isAbilityOf aid)
      getsGame (HashMap.member aid . gameAttachments) `shouldReturn` False

  it "stunned attacks and confused thwarts do not satisfy Psychic Inertia" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      pass
      placeMainThreat 2
      resolve [GiveStatus (toRef you) StunnedStatus,GiveStatus (toRef you) ConfusedStatus]
      choose "attempt stunned attack" (isHeroAttack you)
      pass
      resolve [IdentityMessage you ReadiedIdentity]
      choose "attempt confused thwart" (isHeroThwart you)
      pass
      getIdentityAttackedAndThwartedThisPhase you `shouldReturn` False
      inertia <- revealNow you Attachments.ne40173
      resolve []
      offers (isAbilityOf $ AttachmentId $ unCardId $ toCardId inertia) `shouldReturn` False

  it "Telepathic Camouflage adds each player's most common type count" do
    tg <- testGame ["spider-man"]
    inGame tg do
      clearHand (player1 tg)
      for_ ["01002","01002","01003"] (putInHand $ player1 tg)
      card <- revealNow (player1 tg) Schemes.ne40176
      resolve []
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 4

  it "Zero shuffles back when its defeater has fewer than three of every type" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      card <- encounterCard Minions.ne40174
      resolve [PutEncounterCardIntoPlay you card]
      let mid = minionOf card
      resolve [MinionMessage mid (MinionDefeatedWithExcess (toSource you) (toDamage 4 FromAbility) 0)]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
      deck <- getEncounterDeck
      liftIO $ map ecCardId deck `shouldSatisfy` elem (ecCardId card)

  it "Telekinetic Wave boost places threat only when a hand has a group of three" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      for_ ["01002","01002","01002"] (putInHand you)
      card <- encounterCard Treacheries.ne40179
      before <- mainSchemeThreatNow
      vid <- villainIdNow
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      mainSchemeThreatNow `shouldReturn` before + 3
