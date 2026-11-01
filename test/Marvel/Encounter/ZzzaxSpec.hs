module Marvel.Encounter.ZzzaxSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types qualified as Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Environment.Cards qualified as Environments
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Phase
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Status
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

settle :: GameAppT ()
settle = do
  pressWhenRevealed
  forced <- offers isForced
  if forced then choose "resolve forced ability" isForced >> settle
    else do
      pass
      whenM (offers isForced) settle
 where
  isForced = \case
    UseAbility a -> abilityType a `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse]
    _ -> False

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

minionAttrs :: MinionId -> GameAppT (Attrs Minion)
minionAttrs mid = getsGame (maybe (error "missing minion") toAttrs . HashMap.lookup mid . gameMinions)

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

environmentOf :: EncounterCard -> EnvironmentId
environmentOf = EnvironmentId . unCardId . ecCardId

cardAbility :: IsRef a => a -> Natural -> Choice -> Bool
cardAbility target index = \case
  UseAbility a -> abilitySource a == toRef target && abilityIndex a == index
  _ -> False

hasAttachment :: AttachmentId -> GameAppT Bool
hasAttachment aid = getsGame (HashMap.member aid . gameAttachments)

hasEnvironment :: EnvironmentId -> GameAppT Bool
hasEnvironment eid = getsGame (HashMap.member eid . gameEnvironments)

surgeDealtTo :: IdentityId -> GameAppT Bool
surgeDealtTo p = any ((== toCardCode Minions.hydraMercenary) . toCardCode) <$> dealtTo p

spec :: Spec
spec = describe "Zzzax modular" do
  describe "Feedback Loop" do
    it "adds every player's hand and controlled printed energy icons to its fixed two threat" do
      tg <- testGame ["ironheart", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        clearHand other
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        _ <- putInHand other "01085"
        _ <- putIntoPlay other "29015"
        card <- revealNow p Schemes.feedbackLoop
        choose "first player resolves the other player first" (isTargetChoice other)
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 6

    it "does not count printed wild resources as energy" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "29010"
        card <- revealNow p Schemes.feedbackLoop
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 2

    it "counts each converted icon separately while Haywire is attached" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01089"
        _ <- revealNow p Attachments.haywire
        settle
        card <- revealNow p Schemes.feedbackLoop
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 4

  describe "Zzzax" do
    it "gets attack and hit points from only the engaged player's in-play energy icons" do
      tg <- testGame ["ironheart", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01088"
        _ <- putIntoPlay other "29015"
        z <- addMinion p Minions.zzzax
        attrs <- minionAttrs z
        Minion.getModifiedAttack attrs `shouldReturn` 2
        Minion.getModifiedHitPoints attrs `shouldReturn` 4
        ally <- putIntoPlay p "29023"
        attrs' <- minionAttrs z
        Minion.getModifiedAttack attrs' `shouldReturn` 3
        Minion.getModifiedHitPoints attrs' `shouldReturn` 5
        resolve [RemoveFromPlay $ AllyRef $ allyIdOf ally]
        attrs'' <- minionAttrs z
        Minion.getModifiedAttack attrs'' `shouldReturn` 2
        Minion.getModifiedHitPoints attrs'' `shouldReturn` 4

    it "its boost enters play when a single double-energy card is in hand" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putInHand p "01088"
        [boost] <- stackEncounterDeck [Minions.zzzax]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        settle
        selectAny (MinionIs Minions.zzzax) `shouldReturn` True
        getMinion <- getsGame (HashMap.lookup (minionOf boost) . gameMinions)
        Minion.getMinionEngagedIdentity <$> getMinion `shouldBe` Just p
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "29037")

    it "its boost does not enter play with only one energy icon or wild icons" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putInHand p "01085"
        _ <- putInHand p "29010"
        _ <- stackEncounterDeck [Minions.zzzax]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        settle
        selectAny (MinionIs Minions.zzzax) `shouldReturn` False
        encounterDiscardCodes >>= (`shouldSatisfy` elem "29037")

  describe "Haywire" do
    it "attaches to the revealing identity and changes all its hand icons without affecting others" do
      tg <- testGame ["ironheart", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        clearHand other
        double <- putInHand p "01089"
        wild <- putInHand p "29010"
        unaffected <- putInHand other "01089"
        card <- revealNow p Attachments.haywire
        settle
        target <- getsGame (fmap (Attachment.attachmentIdentity . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments)
        target `shouldBe` Just (Just p)
        printedResourcesFor double `shouldReturn` [Energy, Energy]
        printedResourcesFor wild `shouldReturn` [Energy]
        printedResourcesFor unaffected `shouldReturn` [Mental, Mental]

    it "uses the converted printed resources for payment" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- revealNow p Attachments.haywire
        settle
        genius <- putInHand p "01089"
        attack <- putInHand p "29017"
        playWith attack [genius]
        vid <- villainIdNow
        choose "attack the villain" (isTargetChoice vid)
        settle
        villainDamageNow `shouldReturn` 6

    it "can be discarded by paying two indirect damage in hero form" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- revealNow p Attachments.haywire
        settle
        choose "pay the damage cost" (cardAbility (AttachmentRef $ attachmentOf card) 2)
        damageOf p `shouldReturn` 2
        hasAttachment (attachmentOf card) `shouldReturn` False

    it "can discard an in-play printed-energy card instead of taking damage" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asHero p
        ally <- putIntoPlay p "29015"
        card <- revealNow p Attachments.haywire
        settle
        choose "discard an energy card" (cardAbility (AttachmentRef $ attachmentOf card) 1)
        settle
        alliesWith "29015" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` any ((== pcCardId ally) . pcCardId))
        damageOf p `shouldReturn` 0
        hasAttachment (attachmentOf card) `shouldReturn` False

    it "does not allow converted hand cards as controlled discard costs or actions in alter-ego" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01089"
        card <- revealNow p Attachments.haywire
        settle
        offers (isAbilityOf $ AttachmentRef $ attachmentOf card) `shouldReturn` False
        changeForm p
        pass
        offers (cardAbility (AttachmentRef $ attachmentOf card) 1) `shouldReturn` False
        offers (cardAbility (AttachmentRef $ attachmentOf card) 2) `shouldReturn` True

    it "cannot pay indirect damage while its only character is tough" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- revealNow p Attachments.haywire
        settle
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        offers (cardAbility (AttachmentRef $ attachmentOf card) 2) `shouldReturn` False

  describe "Air Static" do
    it "damages each player with energy in hand or play, and leaves players with only wild resources alone" do
      tg <- testGame ["ironheart", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        clearHand other
        _ <- putInHand p "01085"
        _ <- putInHand other "29010"
        _ <- revealNow p Environments.airStatic
        settle
        runNow [BeginPhase VillainPhase]
        settle
        choose "first player resolves the other player first" (isTargetChoice other)
        settle
        damageOf p `shouldReturn` 2
        damageOf other `shouldReturn` 0

    it "also triggers for an in-play energy icon with no energy in hand" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "29015"
        _ <- revealNow p Environments.airStatic
        settle
        runNow [BeginPhase VillainPhase]
        choose "resolve Air Static" isAnyAbility
        choose "assign first damage to identity" (isTargetChoice p)
        choose "assign second damage to identity" (isTargetChoice p)
        settle
        damageOf p `shouldReturn` 2

    it "does not trigger at the beginning of the player phase" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01088"
        _ <- revealNow p Environments.airStatic
        settle
        runNow [BeginPhase PlayerPhase]
        settle
        damageOf p `shouldReturn` 0

    it "any hero can discard it by taking two indirect damage" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- revealNow p Environments.airStatic
        settle
        choose "remove Air Static" (cardAbility (EnvironmentRef $ environmentOf card) 3)
        damageOf p `shouldReturn` 2
        hasEnvironment (environmentOf card) `shouldReturn` False

    it "can instead discard a printed-energy card without taking damage" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "29015"
        card <- revealNow p Environments.airStatic
        settle
        choose "discard an energy card" (cardAbility (EnvironmentRef $ environmentOf card) 2)
        settle
        alliesWith "29015" `shouldReturn` []
        damageOf p `shouldReturn` 0
        hasEnvironment (environmentOf card) `shouldReturn` False

  describe "Zzzap!" do
    it "deals indirect damage for every energy icon and does not surge after two dealt to the identity" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01088"
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.zzzap
        settle
        damageOf p `shouldReturn` 2
        surgeDealtTo p `shouldReturn` False

    it "counts damage dealt before an Energy Barrier interrupt prevents some of it" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01088"
        barrier <- putIntoPlay p "05017"
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.zzzap
        pressWhenRevealed
        choose "prevent one damage" (isAbilityOfCard barrier)
        chooseVillain
        settle
        damageOf p `shouldReturn` 1
        surgeDealtTo p `shouldReturn` False

    it "surges with one energy icon and after zero damage" do
      for_ [Nothing, Just "01085"] $ \energy -> do
        tg <- testGame ["ironheart"]
        let p = player1 tg
        inGame tg do
          clearHand p
          traverse_ (putInHand p) energy
          _ <- stackEncounterDeck [Minions.hydraMercenary]
          _ <- revealNow p Treacheries.zzzap
          settle
          damageOf p `shouldReturn` (if isJust energy then 1 else 0)
          surgeDealtTo p `shouldReturn` True

    it "surges when two damage are assigned to an ally and none to the identity" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01088"
        ally <- putIntoPlay p "01084"
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.zzzap
        pressWhenRevealed
        choose "assign first damage to Nick Fury" (isTargetChoice $ allyIdOf ally)
        choose "assign second damage to Nick Fury" (isTargetChoice $ allyIdOf ally)
        settle
        damageOf p `shouldReturn` 0
        surgeDealtTo p `shouldReturn` True

    it "uses energy icons supplied by Haywire when counting indirect damage" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01089"
        _ <- revealNow p Attachments.haywire
        settle
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.zzzap
        settle
        damageOf p `shouldReturn` 2
        surgeDealtTo p `shouldReturn` False

    it "counts damage dealt even when a tough status prevents taking it" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01088"
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.zzzap
        settle
        damageOf p `shouldReturn` 0
        playerIdentityTough <$> playerAttrs p `shouldReturn` False
        surgeDealtTo p `shouldReturn` False
