module Marvel.Encounter.SpiderHamNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types qualified as Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Game.Source
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Query
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

settle :: GameAppT ()
settle = do
  pressWhenRevealed
  forced <- offers isForced
  if forced then choose "resolve the forced encounter ability" isForced >> settle
    else pass >> whenM (offers isForced) settle
 where
  isForced = \case
    UseAbility a -> abilityType a `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse]
    _ -> False

minionAttrs :: MinionId -> GameAppT (Attrs Minion)
minionAttrs mid = getsGame (maybe (error "missing minion") toAttrs . HashMap.lookup mid . gameMinions)

spec :: Spec
spec = describe "Spider-Ham nemesis" do
  describe "I Really Want a Hot Dog!" do
    it "lets Peter Porker exhaust and spend one toon counter to remove the obligation from the game" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        resolve [PlaceCounters (IdentityRef p) 2]
        _ <- revealNow p Obligations.iReallyWantAHotDog
        choose "pay the obligation" (isLabel "$label.exhaustPeterPorkerRemoveHotDog")
        settle
        isExhaustedNow p `shouldReturn` True
        playerIdentityCounters <$> playerAttrs p `shouldReturn` 1
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "30024")

    it "cannot pay without a counter and becomes stunned instead" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.iReallyWantAHotDog
        settle
        isStunned p `shouldReturn` True
        encounterDiscardCodes >>= (`shouldSatisfy` elem "30024")

    it "surges if already stunned" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        resolve [GiveStatus (IdentityRef p) StunnedStatus]
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        before <- length <$> dealtTo p
        _ <- revealNow p Obligations.iReallyWantAHotDog
        settle
        length <$> dealtTo p `shouldReturn` before + 1
        isStunned p `shouldReturn` True

  describe "The Green Gobbler" do
    it "clears only the engaged player's counters on identity, allies, supports and upgrades" do
      tg <- testGame ["spider-ham", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        resolve [PlaceCounters (IdentityRef p) 3, PlaceCounters (IdentityRef other) 2]
        ally <- putIntoPlay p "01068"
        _ <- putIntoPlay p "01080"
        _ <- putIntoPlay p "01008"
        resolve [PlaceCounters (AllyRef $ allyIdOf ally) 2]
        map supportUses <$> supportsWith "01080" `shouldReturn` [3]
        _ <- addMinion p Minions.theGreenGobbler
        settle
        playerIdentityCounters <$> playerAttrs p `shouldReturn` 0
        playerIdentityCounters <$> playerAttrs other `shouldReturn` 2
        getCountersOn (AllyRef $ allyIdOf ally) `shouldReturn` 0
        supportsWith "01080" `shouldReturn` []
        upgradesWith "01008" `shouldReturn` []
        damageOf p `shouldReturn` 0

  describe "Nefarious Trap" do
    it "starts with three fixed threat and finds the Gobbler from the encounter deck when defeated" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [Minions.theGreenGobbler]
        card <- revealNow p Schemes.nefariousTrap
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 3
        resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (IdentityRef p) 3)]
        settle
        selectList (MinionIs Minions.theGreenGobbler) >>= (`shouldSatisfy` (not . null))
        damageOf p `shouldReturn` 0

    it "makes the Gobbler already in play attack the player who defeated the scheme" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.theGreenGobbler
        card <- revealNow p Schemes.nefariousTrap
        settle
        resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (IdentityRef p) 3)]
        settle
        damageOf p `shouldReturn` 2

    it "does not resolve its When Defeated while Vivian blanks it" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        _ <- stackEncounterDeck [Minions.theGreenGobbler]
        card <- revealNow p Schemes.nefariousTrap
        settle
        vivian <- putInHand p "29024"
        genius <- putInHand p "01089"
        playWith vivian [genius]
        choose "Vivian response" (isAbilityOfCard vivian)
        choose "blank Nefarious Trap" (isTargetChoice $ sideSchemeOf card)
        settle
        resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (IdentityRef p) 3)]
        settle
        selectList (MinionIs Minions.theGreenGobbler) `shouldReturn` []

  describe "Gobbler Glider" do
    it "prefers the Gobbler, grants attack, scheme and Aerial, and surges" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.hydraMercenary
        gobbler <- addMinion p Minions.theGreenGobbler
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        before <- length <$> dealtTo p
        card <- revealNow p Attachments.gobblerGlider
        settle
        let aid = AttachmentId $ unCardId $ ecCardId card
        target <- getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup aid . gameAttachments)
        target `shouldBe` Just (Just $ EnemyMinionId gobbler)
        a <- minionAttrs gobbler
        Minion.getModifiedAttack a `shouldReturn` 3
        Minion.getModifiedScheme a `shouldReturn` 2
        getModifiers (MinionRef gobbler) >>= (`shouldSatisfy` elem (TraitModifier Aerial))
        length <$> dealtTo p `shouldReturn` before + 1

    it "falls back to another minion and is discarded if none exists" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        minion <- addMinion p Minions.hydraMercenary
        card <- revealNow p Attachments.gobblerGlider
        settle
        let aid = AttachmentId $ unCardId $ ecCardId card
        target <- getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup aid . gameAttachments)
        target `shouldBe` Just (Just $ EnemyMinionId minion)
        resolve [RemoveFromPlay (MinionRef minion)]
        _ <- revealNow p Attachments.gobblerGlider
        settle
        encounterDiscardCodes >>= (`shouldSatisfy` elem "30027")

  describe "Feast on This!" do
    it "deals two damage and confuses without surging when not already confused" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.feastOnThis
        settle
        damageOf p `shouldReturn` 2
        isConfused p `shouldReturn` True
        length <$> dealtTo p `shouldReturn` before

    it "surges if already confused and still deals damage" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        resolve [GiveStatus (IdentityRef p) ConfusedStatus]
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.feastOnThis
        settle
        damageOf p `shouldReturn` 2
        isConfused p `shouldReturn` True
        length <$> dealtTo p `shouldReturn` before + 1
