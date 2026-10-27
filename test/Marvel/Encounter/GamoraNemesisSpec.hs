module Marvel.Encounter.GamoraNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types (Attrs (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

-- | The player in hero form, past the change's responses.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

-- | Reveal Waylay for the player, a Hydra Mercenary on top of the encounter deck: whether
-- it surged, dealing the player the Mercenary facedown.
waylaySurges :: IdentityId -> GameAppT Bool
waylaySurges p = do
  [next] <- stackEncounterDeck [Minions.hydraMercenary]
  _ <- revealNow p Treacheries.waylay
  pass
  elem (ecCardId next) . map ecCardId <$> dealtTo p

isChangeForm :: IdentityId -> Choice -> Bool
isChangeForm p = \case
  UseAbility ab -> ChangeForm `elem` abilityChoices ab && abilitySource ab == toSource p
  _ -> False

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

-- | Play Acrobatic Move (an attack event, cost 0) at the villain: whether Gamora is then
-- offered a response (Finesse, her text box's).
finesseAfterAcrobaticMove :: IdentityId -> GameAppT Bool
finesseAfterAcrobaticMove p = do
  -- Finesse needs a scheme with threat to remove
  placeMainThreat 1
  move <- putInHand p "18003"
  playWith move []
  chooseIf . isTargetChoice =<< villainIdNow
  offers isNoResponse

spec :: Spec
spec = describe "Gamora nemesis" do
  describe "Sibling Rivalry (18025)" do
    it "another player is not offered it to thwart; his thwart of the main scheme still works" do
      tg <- testGame ["gamora", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.siblingRivalry
        pass
        sideSchemeThreat scheme `shouldReturn` 4
        endTurn
        asHero other
        placeMainThreat 1
        main <- mainSchemeThreatNow
        choose "thwart" (isHeroThwart other)
        offers (isTargetChoice scheme) `shouldReturn` False
        chooseIf . isTargetChoice =<< mainSchemeIdNow
        pass
        sideSchemeThreat scheme `shouldReturn` 4
        mainSchemeThreatNow `shouldReturn` (main - 1)

    it "Gamora's thwart removes threat from it" do
      tg <- testGame ["gamora", "spider-man"]
      let p = player1 tg
      inGame tg do
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.siblingRivalry
        pass
        asHero p
        choose "thwart" (isHeroThwart p)
        chooseIf (isTargetChoice scheme)
        pass
        sideSchemeThreat scheme `shouldReturn` 2

    it "and so does her ally's" do
      tg <- testGame ["gamora", "spider-man"]
      let p = player1 tg
      inGame tg do
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.siblingRivalry
        pass
        _ <- putIntoPlay p "18002"
        pass
        nebula <- allyIn "18002"
        choose "ally thwart" (isAllyThwart nebula)
        chooseIf (isTargetChoice scheme)
        pass
        sideSchemeThreat scheme `shouldReturn` 2

    it "deals Gamora 1 facedown encounter card as the villain phase begins" do
      tg <- testGame ["gamora", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- revealNow p SideSchemes.siblingRivalry
        pass
        asHero p
        endTurn
        asHero other
        endTurn
        -- the villain attacks Gamora first: the encounter cards are dealt after
        length <$> dealtTo p `shouldReturn` 1
        length <$> dealtTo other `shouldReturn` 0

  describe "Nebula (18026)" do
    it "discards the Nebula ally from play as she enters play" do
      tg <- testGame ["gamora"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "18002"
        pass
        length <$> alliesWith "18002" `shouldReturn` 1
        _ <- revealNow p Minions.nebulaMinion
        pass
        length <$> alliesWith "18002" `shouldReturn` 0
        getsGame (map (toCardCode . minionCardDef . toAttrs) . HashMap.elems . gameMinions) `shouldReturn` ["18026"]

    it "and as she is put into play" do
      tg <- testGame ["gamora"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "18002"
        pass
        card <- encounterCard Minions.nebulaMinion
        runNow [PutEncounterCardIntoPlay p card]
        pass
        length <$> alliesWith "18002" `shouldReturn` 0
        getsGame (HashMap.member (minionOf card) . gameMinions) `shouldReturn` True

    it "keeps the Nebula ally from being played while she is in play (RR Unique Icon)" do
      tg <- testGame ["gamora"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Minions.nebulaMinion
        pass
        ally <- putInHand p "18002"
        _ <- putInHand p "01088"
        offers (isPlay ally) `shouldReturn` False

  describe "In a Bind (18027)" do
    it "Finesse answers an attack event without it" do
      tg <- testGame ["gamora"]
      let p = player1 tg
      inGame tg do
        asHero p
        clearHand p
        finesseAfterAcrobaticMove p `shouldReturn` True

    it "attaches to Gamora and blanks Finesse; her basic powers and changing form remain" do
      tg <- testGame ["gamora"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p Attachments.inABind
        pass
        getsGame (fmap (attachmentIdentity . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments)
          `shouldReturn` Just (Just p)
        offers (isChangeForm p) `shouldReturn` True
        asHero p
        clearHand p
        offers (isHeroAttack p) `shouldReturn` True
        placeMainThreat 1
        offers (isHeroThwart p) `shouldReturn` True
        finesseAfterAcrobaticMove p `shouldReturn` False

    it "Hero Action: discard an attack event and deal 1 damage to Gamora to discard it" do
      tg <- testGame ["gamora"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p Attachments.inABind
        pass
        asHero p
        clearHand p
        let bind = attachmentOf card
        -- a thwart event is not an attack event
        _ <- putInHand p "18005"
        offers (isAbilityOf bind) `shouldReturn` False
        move <- putInHand p "18003"
        choose "In a Bind" (isAbilityOf bind)
        chooseIf (isTargetChoice move)
        pass
        damageOf p `shouldReturn` 1
        map pcCardId <$> discardOf p >>= (`shouldSatisfy` elem (pcCardId move))
        getsGame (HashMap.member bind . gameAttachments) `shouldReturn` False

  describe "Waylay (18028)" do
    it "stuns and confuses Gamora, without surge when she was neither" do
      tg <- testGame ["gamora"]
      let p = player1 tg
      inGame tg do
        waylaySurges p `shouldReturn` False
        isStunned p `shouldReturn` True
        isConfused p `shouldReturn` True

    it "gains surge when Gamora was already stunned or confused" do
      tg <- testGame ["gamora"]
      let p = player1 tg
      inGame tg do
        waylaySurges p `shouldReturn` False
        waylaySurges p `shouldReturn` True
