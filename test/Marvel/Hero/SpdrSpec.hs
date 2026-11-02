module Marvel.Hero.SpdrSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (HasAbilities)
import Marvel.AlterEgo.Runner qualified as AlterEgo
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Cost.Types (Cost (..))
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity)
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (HasModifiersFor (..), Modifier (Blank))
import Marvel.Query (selectAny)
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types qualified as Obligation
import TestImport

spdr :: IO TestGame
spdr = testGame ["spdr"]

asSuit :: IdentityId -> GameAppT ()
asSuit who = changeForm who >> pass >> clearHand who

maintenance :: Choice -> Bool
maintenance = isNamedAbility "$label.spdrMaintenance"

sync :: UpgradeId -> Choice -> Bool
sync uid = \case
  UseAbility ab -> abilityCost ab == ExhaustUpgradeCost (UpgradeWithId uid)
  _ -> False

pilot :: GameAppT (Attrs Upgrade)
pilot = upgradesWith "31002b" >>= \case
  [a] -> pure a
  _ -> error "expected the Pilot"

inactiveSuit :: GameAppT (Attrs Support)
inactiveSuit = supportsWith "31001b" >>= \case
  [a] -> pure a
  _ -> error "expected inactive Suit"

paySync :: PlayerCard -> UpgradeId -> GameAppT ()
paySync card uid = do
  choose "play" (isPlay card)
  choose "Sync Ratio" (sync uid)
  whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
  pass

rhinoAttack :: IdentityId -> GameAppT ()
rhinoAttack who = do
  _ <- stackEncounterDeck [Minions.vulture]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks who)]

-- An encounter card's blanking effect proves both printed protections. The same
-- probe against an ordinary Interface does blank it, so the protection is exercised.
data BlankingProbe = BlankingProbe (Attrs Obligation.Obligation) Target
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasAbilities)
instance Obligation.IsObligation BlankingProbe where
  toObligationAttrs (BlankingProbe attrs _) = attrs
instance HasModifiersFor BlankingProbe where
  getBlankingModifiersFor _ target (BlankingProbe _ affected) = pure [Blank | target == affected]
instance RunMessage BlankingProbe where
  runMessage msg (BlankingProbe attrs target) = (`BlankingProbe` target) <$> runMessage msg attrs

blankWithEncounterCard :: IdentityId -> Target -> GameAppT ()
blankWithEncounterCard who target = do
  oid <- getRandom
  let attrs = Obligation.ObligationAttrs
        { Obligation.obligationId = oid
        , Obligation.obligationCardDef = Obligations.inheritedBurden
        , Obligation.obligationSurge = False
        , Obligation.obligationHeldCards = []
                  , Obligation.obligationHeldEncounterCards = []
        , Obligation.obligationSideScheme = Nothing
        , Obligation.obligationOwner = Just who
        }
  withGame_ $ entitiesL . obligationsL . at oid ?~ Obligation.Obligation (BlankingProbe attrs target)

spec :: Spec
spec = describe "SP//dr" do
  describe "separated identity" do
    it "starts with Peni and an inactive Suit, fourteen hit points, four-card hand and the forty-card starter" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        (toCardCode <$> getPlayer who) `shouldReturn` "31002a"
        getIdentityCode who `shouldReturn` "31001a"
        hitPointsOf who `shouldReturn` 14
        handSizeOf who `shouldReturn` 4
        length <$> handOf who `shouldReturn` 4
        suit <- inactiveSuit
        supportExhausted suit `shouldBe` False
        upgradesWith "31002b" `shouldReturn` []
        attrs <- playerAttrs who
        let cards = unDeck (playerIdentityDeck attrs) <> unHand (playerIdentityHand attrs) <> unDiscard (playerIdentityDiscard attrs)
        length cards `shouldBe` 40
        map toCardCode cards `shouldSatisfy` all (`notElem` ["31001b", "31002b"])
        currentIdentity <$> getPlayer who >>= \case
          AlterEgoSide a -> do
            rec <- AlterEgo.getModifiedRecovery (toAttrs a)
            rec `shouldBe` 4
          _ -> fail "Peni must be the starting face"

    it "Maintenance draws two and exhausts only the inactive Suit, independently of Peni's recovery" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        clearHand who
        setDamage who 6
        choose "Maintenance" maintenance
        length <$> handOf who `shouldReturn` 2
        supportExhausted <$> inactiveSuit `shouldReturn` True
        isExhaustedNow who `shouldReturn` False
        offers maintenance `shouldReturn` False
        choose "Peni recovers" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
        damageOf who `shouldReturn` 2
        isExhaustedNow who `shouldReturn` True

    it "can perform Maintenance while Peni is exhausted, with no printed once-per-round limit" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        clearHand who
        setDamage who 4
        choose "recover" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
        offers maintenance `shouldReturn` True
        choose "Maintenance" maintenance
        length <$> handOf who `shouldReturn` 2
        suit <- inactiveSuit
        resolve [SupportMessage (supportId suit) ReadiedSupport]
        choose "Maintenance after the Suit readies" maintenance
        length <$> handOf who `shouldReturn` 4
        isExhaustedNow who `shouldReturn` True

    it "keeps the Suit exhausted after Maintenance when changing to hero, while Pilot stays ready" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        choose "Maintenance" maintenance
        changeForm who
        pass
        (toCardCode <$> getPlayer who) `shouldReturn` "31001a"
        getIdentityCode who `shouldReturn` "31001a"
        isExhaustedNow who `shouldReturn` True
        upgradeExhausted <$> pilot `shouldReturn` False
        supportsWith "31001b" `shouldReturn` []
        handSizeOf who `shouldReturn` 3
        currentIdentity <$> getPlayer who >>= \case
          HeroSide h -> do
            let attrs = toAttrs h
            (,,) <$> Hero.getModifiedThwart attrs <*> Hero.getModifiedAttack attrs <*> Hero.getModifiedDefense attrs `shouldReturn` (2, 2, 2)
          _ -> fail "expected Suit hero"
        offers maintenance `shouldReturn` False
        offers (\case UseAbility ab -> ChangeForm `elem` abilityChoices ab; _ -> False) `shouldReturn` False

    it "keeps exhausted Peni as an exhausted Pilot, then exchanges the independent exhaustion on return" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        setDamage who 4
        choose "recover" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
        changeForm who
        pass
        isExhaustedNow who `shouldReturn` False
        upgradeExhausted <$> pilot `shouldReturn` True
        choose "Suit attacks" (isHeroAttack who)
        chooseVillain
        pass
        -- A card-forced flip, after the turn's ordinary change has been spent.
        resolve [IdentityMessage who (ChangedToForm B)]
        isExhaustedNow who `shouldReturn` True
        supportExhausted <$> inactiveSuit `shouldReturn` True
        upgradesWith "31002b" `shouldReturn` []

    it "transfers status cards, counters and attached cards in both directions without changing the shared dial" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        setDamage who 3
        resolve [PlaceCounters (toRef who) 2, GiveStatus (toRef who) StunnedStatus, GiveStatus (toRef who) ConfusedStatus, GiveStatus (toRef who) ToughStatus]
        attached <- revealNow who Attachments.sapPower
        resolve []
        let aid = AttachmentId $ unCardId $ ecCardId attached
            checkState = do
              damageOf who `shouldReturn` 3
              attrs <- playerAttrs who
              (playerIdentityStunned attrs, playerIdentityConfused attrs, playerIdentityTough attrs, playerIdentityCounters attrs) `shouldBe` (True, True, True, 2)
              getsGame (fmap (Attachment.attachmentIdentity . toAttrs) . HashMap.lookup aid . gameAttachments) `shouldReturn` Just (Just who)
        changeForm who
        pass
        checkState
        resolve [IdentityMessage who (ChangedToForm B)]
        checkState

    it "protects both permanent companion faces from discard and printed-text blanking" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        suit <- inactiveSuit
        blankWithEncounterCard who (toRef $ supportId suit)
        isBlanked (supportId suit) `shouldReturn` False
        resolve [RemoveFromPlay $ toRef $ supportId suit]
        length <$> supportsWith "31001b" `shouldReturn` 1
        asSuit who
        interface <- pilot
        blankWithEncounterCard who (toRef $ upgradeId interface)
        isBlanked (upgradeId interface) `shouldReturn` False
        resolve [RemoveFromPlay $ toRef $ upgradeId interface]
        length <$> upgradesWith "31002b" `shouldReturn` 1
        selectAny (UpgradeWithId (upgradeId interface) <> UpgradeThatCanBeDiscarded) `shouldReturn` False
        psychic <- putIntoPlay who "31011"
        blankWithEncounterCard who (toRef $ upgradeIdOf psychic)
        isBlanked (upgradeIdOf psychic) `shouldReturn` True

  describe "Sync Ratio" do
    it "generates the Pilot's wild resource without exhausting the Suit, and cannot reuse an exhausted Interface" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        asSuit who
        interface <- pilot
        support <- putInHand who "31007"
        paySync support (upgradeId interface)
        length <$> supportsWith "31007" `shouldReturn` 1
        upgradeExhausted <$> pilot `shouldReturn` True
        isExhaustedNow who `shouldReturn` False
        another <- putInHand who "31009"
        offers (isPlay another) `shouldReturn` False

    for_ [("31010", Wild), ("31011", Mental), ("31012", Physical), ("31013", Energy)] \(code, icon) ->
      it ("generates the printed resource of Interface " <> show code) do
        tg <- spdr
        let who = player1 tg
        inGame tg do
          asSuit who
          interface <- putIntoPlay who code
          abilities <- getResourceAbilities who Nothing
          let matches = [ab | ab <- abilities, abilityCost ab == ExhaustUpgradeCost (UpgradeWithId $ upgradeIdOf interface)]
          concatMapM abilityResources matches `shouldReturn` [icon]
          card <- putInHand who "31007"
          paySync card (upgradeIdOf interface)
          upgradeIsExhausted (upgradeIdOf interface) `shouldReturn` True

    it "uses several Interfaces during one payment and allows an exhausted Suit to generate resources" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        asSuit who
        psychic <- putIntoPlay who "31011"
        armor <- putIntoPlay who "31012"
        resolve [IdentityMessage who ExhaustedIdentity]
        host <- putInHand who "31010"
        interface <- pilot
        choose "play Host Spider" (isPlay host)
        choose "Pilot Sync Ratio" (sync $ upgradeId interface)
        choose "Psychic Link Sync Ratio" (sync $ upgradeIdOf psychic)
        choose "Speed-Metal Alloy Sync Ratio" (sync $ upgradeIdOf armor)
        whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
        pass
        length <$> upgradesWith "31010" `shouldReturn` 1
        upgradeExhausted <$> pilot `shouldReturn` True
        upgradeIsExhausted (upgradeIdOf psychic) `shouldReturn` True
        upgradeIsExhausted (upgradeIdOf armor) `shouldReturn` True
        isExhaustedNow who `shouldReturn` True

    it "has no Sync Ratio in alter-ego form, even while other Interfaces are ready" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        clearHand who
        _ <- putIntoPlay who "31011"
        card <- putInHand who "31007"
        offers (isPlay card) `shouldReturn` False
        abilities <- getResourceAbilities who Nothing
        filter (\ab -> abilityIndex ab >= 991) abilities `shouldBe` []

  describe "Host Spider" do
    it "exhausts itself to ready the Suit and cannot do so twice or while the Suit is already ready" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        asSuit who
        host <- putIntoPlay who "31010"
        offers (isAbilityOfCard host) `shouldReturn` False
        resolve [IdentityMessage who ExhaustedIdentity]
        choose "Host Spider" (isAbilityOfCard host)
        isExhaustedNow who `shouldReturn` False
        upgradeIsExhausted (upgradeIdOf host) `shouldReturn` True
        resolve [IdentityMessage who ExhaustedIdentity]
        offers (isAbilityOfCard host) `shouldReturn` False

    it "cannot ready Peni in alter-ego form or use its action after paying through Sync Ratio" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        host <- putIntoPlay who "31010"
        resolve [IdentityMessage who ExhaustedIdentity]
        offers (isAbilityOfCard host) `shouldReturn` False
        asSuit who
        support <- putInHand who "31007"
        paySync support (upgradeIdOf host)
        resolve [IdentityMessage who ExhaustedIdentity]
        offers (isAbilityOfCard host) `shouldReturn` False

  describe "Psychic Link" do
    it "adds two THW only to this basic thwart, then is exhausted" do
      -- Two players give Rhino's main scheme enough room to distinguish a
      -- four-threat thwart from the following unmodified two-threat thwart.
      tg <- testGame ["spdr", "spider-man"]
      let who = player1 tg
      inGame tg do
        asSuit who
        placeMainThreat 8
        link <- putIntoPlay who "31011"
        before <- mainSchemeThreatNow
        choose "thwart" (isHeroThwart who)
        choose "Psychic Link" (isAbilityOfCard link)
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 4
        upgradeIsExhausted (upgradeIdOf link) `shouldReturn` True
        resolve [IdentityMessage who ReadiedIdentity]
        choose "thwart again" (isHeroThwart who)
        offers (isAbilityOfCard link) `shouldReturn` False
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 6

    it "does not enhance a thwart event" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        asSuit who
        placeMainThreat 5
        link <- putIntoPlay who "31011"
        event <- putInHand who "31005"
        resources <- traverse (putInHand who) ["31005", "31005"]
        playWith event resources
        offers (isAbilityOfCard link) `shouldReturn` False
        chooseMainScheme
        pass
        upgradeIsExhausted (upgradeIdOf link) `shouldReturn` False

  describe "Speed-Metal Alloy" do
    it "adds two DEF for this defense, exhausts, and does not affect a later attack" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        asSuit who
        armor <- putIntoPlay who "31012"
        rhinoAttack who
        choose "defend" isDefend
        choose "Speed-Metal Alloy" (isAbilityOfCard armor)
        resolve []
        damageOf who `shouldReturn` 0
        upgradeIsExhausted (upgradeIdOf armor) `shouldReturn` True
        resolve [IdentityMessage who ReadiedIdentity]
        rhinoAttack who
        choose "defend again" isDefend
        offers (isAbilityOfCard armor) `shouldReturn` False
        resolve []
        damageOf who `shouldReturn` 2

    it "does not trigger without defending" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        asSuit who
        armor <- putIntoPlay who "31012"
        rhinoAttack who
        choose "do not defend" isNoDefender
        offers (isAbilityOfCard armor) `shouldReturn` False
        resolve []
        damageOf who `shouldReturn` 4
        upgradeIsExhausted (upgradeIdOf armor) `shouldReturn` False

  describe "Web-Fluid Compressor" do
    it "adds two ATK to one basic attack, exhausts, and does not boost the next attack" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        asSuit who
        compressor <- putIntoPlay who "31013"
        choose "attack" (isHeroAttack who)
        choose "Web-Fluid Compressor" (isAbilityOfCard compressor)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4
        upgradeIsExhausted (upgradeIdOf compressor) `shouldReturn` True
        resolve [IdentityMessage who ReadiedIdentity]
        choose "attack again" (isHeroAttack who)
        offers (isAbilityOfCard compressor) `shouldReturn` False
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 6

    it "does not enhance an attack event" do
      tg <- spdr
      let who = player1 tg
      inGame tg do
        asSuit who
        compressor <- putIntoPlay who "31013"
        event <- putInHand who "31006"
        resources <- traverse (putInHand who) ["31005", "31005"]
        playWith event resources
        offers (isAbilityOfCard compressor) `shouldReturn` False
        chooseVillain
        pass
        upgradeIsExhausted (upgradeIdOf compressor) `shouldReturn` False
