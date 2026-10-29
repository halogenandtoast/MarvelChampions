module Marvel.Encounter.NebulaNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity)
import Marvel.Keyword
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade)
import Marvel.Ability (HasAbilities)
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport hiding (allyDamage)

newtype TestModifiers = TestModifiers (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage TestModifiers where
  runMessage msg (TestModifiers a) = TestModifiers <$> runMessage msg a

giveModifiers :: EntityMatcher -> [Modifier] -> GameAppT ()
giveModifiers matcher mods = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (TestModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = mods, effectEnds = Nothing })

newtype ProtectedUpgrade = ProtectedUpgrade (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
instance HasModifiersFor ProtectedUpgrade where
  getModifiersFor _ (UpgradeRef uid) (ProtectedUpgrade attrs)
    | uid == upgradeId attrs = pure [CannotBeDiscarded]
  getModifiersFor _ _ _ = pure []
instance RunMessage ProtectedUpgrade where
  runMessage msg (ProtectedUpgrade attrs) = ProtectedUpgrade <$> runMessage msg attrs

protect :: PlayerCard -> GameAppT ()
protect card = withGame_ $ entitiesL . upgradesL . ix (upgradeIdOf card) %~ (Upgrade . ProtectedUpgrade . toAttrs)

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

stats :: IdentityId -> GameAppT (Natural, Natural, Natural)
stats p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> (,,) <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
  _ -> pure (0, 0, 0)

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

attachmentAttrs :: EncounterCard -> GameAppT (Attrs Attachment.Attachment)
attachmentAttrs c = getsGame (maybe (error "missing attachment") toAttrs . HashMap.lookup (attachmentOf c) . gameAttachments)

hasAttachment :: EncounterCard -> GameAppT Bool
hasAttachment c = getsGame (HashMap.member (attachmentOf c) . gameAttachments)

hasUpgrade :: PlayerCard -> GameAppT Bool
hasUpgrade c = selectAny (UpgradeWithId $ upgradeIdOf c)

allyDamage :: AllyId -> GameAppT Natural
allyDamage aid = getsGame (maybe 0 (Ally.allyDamage . toAttrs) . HashMap.lookup aid . gameAllies)

-- A marker card is dealt by surge, never revealed as part of this assertion.
prepareSurge :: GameAppT EncounterCard
prepareSurge = do
  [marker] <- stackEncounterDeck [Minions.hydraMercenary]
  pure marker

wasDealt :: IdentityId -> EncounterCard -> GameAppT Bool
wasDealt p c = elem (ecCardId c) . map ecCardId <$> dealtTo p

chooseDiscardTechniques :: GameAppT ()
chooseDiscardTechniques = choose "discard Techniques" (isLabel "$label.discardTechniquesInferiorityComplex")

spec :: Spec
spec = describe "Nebula nemesis" do
  describe "Inferiority Complex" do
    it "allows flipping to alter-ego, exhausting it and removing the obligation" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- revealNow p Obligations.inferiorityComplex
        choose "flip" (isLabel "$label.changeToB")
        choose "exhaust and remove" (isLabel "$label.exhaustAlterEgoRemoveInferiorityComplex")
        pass
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        identityMatches AlterEgoIdentity p `shouldReturn` True

    it "can stay in hero form and discards two chosen controlled Techniques" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        asHero p
        one <- putIntoPlay p "22004"
        two <- putIntoPlay p "22007"
        spare <- putIntoPlay p "22008"
        marker <- prepareSurge
        _ <- revealNow p Obligations.inferiorityComplex
        choose "stay hero" (isLabel "$label.doNotFlipToAlterEgo")
        choose "first" (isTargetChoice $ upgradeIdOf two)
        choose "second" (isTargetChoice $ upgradeIdOf one)
        pass
        hasUpgrade one `shouldReturn` False
        hasUpgrade two `shouldReturn` False
        hasUpgrade spare `shouldReturn` True
        wasDealt p marker `shouldReturn` False
        identityMatches HeroIdentity p `shouldReturn` True

    it "discards a sole Technique without surge, resolving as much as possible" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        one <- putIntoPlay p "22004"
        marker <- prepareSurge
        _ <- revealNow p Obligations.inferiorityComplex
        chooseDiscardTechniques
        choose "only Technique" (isTargetChoice $ upgradeIdOf one)
        pass
        hasUpgrade one `shouldReturn` False
        wasDealt p marker `shouldReturn` False

    it "surges when no Technique can be discarded, ignoring other upgrades" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        ordinary <- putIntoPlay p "01065"
        marker <- prepareSurge
        _ <- revealNow p Obligations.inferiorityComplex
        chooseDiscardTechniques
        pass
        hasUpgrade ordinary `shouldReturn` True
        wasDealt p marker `shouldReturn` True

    it "cannot pay by exhausting an already exhausted alter-ego" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p ExhaustedIdentity]
        marker <- prepareSurge
        _ <- revealNow p Obligations.inferiorityComplex
        pass
        offers (isLabel "$label.exhaustAlterEgoRemoveInferiorityComplex") `shouldReturn` False
        wasDealt p marker `shouldReturn` True

    it "does not discard another player's Technique or protected Techniques" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        protected <- putIntoPlay p "22004"
        otherUpgrade <- putIntoPlay other "22007"
        protect protected
        marker <- prepareSurge
        _ <- revealNow p Obligations.inferiorityComplex
        chooseDiscardTechniques
        pass
        hasUpgrade protected `shouldReturn` True
        hasUpgrade otherUpgrade `shouldReturn` True
        wasDealt p marker `shouldReturn` True

  describe "Gamora nemesis" do
    it "discards the associated Gamora ally before unique entry completes" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22002"
        _ <- revealNow p Minions.gamoraNemesis
        pass
        length <$> alliesWith "22002" `shouldReturn` 0
        selectAny (MinionIs Minions.gamoraNemesis) `shouldReturn` True

    it "also discards her ally when put into play without revealing" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22002"
        c <- encounterCard Minions.gamoraNemesis
        resolve [PutEncounterCardIntoPlay p c]
        length <$> alliesWith "22002" `shouldReturn` 0
        selectAny (MinionIs Minions.gamoraNemesis) `shouldReturn` True

    it "forces the damaged player to choose one of their upgrades" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        one <- putIntoPlay p "22004"
        two <- putIntoPlay p "22008"
        otherUpgrade <- putIntoPlay other "01065"
        gamora <- addMinion p Minions.gamoraNemesis
        runNow [MinionMessage gamora $ MinionAttacks p]
        pass
        choose "Gamora forced response" (isAbilityOf gamora)
        choose "discard upgrade" (isTargetChoice $ upgradeIdOf two)
        pass
        hasUpgrade two `shouldReturn` False
        hasUpgrade one `shouldReturn` True
        hasUpgrade otherUpgrade `shouldReturn` True
        damageOf p `shouldReturn` 2

    it "does not discard an upgrade when tough prevents all attack damage" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        one <- putIntoPlay p "22004"
        gamora <- addMinion p Minions.gamoraNemesis
        resolve [GiveStatus (IdentityRef p) ToughStatus, MinionMessage gamora $ MinionAttacks p]
        hasUpgrade one `shouldReturn` True
        damageOf p `shouldReturn` 0

    it "does not trigger when Gamora damages an ally defender" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        one <- putIntoPlay p "22004"
        ally <- putIntoPlay p "01051"
        gamora <- addMinion p Minions.gamoraNemesis
        runNow [MinionMessage gamora $ MinionAttacks p]
        choose "ally defends" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
        pass
        hasUpgrade one `shouldReturn` True
        damageOf p `shouldReturn` 0

    it "finishes an attack with no upgrades and never asks an empty question" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        gamora <- addMinion p Minions.gamoraNemesis
        resolve [MinionMessage gamora $ MinionAttacks p]
        damageOf p `shouldReturn` 2

  describe "Self-Preservation" do
    it "starts with two threat per player and penalizes all three Nebula hero powers" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.selfPreservation
        pass
        sideSchemeThreat scheme `shouldReturn` 4
        stats p `shouldReturn` (1, 1, 1)

    it "penalizes Nebula only and stops when the scheme leaves play" do
      tg <- testGame ["nebula", "gamora"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        runNow [IdentityMessage other $ ChangedToForm A]
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.selfPreservation
        pass
        stats other `shouldReturn` (2, 2, 2)
        resolve [RemoveFromPlay $ SideSchemeRef scheme]
        stats p `shouldReturn` (2, 2, 2)

    it "gives the associated Gamora ally +1 ATK and piercing, even against a friendly identity" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "22002"
        _ <- revealNow p SideSchemes.selfPreservation
        pass
        resolve [GiveStatus (IdentityRef p) ToughStatus, FriendlyCharacterAttacks (AllyCharacter $ allyIdOf ally) p]
        damageOf p `shouldReturn` 3
        allyDamage (allyIdOf ally) `shouldReturn` 1

    it "gives nemesis Gamora +1 ATK and piercing" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        gamora <- addMinion p Minions.gamoraNemesis
        _ <- revealNow p SideSchemes.selfPreservation
        pass
        resolve [GiveStatus (IdentityRef p) ToughStatus, MinionMessage gamora $ MinionAttacks p]
        damageOf p `shouldReturn` 3

    it "does not buff Gamora's hero, which belongs to a different identity" do
      tg <- testGame ["nebula", "gamora"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        runNow [IdentityMessage other $ ChangedToForm A]
        _ <- revealNow p SideSchemes.selfPreservation
        pass
        stats other `shouldReturn` (2, 2, 2)
        resolve [GiveStatus (IdentityRef p) ToughStatus, FriendlyCharacterAttacks (IdentityCharacter other) p]
        damageOf p `shouldReturn` 0

  describe "Lethal Weapon" do
    it "attaches to the associated Gamora ally and applies its printed +1 ATK" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "22002"
        weapon <- revealNow p Attachments.lethalWeapon
        pass
        Attachment.attachmentAlly <$> attachmentAttrs weapon `shouldReturn` Just (allyIdOf ally)
        resolve [FriendlyCharacterAttacks (AllyCharacter $ allyIdOf ally) p]
        damageOf p `shouldReturn` 3

    it "attaches to nemesis Gamora and gives +1 ATK" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        gamora <- addMinion p Minions.gamoraNemesis
        weapon <- revealNow p Attachments.lethalWeapon
        pass
        Attachment.attachmentEnemy <$> attachmentAttrs weapon `shouldReturn` Just (EnemyMinionId gamora)
        resolve [MinionMessage gamora $ MinionAttacks p]
        damageOf p `shouldReturn` 3

    it "falls back to the villain, even if the other player's Gamora hero is in play" do
      tg <- testGame ["nebula", "gamora"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        runNow [IdentityMessage other $ ChangedToForm A]
        weapon <- revealNow p Attachments.lethalWeapon
        pass
        villain <- villainIdNow
        Attachment.attachmentEnemy <$> attachmentAttrs weapon `shouldReturn` Just (EnemyVillainId villain)
        stats other `shouldReturn` (2, 2, 2)

    it "Hero Action discards a chosen controlled upgrade then this attachment" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        asHero p
        upgrade <- putIntoPlay p "22004"
        weapon <- revealNow p Attachments.lethalWeapon
        pass
        choose "remove weapon" (isAbilityOf $ attachmentOf weapon)
        pass
        hasUpgrade upgrade `shouldReturn` False
        hasAttachment weapon `shouldReturn` False

    it "cannot use the Hero Action in alter-ego form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22004"
        weapon <- revealNow p Attachments.lethalWeapon
        pass
        offers (isAbilityOf $ attachmentOf weapon) `shouldReturn` False

    it "does not spend another player's or a protected upgrade" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        protected <- putIntoPlay p "22004"
        _ <- putIntoPlay other "01065"
        protect protected
        weapon <- revealNow p Attachments.lethalWeapon
        pass
        offers (isAbilityOf $ attachmentOf weapon) `shouldReturn` False

  describe "Old Rivals" do
    it "makes the Gamora ally attack without exhausting and take consequential damage" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "22002"
        marker <- prepareSurge
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 2
        allyDamage (allyIdOf ally) `shouldReturn` 1
        getsGame (fmap (Ally.allyExhausted . toAttrs) . HashMap.lookup (allyIdOf ally) . gameAllies) `shouldReturn` Just False
        wasDealt p marker `shouldReturn` False

    it "also attacks with an already exhausted ally" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "22002"
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 2
        allyDamage (allyIdOf ally) `shouldReturn` 1

    it "explicitly permits the Gamora hero to attack, without exhausting her" do
      tg <- testGame ["nebula", "gamora"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        runNow [IdentityMessage other $ ChangedToForm A]
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 2
        isExhaustedNow other `shouldReturn` False

    it "does not treat Gamora's alter-ego as her hero and surges without Gamora" do
      tg <- testGame ["nebula", "gamora"]
      let p = player1 tg
      inGame tg do
        marker <- prepareSurge
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 0
        wasDealt p marker `shouldReturn` True

    it "has nemesis Gamora make an ordinary attack against an alter-ego" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.gamoraNemesis
        marker <- prepareSurge
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 2
        wasDealt p marker `shouldReturn` False

    it "surges if stun replaces the ally's attack, consuming stun but no consequential damage" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "22002"
        resolve [GiveStatus (AllyRef $ allyIdOf ally) StunnedStatus]
        marker <- prepareSurge
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 0
        allyDamage (allyIdOf ally) `shouldReturn` 0
        getsGame (fmap (Ally.allyStunned . toAttrs) . HashMap.lookup (allyIdOf ally) . gameAllies) `shouldReturn` Just False
        wasDealt p marker `shouldReturn` True

    it "surges if Gamora is prohibited from attacking" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "22002"
        giveModifiers (AllyEntity $ AllyWithId $ allyIdOf ally) [CannotAttack]
        marker <- prepareSurge
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 0
        allyDamage (allyIdOf ally) `shouldReturn` 0
        wasDealt p marker `shouldReturn` True

    it "does not surge when tough prevents damage from an attack that was made" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "22002"
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        marker <- prepareSurge
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 0
        allyDamage (allyIdOf ally) `shouldReturn` 1
        wasDealt p marker `shouldReturn` False

    it "the ally's consequential damage can defeat her after attacking" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "22002"
        resolve [AllyMessage (allyIdOf ally) $ AllyDamaged GameRef (toDamage 2 FromAbility)]
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 2
        length <$> alliesWith "22002" `shouldReturn` 0

    it "a stunned nemesis removes stun instead of attacking and surges" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        gamora <- addMinion p Minions.gamoraNemesis
        resolve [GiveStatus (MinionRef gamora) StunnedStatus]
        marker <- prepareSurge
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 0
        selectAny (MinionWithId gamora <> StunnedMinion) `shouldReturn` False
        wasDealt p marker `shouldReturn` True

    it "a stunned Gamora hero also replaces her attack and causes surge" do
      tg <- testGame ["nebula", "gamora"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        runNow [IdentityMessage other $ ChangedToForm A, GiveStatus (IdentityRef other) StunnedStatus]
        marker <- prepareSurge
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 0
        isStunned other `shouldReturn` False
        wasDealt p marker `shouldReturn` True

  describe "transformed Gamora references and protected attachment costs" do
    it "Self-Preservation still buffs the associated Gamora when Beguiled makes her a minion" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        gamora <- putIntoPlay p "22002"
        _ <- revealNow p Attachments.beguiled
        pass
        let mid = MinionId $ unAllyId $ allyIdOf gamora
        _ <- revealNow p SideSchemes.selfPreservation
        pass
        resolve [GiveStatus (IdentityRef p) ToughStatus, MinionMessage mid $ MinionAttacks p]
        damageOf p `shouldReturn` 3

    it "Lethal Weapon attaches to the associated Gamora transformed into a minion" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        gamora <- putIntoPlay p "22002"
        _ <- revealNow p Attachments.beguiled
        pass
        let mid = MinionId $ unAllyId $ allyIdOf gamora
        weapon <- revealNow p Attachments.lethalWeapon
        pass
        Attachment.attachmentEnemy <$> attachmentAttrs weapon `shouldReturn` Just (EnemyMinionId mid)
        resolve [MinionMessage mid $ MinionAttacks p]
        damageOf p `shouldReturn` 3

    it "Old Rivals does not treat a Beguiled ally as the specifically named nemesis minion" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22002"
        _ <- revealNow p Attachments.beguiled
        pass
        marker <- prepareSurge
        _ <- revealNow p Treacheries.oldRivals
        pass
        damageOf p `shouldReturn` 0
        wasDealt p marker `shouldReturn` True

    it "Powerful Enchantments blocks Lethal Weapon's action before an upgrade can be paid" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "22002"
        upgrade <- putIntoPlay p "22004"
        weapon <- revealNow p Attachments.lethalWeapon
        pass
        _ <- revealNow p SideSchemes.powerfulEnchantments
        pass
        offers (isAbilityOf $ attachmentOf weapon) `shouldReturn` False
        hasUpgrade upgrade `shouldReturn` True
        hasAttachment weapon `shouldReturn` True
