-- | Nebula's Technique attachments (The Galaxy's Most Wanted, Nebula): each attaches to
-- Nebula, holds while it is there, and has a "Special" ability that Nebula's forced
-- interrupt, Combat Ready and its own boost resolve ('resolveSpecial').
module Marvel.Attachment.Attachments.Techniques (
  cutthroatAmbition,
  CutthroatAmbition (..),
  evasiveManeuvering,
  EvasiveManeuvering (..),
  unyieldingPersistence,
  UnyieldingPersistence (..),
  weaponMastery,
  WeaponMastery (..),
  wideStance,
  WideStance (..),
  resolveSpecial,
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

-- | "Resolve its Special ability", for the player it is against.
resolveSpecial :: (ReverseQueue m) => AttachmentId -> IdentityId -> m ()
resolveSpecial aid = cardSpecific (AttachmentRef aid) "techniqueSpecial"

-- | What every Technique does but hold and its Special: "Attach to Nebula" as it is
-- revealed; its Special resolved for the player, against the villain; and "[star] Boost:
-- after this activation ends, attach this card to Nebula and resolve its Special" (for
-- the player the activation was against). Attached that way it is not revealed.
runTechnique :: (ReverseQueue m) => (IdentityId -> VillainId -> m ()) -> Message -> Attrs Attachment -> m (Attrs Attachment)
runTechnique special msg attrs = case msg of
  AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> attrs <$ attachToTheVillain attrs
  CardSpecific (isTarget attrs -> True) "techniqueSpecial" (fromJSON -> Success you) -> do
    specialAbility you attrs $ selectOne ActiveVillain >>= traverse_ (special you)
    pure attrs
  Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
    you <- getBoostedPlayer enemy
    let card = MkEncounterCard (CardId $ unAttachmentId $ attachmentId attrs) (getCardDef attrs)
    selectOne ActiveVillain >>= traverse_ \villain ->
      afterMatching (== ClearBoosts) do
        attachEncounterCard card villain
        resolveSpecial (attachmentId attrs) you
    pure attrs
  _ -> runMessage msg attrs

-- | What the Technique gives the villain it is attached to.
onNebula :: (Applicative m) => Attrs Attachment -> Target -> [Modifier] -> m [Modifier]
onNebula a (VillainRef vid) ms | attachmentEnemy a == Just (EnemyVillainId vid) = pure ms
onNebula _ _ _ = pure []

-- | If the villain already has what the Special gives, 1 facedown boost card instead.
orBoost :: (ReverseQueue m) => Bool -> VillainId -> m () -> m ()
orBoost already villain body = if already then dealBoost villain else body

cutthroatAmbition :: AttachmentCard CutthroatAmbition
cutthroatAmbition = attachment CutthroatAmbition Cards.cutthroatAmbition

-- | Nebula cannot take more than 5 damage from a single attack. Special: place 1 threat
-- on the main scheme.
newtype CutthroatAmbition = CutthroatAmbition (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor CutthroatAmbition where
  getModifiersFor _ target (CutthroatAmbition a) = onNebula a target [AttackDamageCap 5]

instance RunMessage CutthroatAmbition where
  runMessage msg (CutthroatAmbition attrs) = runQueueT $ CutthroatAmbition <$> runTechnique special msg attrs
   where
    special _ _ = selectOne EncounterMainScheme >>= traverse_ (`placeThreat` 1)

evasiveManeuvering :: AttachmentCard EvasiveManeuvering
evasiveManeuvering = attachment EvasiveManeuvering Cards.evasiveManeuvering

-- | Nebula gains stalwart. Special: you are stunned; already stunned, give Nebula 1
-- facedown boost card.
newtype EvasiveManeuvering = EvasiveManeuvering (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor EvasiveManeuvering where
  getModifiersFor _ target (EvasiveManeuvering a) = onNebula a target [KeywordModifier Stalwart]

instance RunMessage EvasiveManeuvering where
  runMessage msg (EvasiveManeuvering attrs) = runQueueT $ EvasiveManeuvering <$> runTechnique special msg attrs
   where
    special you villain = do
      stunned <- identityMatches StunnedIdentity you
      orBoost stunned villain $ stunIdentity you

unyieldingPersistence :: AttachmentCard UnyieldingPersistence
unyieldingPersistence = attachment UnyieldingPersistence Cards.unyieldingPersistence

-- | Nebula gains stalwart. Special: give Nebula a tough status card; already tough, give
-- her 1 facedown boost card.
newtype UnyieldingPersistence = UnyieldingPersistence (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor UnyieldingPersistence where
  getModifiersFor _ target (UnyieldingPersistence a) = onNebula a target [KeywordModifier Stalwart]

instance RunMessage UnyieldingPersistence where
  runMessage msg (UnyieldingPersistence attrs) = runQueueT $ UnyieldingPersistence <$> runTechnique special msg attrs
   where
    special _ villain = do
      tough <- selectAny (VillainWithId villain <> VillainWithToughStatus)
      orBoost tough villain $ enemyBecomesTough villain

weaponMastery :: AttachmentCard WeaponMastery
weaponMastery = attachment WeaponMastery Cards.weaponMastery

-- | Nebula gains retaliate 1. Special: take 1 damage.
newtype WeaponMastery = WeaponMastery (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor WeaponMastery where
  getModifiersFor _ target (WeaponMastery a) = onNebula a target [KeywordModifier (Retaliate 1)]

instance RunMessage WeaponMastery where
  runMessage msg (WeaponMastery attrs) = runQueueT $ WeaponMastery <$> runTechnique special msg attrs
   where
    special you _ = damageIdentity you attrs (toDamage 1 FromAbility)

wideStance :: AttachmentCard WideStance
wideStance = attachment WideStance Cards.wideStance

-- | Reduce the amount of damage Nebula takes from each attack by 1. Special: discard 1
-- card at random from your hand.
newtype WideStance = WideStance (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor WideStance where
  getModifiersFor _ target (WideStance a) = onNebula a target [AttackDamageReduction 1]

instance RunMessage WideStance where
  runMessage msg (WideStance attrs) = runQueueT $ WideStance <$> runTechnique special msg attrs
   where
    special you _ = discardFrom you RandomFromHand 1
