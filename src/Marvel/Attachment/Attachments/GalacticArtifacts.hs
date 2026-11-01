-- | Infiltrate the Museum's Biogram Image and the Galactic Artifacts modular set's
-- attachments (The Galaxy's Most Wanted), as printed: Cloak of Hercules gives +3 ATK, The
-- Beyonder's Blazer asks for 2 resources of the same type, and the wand is the Vandarian
-- Power Wand.
module Marvel.Attachment.Attachments.GalacticArtifacts (
  biogramImage,
  BiogramImage (..),
  cloakOfHercules,
  CloakOfHercules (..),
  obediencePotion,
  ObediencePotion (..),
  theBeyondersBlazer,
  TheBeyondersBlazer (..),
  thePoison,
  ThePoison (..),
  vandarianPowerWand,
  VandarianPowerWand (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

-- | "Attach to the enemy with the lowest ATK" (or highest, lowest SCH): the first player
-- picks among those tied (RR First Player: an encounter card targeting one of several).
attachToEnemyWith :: (ReverseQueue m) => EnemyMatcher -> Attrs Attachment -> m ()
attachToEnemyWith matcher attrs =
  selectList matcher >>= \case
    [] -> removeFromPlay attrs
    enemies -> do
      first <- selectJust FirstPlayer
      chooseOrRunOne first [targeting (enemyRef e) (attachTo (attachmentId attrs) e) | e <- enemies]

-- | The attached enemy's stat, for the modifiers.
onEnemy :: Attrs Attachment -> Target -> Bool
onEnemy a = \case
  VillainRef vid -> attachmentEnemy a == Just (EnemyVillainId vid)
  MinionRef mid -> attachmentEnemy a == Just (EnemyMinionId mid)
  _ -> False

-- | "Hero Action: Spend these resources → discard this card", on an enemy: anyone's.
discardWith :: Attrs Attachment -> Cost -> [Ability]
discardWith a cost = [ability a 1 HeroAction NoCriteria cost $ DiscardTarget (toRef a) | isJust (attachmentEnemy a)]

biogramImage :: AttachmentCard BiogramImage
biogramImage = attachment BiogramImage Cards.biogramImage

-- | Attach to Collector. Forced Interrupt: when Collector would take any amount of damage,
-- put this card faceup into The Collection → prevent all of that damage, then place
-- threat on the main scheme equal to the amount prevented this way. [star] Boost: after
-- this activation ends, reveal this card.
newtype BiogramImage = BiogramImage (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BiogramImage where
  getAbilities (BiogramImage a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) ->
      [windowAbility a 1 (W.VillainDamaged W.When $ VillainWithId vid) ForcedInterrupt NoCost $ runAbility a 1]
    _ -> []

instance RunMessage BiogramImage where
  runMessage msg a@(BiogramImage attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> a <$ attachToTheVillain attrs
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(vid, damageAmount d) | W.DamagedVillain vid d <- windows, damageAmount d > 0] \(vid, n) -> do
        let damaged = \case
              VillainMessage vid' (VillainDamaged _ _) -> vid' == vid
              _ -> False
        -- the damage is still to come: nothing else has prevented it
        pending <- isQueued damaged
        when pending do
          placeInCollection attrs
          insteadOf damaged $ selectList EncounterMainScheme >>= traverse_ (`placeThreat` n)
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      afterMatching (== ClearBoosts) $ revealEncounterCard you (MkEncounterCard (CardId $ unAttachmentId $ attachmentId attrs) (getCardDef attrs))
      pure a
    _ -> BiogramImage <$> runMessage msg attrs

cloakOfHercules :: AttachmentCard CloakOfHercules
cloakOfHercules = attachment CloakOfHercules Cards.cloakOfHercules

-- | Attach to the enemy with the lowest ATK; it gets +3 ATK. Hero Action: spend
-- [physical][physical][physical] resources → discard this card.
newtype CloakOfHercules = CloakOfHercules (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor CloakOfHercules where
  getModifiersFor _ target (CloakOfHercules a) | onEnemy a target = pure [AttackModifier 3]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CloakOfHercules where
  getAbilities (CloakOfHercules a) = discardWith a (MultiResourceCost [Just Physical, Just Physical, Just Physical])

instance RunMessage CloakOfHercules where
  runMessage msg a@(CloakOfHercules attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs ->
      a <$ attachToEnemyWith EnemyWithLowestAttack attrs
    _ -> CloakOfHercules <$> runMessage msg attrs

obediencePotion :: AttachmentCard ObediencePotion
obediencePotion = attachment ObediencePotion Cards.obediencePotion

-- | Attach to your identity. Attached character gets -1 THW, -1 ATK, and -1 DEF. Hero
-- Action: take 1 damage and spend [mental][mental] resources → discard this card (any
-- player can do this).
newtype ObediencePotion = ObediencePotion (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ObediencePotion where
  getModifiersFor _ (IdentityRef ident) (ObediencePotion a)
    | attachmentIdentity a == Just ident = pure [ThwartPenalty 1, AttackPenalty 1, DefensePenalty 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities ObediencePotion where
  getAbilities (ObediencePotion a) =
    [ ability a 1 HeroAction ForAnyPlayer (DamageCost 1 <> MultiResourceCost [Just Mental, Just Mental]) $ DiscardTarget (toRef a)
    | isJust (attachmentIdentity a)
    ]

instance RunMessage ObediencePotion where
  runMessage msg a@(ObediencePotion attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> a <$ attachToIdentity ident you
    _ -> ObediencePotion <$> runMessage msg attrs

theBeyondersBlazer :: AttachmentCard TheBeyondersBlazer
theBeyondersBlazer = attachment TheBeyondersBlazer Cards.theBeyondersBlazer

-- | Attach to the enemy with the highest SCH. Its amplify icon is its card's. Hero Action:
-- place 2 threat on the main scheme and spend 2 resources of the same type → discard
-- this card. The same type is one ability for each type (a wild resource being any).
newtype TheBeyondersBlazer = TheBeyondersBlazer (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TheBeyondersBlazer where
  getAbilities (TheBeyondersBlazer a) =
    [ labeledAbility (countLabel key 2) $ ability a 1 HeroAction NoCriteria (MultiResourceCost [Just r, Just r]) $ runAbility a 1
    | isJust (attachmentEnemy a)
    , (key, r) <- [("spendPhysicalResources", Physical), ("spendMentalResources", Mental), ("spendEnergyResources", Energy)]
    ]
   where
    labeledAbility l x = x {abilityLabel = Just l}

instance RunMessage TheBeyondersBlazer where
  runMessage msg a@(TheBeyondersBlazer attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs ->
      a <$ attachToEnemyWith EnemyWithHighestScheme attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      selectList EncounterMainScheme >>= traverse_ (`placeThreat` 2)
      removeFromPlay attrs
      pure a
    _ -> TheBeyondersBlazer <$> runMessage msg attrs

thePoison :: AttachmentCard ThePoison
thePoison = attachment ThePoison Cards.thePoison

-- | Attach to your identity. Forced Interrupt: when your turn begins, place 1 poison
-- counter here, then take 1 damage for each poison counter here. Hero Action: spend 3
-- resources of different types → discard this card (any player can do this).
newtype ThePoison = ThePoison (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ThePoison where
  getAbilities (ThePoison a) = case attachmentIdentity a of
    Just you ->
      [ windowAbility a 1 (W.TurnBegins $ IdentityWithId you) ForcedInterrupt NoCost $ runAbility a 1
      , ability a 2 HeroAction ForAnyPlayer (DifferentResourcesCost 3) $ DiscardTarget (toRef a)
      ]
    Nothing -> []

instance RunMessage ThePoison where
  runMessage msg a@(ThePoison attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> a <$ attachToIdentity ident you
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      placeCounters attrs 1
      cardSpecific attrs "poisoned" ()
      pure a
    CardSpecific (isTarget attrs -> True) "poisoned" _ -> do
      for_ (attachmentIdentity attrs) \you ->
        damageIdentity you attrs (toDamage (attachmentCounters attrs) FromAbility)
      pure a
    _ -> ThePoison <$> runMessage msg attrs

vandarianPowerWand :: AttachmentCard VandarianPowerWand
vandarianPowerWand = attachment VandarianPowerWand Cards.vandarianPowerWand

-- | Attach to the enemy with the lowest SCH. Its hazard icon is its card's. Hero Action:
-- spend [energy][energy][energy] resources → discard this card.
newtype VandarianPowerWand = VandarianPowerWand (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities VandarianPowerWand where
  getAbilities (VandarianPowerWand a) = discardWith a (MultiResourceCost [Just Energy, Just Energy, Just Energy])

instance RunMessage VandarianPowerWand where
  runMessage msg a@(VandarianPowerWand attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs ->
      a <$ attachToEnemyWith EnemyWithLowestScheme attrs
    _ -> VandarianPowerWand <$> runMessage msg attrs
