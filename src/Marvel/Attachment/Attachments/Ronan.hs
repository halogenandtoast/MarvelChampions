-- | Ronan the Accuser's attachments (The Galaxy's Most Wanted, scenario 5), and the Kree
-- Militants modular set's.
module Marvel.Attachment.Attachments.Ronan (
  universalWeapon,
  UniversalWeapon (..),
  fanaticism,
  Fanaticism (..),
  kreeCombatArmor,
  KreeCombatArmor (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window

-- | Whether the modifier's target is the enemy it is attached to.
onItsEnemy :: Attrs Attachment -> Target -> Bool
onItsEnemy a target = case attachmentEnemy a of
  Just (EnemyVillainId vid) -> target == VillainRef vid
  Just (EnemyMinionId mid) -> target == MinionRef mid
  Nothing -> False

universalWeapon :: AttachmentCard UniversalWeapon
universalWeapon = attachment UniversalWeapon Cards.universalWeapon

newtype UniversalWeapon = UniversalWeapon (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Ronan the Accuser gains stalwart.
instance HasModifiersFor UniversalWeapon where
  getModifiersFor _ target (UniversalWeapon a) = pure [KeywordModifier Stalwart | onItsEnemy a target]

-- | Hero Action: take 2 damage and deal yourself 1 facedown encounter card → shuffle
-- Universal Weapon into the encounter deck.
instance HasAbilities UniversalWeapon where
  getAbilities (UniversalWeapon a) =
    [ability a 1 HeroAction NoCriteria (DamageCost 2 <> DealEncounterCardCost) $ runAbility a 1 | attached a]

-- | Attach to Ronan the Accuser. [star] Boost: attach Universal Weapon to him.
instance RunMessage UniversalWeapon where
  runMessage msg a@(UniversalWeapon attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> a <$ attachToTheVillain attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      removeFromGame attrs
      shuffleIntoEncounterDeck [MkEncounterCard (CardId $ unAttachmentId $ attachmentId attrs) (getCardDef attrs)]
      pure a
    -- put into play, it attaches itself as it does revealed
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      a <$ (putBoostIntoPlay attrs =<< getBoostedPlayer enemy)
    _ -> UniversalWeapon <$> runMessage msg attrs

fanaticism :: AttachmentCard Fanaticism
fanaticism = attachment Fanaticism Cards.fanaticism

newtype Fanaticism = Fanaticism (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK.
instance HasModifiersFor Fanaticism where
  getModifiersFor _ target (Fanaticism a) = pure [AttackModifier 1 | onItsEnemy a target]

-- | [star] Forced Interrupt: when Ronan the Accuser attacks you, that attack gains
-- overkill and piercing. At the end of that attack, remove 1 fury counter from here.
instance HasAbilities Fanaticism where
  getAbilities (Fanaticism a) = case attachmentEnemy a of
    Just enemy | attachmentCounters a > 0 ->
      [windowAbility a 1 (EnemyAttacked When (EnemyWithId enemy) You) ForcedInterrupt NoCost $ runAbility a 1]
    _ -> []

-- | Surge (its card's). Attach to Ronan the Accuser. Uses (1 fury counter, plus 1 per
-- player more): with none left it is discarded.
instance RunMessage Fanaticism where
  runMessage msg a@(Fanaticism attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      fury <- fromIntegral <$> fromGameValue (PerPlayer 1)
      attachToTheVillain attrs
      placeCounters attrs (1 + fury)
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> case attachmentEnemy attrs of
      Just enemy@(EnemyVillainId vid) -> do
        attackGains enemy Overkill
        attackGains enemy Piercing
        flip afterMatching (removeCounters attrs 1) \case
          VillainMessage vid' VillainEndAttack -> vid == vid'
          _ -> False
        pure a
      _ -> pure a
    RemoveCounters target n | target == toRef attrs -> do
      when (attachmentCounters attrs <= n) $ removeFromPlay attrs
      Fanaticism <$> runMessage msg attrs
    _ -> Fanaticism <$> runMessage msg attrs

kreeCombatArmor :: AttachmentCard KreeCombatArmor
kreeCombatArmor = attachment KreeCombatArmor Cards.kreeCombatArmor

newtype KreeCombatArmor = KreeCombatArmor (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Reduce the amount of damage attached character takes from each attack by 1.
instance HasModifiersFor KreeCombatArmor where
  getModifiersFor _ target (KreeCombatArmor a) = pure [AttackDamageReduction 1 | onItsEnemy a target]

-- | Hero Action: spend 3 resources of the same type → discard this card. One ability
-- for each type ([wild] counting as any).
instance HasAbilities KreeCombatArmor where
  getAbilities (KreeCombatArmor a) =
    [ label (countLabel key 3) $ ability a n HeroAction NoCriteria (MultiResourceCost (replicate 3 (Just r))) $ DiscardTarget (toRef a)
    | attached a
    , (n, r, key) <- [(1, Physical, "spendPhysicalResources"), (2, Mental, "spendMentalResources"), (3, Energy, "spendEnergyResources")]
    ]

-- | Attach to the enemy with the highest ATK, the player revealing it choosing among
-- those tied (RR Choose); with none, it is discarded.
instance RunMessage KreeCombatArmor where
  runMessage msg a@(KreeCombatArmor attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      enemies <- selectList EnemyWithHighestAttack
      if null enemies
        then removeFromPlay attrs
        else chooseOrRunOne you [targeting (enemyRef enemy) (attachTo ident enemy) | enemy <- enemies]
      pure a
    _ -> KreeCombatArmor <$> runMessage msg attrs
