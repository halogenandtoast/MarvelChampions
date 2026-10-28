-- | Tower Defense's attachments (The Mad Titan's Shadow).
module Marvel.Attachment.Attachments.TowerDefense (
  focusedDefense,
  FocusedDefense (..),
  proximasSpear,
  ProximasSpear (..),
  corvussGlaive,
  CorvussGlaive (..),
  directAssault,
  DirectAssault (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity (isTarget)
import Marvel.Criteria
import Marvel.Environment.Environments.AvengersTower (damageAvengersTower)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.MainScheme.Cards qualified as MainSchemes
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window

-- * Focused Defense

focusedDefense :: AttachmentCard FocusedDefense
focusedDefense = attachment FocusedDefense Cards.focusedDefense

newtype FocusedDefense = FocusedDefense (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | While it is in play, "the villain" of a card a player plays or an ability they
-- trigger is the one they choose (MTS rulebook p.11).
instance HasModifiersFor FocusedDefense where
  getModifiersFor _ GameRef _ = pure [PlayersChooseTheVillain]
  getModifiersFor _ _ _ = pure []

-- | Permanent. Put into play attached to The Armies of Thanos (2A); "The villain who
-- matches the attached scheme is the active villain." Forced Response: after the player
-- phase ends, attach this card to the other main scheme.
instance RunMessage FocusedDefense where
  runMessage msg a@(FocusedDefense attrs) = runQueueT $ case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      -- permanent: once on a scheme it stays there, whatever would make it leave play
      RevealAttachment _ | isNothing (attachmentMainScheme attrs) -> do
        aid <- getAreaOfTarget attrs
        schemes <- getMainSchemesIn aid
        armies <- filterM (fmap (== Just (toCardCode MainSchemes.theArmiesOfThanos)) . getMainSchemeCode) schemes
        for_ (take 1 $ armies <> schemes) (attachToMainScheme ident)
        pure a
      RevealAttachment _ -> pure a
      AttachedToMainScheme mid -> do
        villains <- selectList AnyVillain
        paired <- filterM (fmap (== Just mid) . getPairedMainScheme) villains
        for_ (take 1 paired) setActiveVillain
        FocusedDefense <$> runMessage msg attrs
      _ -> FocusedDefense <$> runMessage msg attrs
    EndPhase PlayerPhase -> do
      for_ (attachmentMainScheme attrs) \current -> do
        schemes <- getMainSchemesIn =<< getAreaOfTarget attrs
        for_ (take 1 $ filter (/= current) schemes) (attachToMainScheme (attachmentId attrs))
      pure a
    _ -> FocusedDefense <$> runMessage msg attrs

-- * The villains' weapons

-- | "Attach to <villain>": with him not in play, the card is discarded.
attachToNamed :: (ReverseQueue m) => Text -> Attrs Attachment -> m ()
attachToNamed name attrs = villainNamed name >>= maybe (removeFromPlay attrs) (attachTo (attachmentId attrs))

-- | Hero Action: take 1 damage and spend the resources → discard this card.
discardAbility :: Attrs Attachment -> [Maybe Resource] -> [Ability]
discardAbility a resources =
  [ability a 1 HeroAction NoCriteria (DamageCost 1 <> MultiResourceCost resources) $ DiscardTarget (toRef a) | isJust (attachmentEnemy a)]

attachedVillain :: Attrs Attachment -> VillainId -> Bool
attachedVillain a vid = attachmentEnemy a == Just (EnemyVillainId vid)

proximasSpear :: AttachmentCard ProximasSpear
proximasSpear = attachment ProximasSpear Cards.proximasSpear

newtype ProximasSpear = ProximasSpear (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK. [star] Proxima Midnight's attacks gain overkill and piercing.
instance HasModifiersFor ProximasSpear where
  getModifiersFor _ (VillainRef vid) (ProximasSpear a)
    | attachedVillain a vid = pure [AttackModifier 1, KeywordModifier Overkill, KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: take 1 damage and spend [energy] [mental] resources → discard this card.
instance HasAbilities ProximasSpear where
  getAbilities (ProximasSpear a) = discardAbility a [Just Energy, Just Mental]

-- | Attach to Proxima Midnight.
instance RunMessage ProximasSpear where
  runMessage msg a@(ProximasSpear attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> a <$ attachToNamed "Proxima Midnight" attrs
    _ -> ProximasSpear <$> runMessage msg attrs

corvussGlaive :: AttachmentCard CorvussGlaive
corvussGlaive = attachment CorvussGlaive Cards.corvussGlaive

newtype CorvussGlaive = CorvussGlaive (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Corvus Glaive gains retaliate 1.
instance HasModifiersFor CorvussGlaive where
  getModifiersFor _ (VillainRef vid) (CorvussGlaive a)
    | attachedVillain a vid = pure [KeywordModifier (Retaliate 1)]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: take 1 damage and spend [energy] [physical] resources → discard this
-- card.
instance HasAbilities CorvussGlaive where
  getAbilities (CorvussGlaive a) = discardAbility a [Just Energy, Just Physical]

-- | Attach to Corvus Glaive.
instance RunMessage CorvussGlaive where
  runMessage msg a@(CorvussGlaive attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> a <$ attachToNamed "Corvus Glaive" attrs
    _ -> CorvussGlaive <$> runMessage msg attrs

-- * Direct Assault

directAssault :: AttachmentCard DirectAssault
directAssault = attachment DirectAssault Cards.directAssault

newtype DirectAssault = DirectAssault (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +2 ATK.
instance HasModifiersFor DirectAssault where
  getModifiersFor _ (VillainRef vid) (DirectAssault a)
    | attachedVillain a vid = pure [AttackModifier 2]
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Interrupt: when attached villain attacks, the attack gains ranged. If
-- that attack defeats an ally, deal 3 damage to Avengers Tower. At the end of that
-- attack, discard Direct Assault.
instance HasAbilities DirectAssault where
  getAbilities (DirectAssault a) = case attachmentEnemy a of
    Just enemy@(EnemyVillainId _) ->
      [ windowAbility a 1 (EnemyAttacked When (EnemyWithId enemy) AnyIdentity) ForcedInterrupt NoCost $ runAbility a 1
      , windowAbility a 2 (AllyDefeatedByAttackOf When (EnemyWithId enemy)) ForcedInterrupt NoCost $ runAbility a 2
      ]
    _ -> []

-- | Attach to the villain who is not the active villain: with none, it is discarded.
instance RunMessage DirectAssault where
  runMessage msg a@(DirectAssault attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      active <- selectOne ActiveVillain
      others <- filter ((/= active) . Just) <$> selectList AnyVillain
      maybe (removeFromPlay attrs) (attachTo ident) (headMay others)
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ [v | Just (EnemyVillainId v) <- [attachmentEnemy attrs]] \v -> do
        attackGains v Ranged
        flip afterMatching (removeFromPlay attrs) \case
          VillainMessage v' VillainEndAttack -> v == v'
          _ -> False
      pure a
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> a <$ damageAvengersTower 3
    _ -> DirectAssault <$> runMessage msg attrs
