module Marvel.CivilWar.Namor where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.Resource
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype Namor = Namor (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

namor :: MinionCard Namor
namor = minion Namor C.cw56160 (Sch 1) (Atk 1) (HP 6)

instance RunMessage Namor where
  runMessage msg e@(Namor a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> e <$ minionActivatesAgainst mid you
    _ -> Namor <$> runMessage msg a

newtype NamorEquipment = NamorEquipment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

namorEquipment :: [AttachmentCard NamorEquipment]
namorEquipment = map (attachment NamorEquipment) [C.cw56161,C.cw56162]

instance HasModifiersFor NamorEquipment where
  getModifiersFor _ target (NamorEquipment a)
    | Just target == (enemyRef <$> attachmentEnemy a) || Just target == (AllyRef <$> attachmentAlly a) =
        pure $ if toCardCode a == "56161" then [AttackModifier 1,KeywordModifier K.Piercing] else [SchemeModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities NamorEquipment where
  getAbilities (NamorEquipment a) =
    [ability a 1 HeroAction NoCriteria (MultiResourceCost $ map Just resources) (DiscardTarget $ toRef a)]
    <> [windowAbility a 2 window ForcedInterrupt NoCost (runAbility a 2)
       | toCardCode a == "56162", Just enemy <- [attachmentEnemy a]
       , window <- case enemy of
           EnemyVillainId vid -> [W.VillainSchemed W.When $ VillainWithId vid]
           EnemyMinionId mid -> [W.MinionHasSchemed W.When $ MinionWithId mid]
           _ -> []]
   where resources = if toCardCode a == "56161" then [Energy,Physical] else [Mental,Physical]

instance RunMessage NamorEquipment where
  runMessage msg e@(NamorEquipment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      minions <- asYou you $ selectList $ MinionIs C.cw56160
      allies <- asYou you $ selectList $ AllyIs C.cw56160
      eligibleMinions <- filterM (canAttach . toTarget) minions
      eligibleAllies <- filterM (canAttach . toTarget) allies
      choices <- if null eligibleMinions && null eligibleAllies then do
        leader <- enemyLeaderFor you
        eligible <- filterM (canAttach . toTarget) [vid | Just vid <- [leader]]
        pure [targeting (VillainRef vid) (attachTo aid vid) | vid <- eligible]
      else pure $ [targeting (MinionRef mid) (attachTo aid mid) | mid <- eligibleMinions]
        <> [targeting (AllyRef ally) (attachToAlly aid ally) | ally <- eligibleAllies]
      if null choices then removeFromPlay a else chooseOrRunOne you choices
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> e <$ traverse_ (`giveStatus` ToughStatus) (attachmentEnemy a)
    _ -> NamorEquipment <$> runMessage msg a
   where
    canAttach target = do
      modifiers <- getModifiers target
      pure $ all (`notElem` modifiers) [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]

newtype ImperiusRex = ImperiusRex (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

imperiusRex :: TreacheryCard ImperiusRex
imperiusRex = treachery ImperiusRex C.cw56163

instance RunMessage ImperiusRex where
  runMessage msg e@(ImperiusRex a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      namors <- asYou you $ selectList $ MinionIs C.cw56160
      if null namors then enemyLeaderFor you >>= traverse_ (\vid -> enemyActivationAfterCurrent vid $ villainActivatesAgainst vid you)
      else chooseOrRunOne you [targeting (enemyRef $ EnemyMinionId mid) (enemyActivationAfterCurrent mid $ minionActivatesAgainst mid you) | mid <- namors]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> e <$ dealBoost (enemyRef enemy)
    _ -> ImperiusRex <$> runMessage msg a

newtype RulerOfAtlantis = RulerOfAtlantis (Attrs SideScheme)
  deriving anyclass IsSideScheme
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

rulerOfAtlantis :: SideSchemeCard RulerOfAtlantis
rulerOfAtlantis = sideScheme RulerOfAtlantis C.cw56164 (Static 3)

instance HasModifiersFor RulerOfAtlantis where
  getModifiersFor _ (MinionRef mid) _ = do
    atlantean <- minionMatches (MinionWithTrait T.Atlantis) mid
    pure [CannotTakeDamage | atlantean]
  getModifiersFor _ _ _ = pure []

instance RunMessage RulerOfAtlantis where
  runMessage msg (RulerOfAtlantis a) = runQueueT $ RulerOfAtlantis <$> runMessage msg a
