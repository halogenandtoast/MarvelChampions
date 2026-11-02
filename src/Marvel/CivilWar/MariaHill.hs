module Marvel.CivilWar.MariaHill where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype MariaHill = MariaHill (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

mariaHill :: MinionCard MariaHill
mariaHill = minion MariaHill C.cw56083 (Sch 2) (Atk 2) (HP 4)

canAttachDecoy :: HasGame m => EnemyId -> m Bool
canAttachDecoy enemy = do
  modifiers <- getModifiers (enemyRef enemy)
  existing <- selectAny $ AttachmentIs C.cw56084 <> AttachmentOnEnemy enemy
  pure $ not existing && CannotHaveAttachedCards `notElem` modifiers && CannotHaveAttachedEncounterCards `notElem` modifiers

instance RunMessage MariaHill where
  runMessage msg e@(MariaHill a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      deck <- asYou you getEncounterDeck
      discard <- asYou you getEncounterDiscard
      key <- getDeckKeyFor you
      eligible <- canAttachDecoy (EnemyMinionId mid)
      let inDeck = filter ((== "56084") . toCardCode) deck
          inDiscard = filter ((== "56084") . toCardCode) discard
      when eligible $ lookForEnemyTeam you a (inDeck <> inDiscard)
        [labeled label (attachEncounterCard card mid)
        | (label,Just card) <- [("$label.cwDecoyFromDeck",headMay inDeck),("$label.cwDecoyFromDiscard",headMay inDiscard)]]
      fromDeck key shuffleEncounterDeck
      pure e
    _ -> MariaHill <$> runMessage msg a

newtype LifeModelDecoy = LifeModelDecoy (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

lifeModelDecoy :: AttachmentCard LifeModelDecoy
lifeModelDecoy = attachment LifeModelDecoy C.cw56084

instance HasModifiersFor LifeModelDecoy where
  getModifiersFor _ target (LifeModelDecoy a)
    | Just target == (enemyRef <$> attachmentEnemy a) = pure [KeywordModifier Stalwart]
  getModifiersFor _ _ _ = pure []

instance HasAbilities LifeModelDecoy where
  getAbilities (LifeModelDecoy a) =
    [windowAbility a 1 (W.CharacterWouldTakeAttackDamage $ CharacterWithId $ toCharacter enemy) ForcedInterrupt NoCost (runAbility a 1)
    | Just enemy <- [attachmentEnemy a]]

instance RunMessage LifeModelDecoy where
  runMessage msg e@(LifeModelDecoy a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      minions <- map EnemyMinionId <$> asYou you (selectList AnyMinion)
      leader <- enemyLeaderFor you
      choices <- filterM canAttachDecoy (minions <> [EnemyVillainId vid | Just vid <- [leader]])
      if null choices then removeFromPlay a else chooseForEnemyTeam you a [targeting (enemyRef enemy) (attachTo aid enemy) | enemy <- choices]
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [damage | W.EnemyWouldBeDamaged enemy _ damage <- windows, Just enemy == attachmentEnemy a] $ \damage ->
        for_ (attachmentEnemy a) $ \enemy -> preventEnemyDamage enemy (damageAmount damage)
      removeFromPlay a
      pure e
    _ -> LifeModelDecoy <$> runMessage msg a

newtype ExecutiveOrder = ExecutiveOrder (Attrs Treachery `With` Bool)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

instance IsTreachery ExecutiveOrder where
  toTreacheryAttrs (ExecutiveOrder (a `With` _)) = a

executiveOrder :: TreacheryCard ExecutiveOrder
executiveOrder = treachery (\a -> ExecutiveOrder (a `With` False)) C.cw56085

instance RunMessage ExecutiveOrder where
  runMessage msg e@(ExecutiveOrder (a `With` gained)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId a -> do
      placeThreatOnEach a 2 AnyScheme
      whenM (selectAny $ MinionIs C.cw56083) $ gainSurge a
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      present <- asYou you (selectAny $ MinionIs C.cw56083)
      pure $ ExecutiveOrder (a `With` present)
    Boost (CheckWindows windows) | gained -> do
      for_ [card | W.Window W.When (W.CountingBoostIcons card) <- windows, ecCardId card == CardId (unTreacheryId $ treacheryId a)] $ \card -> changeBoostIconCount card (+ 3)
      pure e
    _ -> (\a' -> ExecutiveOrder (a' `With` gained)) <$> runMessage msg a

newtype ShieldHelicarrier = ShieldHelicarrier (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

shieldHelicarrier :: SideSchemeCard ShieldHelicarrier
shieldHelicarrier = sideScheme ShieldHelicarrier C.cw56086 (Static 3)

instance RunMessage ShieldHelicarrier where
  runMessage msg (ShieldHelicarrier a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      you <- getDefeatedBy a
      friendly <- selectList $ CharacterControlledBy (IdentityWithId you)
      minions <- selectList $ MinionEngagedWith (IdentityWithId you)
      damageCharacters a (toDamage 1 FromAbility) (friendly <> map MinionCharacter minions)
      ShieldHelicarrier <$> runMessage msg a
    _ -> ShieldHelicarrier <$> runMessage msg a
