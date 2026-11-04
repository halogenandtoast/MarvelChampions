module Marvel.FearNoEvil.BullseyeAttachments where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Effect.Types qualified as E
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

newtype BullseyeEquipment = BullseyeEquipment (Attrs Attachment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsAttachment
bullseyeEquipment :: [AttachmentCard BullseyeEquipment]
bullseyeEquipment = map (attachment BullseyeEquipment) [Cards.street60069,Cards.street60070,Cards.street60071]
instance HasModifiersFor BullseyeEquipment where
  getModifiersFor _ target (BullseyeEquipment a) = pure $ case attachmentEnemy a of
    Just enemy | target == enemyRef enemy, toCardCode a == "60070" -> [AttackModifier 2,KeywordModifier Piercing,LoseKeyword Ranged]
    _ -> []
instance HasAbilities BullseyeEquipment where
  getAbilities (BullseyeEquipment a) = case toCardCode a of
    "60071" -> [windowAbility a 1 (W.EnemyWouldAttack VillainEnemy $ IdentityControllingAlly $ AllyWithId aid) ForcedInterrupt NoCost (runAbility a 1) | Just aid <- [attachmentAlly a]]
    "60070" -> [windowAbility a 2 (W.HeroDefended W.After HeroIdentity $ EnemyWithId enemy) HeroResponse (DifferentResourcesCost 2) (runAbility a 2) | Just enemy <- [attachmentEnemy a]]
    _ -> [windowAbility a 3 (W.EnemyAttackedAnyTarget W.When $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 3) | Just enemy <- [attachmentEnemy a]]
instance RunMessage BullseyeEquipment where
  runMessage msg e@(BullseyeEquipment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      if toCardCode a == "60071" then do
        pursuit <- selectList $ AttachmentIs Cards.street60071
        hosts <- catMaybes <$> traverse getAttachedCharacter pursuit
        allies <- selectList AnyAlly
        allies' <- filterM (\ally -> if AllyCharacter ally `elem` hosts then pure False else all (`notElem` [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]) <$> getModifiers (AllyRef ally)) allies
        if null allies' then removeFromPlay a >> surge you else chooseOne you [targeting (AllyRef ally) (attachToAlly aid ally) | ally <- allies']
      else villainNamed "Bullseye" >>= maybe (removeFromPlay a) (attachTo aid)
      pure e
    AttachmentMessage aid (AttachedToEnemy enemy) | aid == attachmentId a, toCardCode a == "60070" -> do
      updated <- runMessage msg a
      attackLoses enemy Ranged
      whenM (isJust <$> getEnemyAttack enemy) $ attackGains enemy Piercing
      pure $ BullseyeEquipment updated
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ (attachmentAlly a) $ \ally -> for_ (headMay [(enemy,you) | W.EnemyAttack enemy you <- windows]) $ \(enemy,you) -> do
        redirectEnemyAttackToAlly enemy you ally
        attackGains enemy Overkill
        createEffect Cards.street60071 (AllyRef ally) (EnemyEntity $ EnemyWithId enemy)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> removeFromPlay a >> pure e
    RanAbility _ (isTarget a -> True) 3 _ _ -> do
      for_ (attachmentEnemy a) $ \enemy -> do
        createEffect Cards.street60069 a (EnemyEntity $ EnemyWithId enemy)
        dealBoost (enemyRef enemy)
      pure e
    _ -> BullseyeEquipment <$> runMessage msg a

-- The conditional instructions belong to the triggered attack. The effect
-- survives its attachment leaving play while that attack is being resolved.
newtype BullseyeAttackEffect = BullseyeAttackEffect (Attrs E.Effect `With` Bool)
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
instance E.IsEffect BullseyeAttackEffect where
  toEffectAttrs (BullseyeAttackEffect (a `With` _)) = a
bullseyeAttackEffects :: [E.CardEffect BullseyeAttackEffect]
bullseyeAttackEffects = map (E.effect (\a -> BullseyeAttackEffect (a `With` False))) [Cards.street60069,Cards.street60071]
instance RunMessage BullseyeAttackEffect where
  runMessage msg e@(BullseyeAttackEffect (a `With` defeated)) = runQueueT $ case E.effectMatcher a of
    EnemyEntity (EnemyWithId enemy) -> case msg of
      CheckWindows windows -> do
        let allies = [ally | W.Window W.When (W.DefeatedAlly ally (Just attacker)) <- windows,attacker == enemy]
            relevantAllies = case E.effectSource a of AllyRef attached -> filter (== attached) allies; _ -> allies
            identities = [you | W.Window W.After (W.EnemyAttackDefeatedCharacter attacker (IdentityCharacter you)) <- windows,attacker == enemy]
        traverse_ removeDefeatedAllyFromGame relevantAllies
        pure $ BullseyeAttackEffect (a `With` (defeated || notNull relevantAllies || notNull identities))
      VillainMessage vid VillainEndAttack | enemy == EnemyVillainId vid -> finish a defeated >> pure e
      MinionMessage mid MinionEndAttack | enemy == EnemyMinionId mid -> finish a defeated >> pure e
      _ -> (\attrs -> BullseyeAttackEffect (attrs `With` defeated)) <$> runMessage msg a
    _ -> pure e
   where
    finish attrs killed = do
      when (E.effectCardCode attrs == "60069") $ case E.effectSource attrs of
        AttachmentRef aid -> finishRaiseTheStakes (MkEncounterCard (CardId $ unAttachmentId aid) Cards.street60069) killed
        _ -> pure ()
      disableEffect $ E.effectId attrs
