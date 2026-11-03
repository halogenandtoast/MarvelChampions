module Marvel.CivilWar.Paladin where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Minion.Runner
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

newtype Paladin = Paladin (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

paladin :: MinionCard Paladin
paladin = minion Paladin C.cw56117 (Sch 2) (Atk 2) (HP 5)

instance RunMessage Paladin where
  runMessage msg e@(Paladin a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      unregistered <- asYou you $ identityMatches (IdentityWithTrait T.Unregistered) you
      discardFor you a (if unregistered then RandomFromHand else FromHand) 1 1
      pure e
    _ -> Paladin <$> runMessage msg a

newtype PaladinsPistol = PaladinsPistol (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

paladinsPistol :: AttachmentCard PaladinsPistol
paladinsPistol = attachment PaladinsPistol C.cw56118

instance HasModifiersFor PaladinsPistol where
  getModifiersFor _ target (PaladinsPistol a)
    | Just target == (enemyRef <$> attachmentEnemy a) = pure [AttackModifier 2]
    | Just target == (AllyRef <$> attachmentAlly a) = pure [AttackModifier 2]
  getModifiersFor _ _ _ = pure []

instance HasAbilities PaladinsPistol where
  getAbilities (PaladinsPistol a) =
    [windowAbility a 1 (W.EnemyAttackedAnyTarget W.When $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 1)
    | Just enemy <- [attachmentEnemy a]]
    <> [windowAbility a 2 (W.AllyAttacked W.When (AllyWithId aid) AnyEnemy) ForcedInterrupt NoCost (runAbility a 2)
       | Just aid <- [attachmentAlly a]]

instance RunMessage PaladinsPistol where
  runMessage msg e@(PaladinsPistol a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      paladins <- asYou you $ selectList $ MinionIs C.cw56117
      controlled <- asYou you $ selectList $ AllyIs C.cw56117
      minions <- filterM (canAttach . toTarget) paladins
      allies <- filterM (canAttach . toTarget) controlled
      choices <- if not (null minions && null allies)
        then pure $ [targeting (MinionRef mid) $ attachTo aid mid | mid <- minions]
          <> [targeting (AllyRef ally) $ attachToAlly aid ally | ally <- allies]
        else do
          leader <- enemyLeaderFor you
          eligible <- filterM (canAttach . toTarget) [vid | Just vid <- [leader]]
          pure [targeting (VillainRef vid) $ attachTo aid vid | vid <- eligible]
      if null choices then removeFromPlay a else chooseOrRunOne you choices
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      for_ (attachmentEnemy a) $ \enemy -> do
        attackGains enemy K.Ranged
        attackGains enemy K.Overkill
        afterMatching (\case
          VillainMessage vid VillainEndAttack -> enemy == EnemyVillainId vid
          MinionMessage mid MinionEndAttack -> enemy == EnemyMinionId mid
          _ -> False) (removeFromPlay a)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      for_ (attachmentAlly a) $ \aid -> do
        modifyAllyBasicAttack aid (\damage -> damage {damageOverkill = True,damageRanged = True})
        -- The landing step queues damage, retaliation and attack windows before
        -- anything already following it, so discard after that whole resolution.
        afterMatching (\case
          AllyMessage who (AllyAttackLands _ _) -> who == aid
          AllyMessage who (AllyEvenAttackLands _ _) -> who == aid
          _ -> False) (removeFromPlay a)
      pure e
    _ -> PaladinsPistol <$> runMessage msg a
   where
    canAttach target = do
      modifiers <- getModifiers target
      pure $ all (`notElem` modifiers) [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]
