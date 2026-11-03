module Marvel.CivilWar.MoonKnight where

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
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype MoonKnight = MoonKnight (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

moonKnight :: MinionCard MoonKnight
moonKnight = let (sch,atk,hp) = C.printedMinionStats "57065"
  in minion MoonKnight C.cw57065 (Sch sch) (Atk atk) (HP hp)

instance RunMessage MoonKnight where
  runMessage msg e@(MoonKnight a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      cards <- asYou you getEncounterDiscard
      for_ (headMay $ filter (cardMatch $ CardWithType TreacheryType) cards) $ resolveEncounterWhenRevealed you
      pure e
    _ -> MoonKnight <$> runMessage msg a

newtype CrescentDart = CrescentDart (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

crescentDart :: AttachmentCard CrescentDart
crescentDart = attachment CrescentDart C.cw57066

instance HasModifiersFor CrescentDart

instance HasAbilities CrescentDart where
  getAbilities (CrescentDart a) =
    [windowAbility a 1 (W.EnemyAttackedAnyTarget W.When $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 1)
    | Just enemy <- [attachmentEnemy a]]
    <> [windowAbility a 2 (W.AllyAttacked W.When (AllyWithId aid) AnyEnemy) ForcedInterrupt NoCost (runAbility a 2)
       | Just aid <- [attachmentAlly a]]

instance RunMessage CrescentDart where
  runMessage msg e@(CrescentDart a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      paladins <- asYou you $ selectList $ MinionIs C.cw57065
      controlled <- asYou you $ selectList $ AllyIs C.cw57065
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
        attackGains enemy K.Piercing
        afterMatching (\case
          VillainMessage vid VillainEndAttack -> enemy == EnemyVillainId vid
          MinionMessage mid MinionEndAttack -> enemy == EnemyMinionId mid
          _ -> False) (removeFromPlay a)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      for_ (attachmentAlly a) $ \aid -> do
        modifyAllyBasicAttack aid (\damage -> damage {damagePiercing = True,damageRanged = True})
        -- The landing step queues damage, retaliation and attack windows before
        -- anything already following it, so discard after that whole resolution.
        afterMatching (\case
          AllyMessage who (AllyAttackLands _ _) -> who == aid
          AllyMessage who (AllyEvenAttackLands _ _) -> who == aid
          _ -> False) (removeFromPlay a)
      pure e
    _ -> CrescentDart <$> runMessage msg a
   where
    canAttach target = do
      modifiers <- getModifiers target
      pure $ all (`notElem` modifiers) [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]

newtype FistOfKhonshu = FistOfKhonshu (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

fistOfKhonshu :: TreacheryCard FistOfKhonshu
fistOfKhonshu = treachery FistOfKhonshu C.cw57067

instance RunMessage FistOfKhonshu where
  runMessage msg e@(FistOfKhonshu a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      exhaust you
      damageIdentity you a (toDamage 2 FromAbility)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      exhaust you
      pure e
    _ -> FistOfKhonshu <$> runMessage msg a

newtype KhonshusAvatar = KhonshusAvatar (Attrs SideScheme `With` Maybe IdentityId)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

instance IsSideScheme KhonshusAvatar where
  toSideSchemeAttrs (KhonshusAvatar (a `With` _)) = a

instance HasModifiersFor KhonshusAvatar

khonshusAvatar :: SideSchemeCard KhonshusAvatar
khonshusAvatar = sideScheme (KhonshusAvatar . (`With` Nothing)) C.cw57068 (Static 3)

instance RunMessage KhonshusAvatar where
  runMessage msg e@(KhonshusAvatar (a `With` actor)) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      you <- getDefeatedBy a
      board <- getAreaOfTarget a
      leader <- getActiveVillainIn board
      keys <- getDeckKeys
      let key = fromMaybe EncounterDeckKey $ headMay [VillainDeckKey vid | Just vid <- [leader],VillainDeckKey vid `elem` keys]
      fromDeck key $ discardUntil FromEncounterDeck (CardWithType TreacheryType) a
      updated <- runMessage msg a
      pure $ KhonshusAvatar (updated `With` Just you)
    WithDiscardedMatch (isTarget a -> True) FromEncounterDeck (EncounterCard card) -> do
      discardCard card
      for_ actor $ \you -> resolveEncounterWhenRevealed you card
      pure e
    _ -> KhonshusAvatar . (`With` actor) <$> runMessage msg a
