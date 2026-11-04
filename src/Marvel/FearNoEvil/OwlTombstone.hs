module Marvel.FearNoEvil.OwlTombstone where

import Data.List (nub)
import Marvel.Prelude
import Marvel.Attack
import Marvel.Ability
import Marvel.Attachment.MinionEligibility
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Phase
import Marvel.Stats
import Marvel.Treachery.Types
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

newtype StreetMinion = StreetMinion (Attrs Minion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass (IsMinion,HasAbilities)
streetMinions :: [MinionCard StreetMinion]
streetMinions = [minion StreetMinion def (Sch sch) (Atk atk) (HP hp) | (def,sch,atk,hp) <-
  [(Cards.street60192,1,2,4),(Cards.street60193,3,2,5),(Cards.street60197,1,1,5),(Cards.street60198,2,2,8)]]
instance HasModifiersFor StreetMinion where
  getModifiersFor _ target (StreetMinion a) = pure [KeywordModifier keyword | target == toRef a, keyword <- case toCardCode a of "60192" -> [Piercing,Ranged]; "60193" -> [Piercing]; _ -> []]
instance RunMessage StreetMinion where
  runMessage msg e@(StreetMinion a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      case toCardCode a of
        "60192" -> do
          identities <- selectList (IdentityWithFewestRemainingHitPoints AnyIdentity)
          chooseOne you [targeting p (redirectMinionEngagement mid p) | p <- identities]
        "60197" -> do
          cards <- (<>) <$> getEncounterDeck <*> getEncounterDiscard
          let eligible = filter (\c -> cdCardType (getCardDef c) == AttachmentType && minionAttachment (getCardDef c)) cards
          chooseOne you [cardLabeled (toCardCode c) (attachEncounterCard c mid >> shuffleEncounterDeck) | c <- eligible]
        _ -> pure ()
      StreetMinion <$> runMessage msg a
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      case toCardCode a of
        "60193" -> whenM (isJust <$> getEnemyAttack enemy) $ attackGains enemy Piercing
        "60198" -> selectOne ActiveVillain >>= traverse_ (`giveStatus` ToughStatus)
        _ -> pure ()
      pure e
    _ -> StreetMinion <$> runMessage msg a

newtype StreetAttachment = StreetAttachment (Attrs Attachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsAttachment
streetAttachments :: [AttachmentCard StreetAttachment]
streetAttachments = map (attachment StreetAttachment) [Cards.street60191,Cards.street60195,Cards.street60196]
instance HasModifiersFor StreetAttachment where
  getModifiersFor _ target (StreetAttachment a) = do
    let host = (enemyRef <$> attachmentEnemy a) <|> (AllyRef <$> attachmentAlly a) <|> (IdentityRef <$> attachmentIdentity a)
        scheme n = case target of AllyRef _ -> ThwartModifier n; IdentityRef _ -> ThwartModifier n; _ -> SchemeModifier n
    pure $ if Just target == host then case toCardCode a of
      "60191" -> [AttackModifier 1,scheme 1] <> [TraitModifier T.Aerial | isJust $ attachmentEnemy a]
      "60195" -> [scheme 2]
      "60196" -> [AttackModifier 2]
      _ -> []
      else []
instance HasAbilities StreetAttachment where
  getAbilities (StreetAttachment a) = case toCardCode a of
    "60191" -> [ability a 1 HeroAction NoCriteria (DiscardMatchingHandCardCost $ CardWithType EventType <> CardWithTrait T.Attack) (runAbility a 1)]
    "60195" -> [windowAbility a 2 (W.MinionHasSchemed W.After $ MinionWithId mid) ForcedResponse NoCost (runAbility a 2) | Just (EnemyMinionId mid) <- [attachmentEnemy a]]
    "60196" -> [windowAbility a 3 (W.EnemyAttackedAnyTarget W.After $ EnemyWithId enemy) ForcedResponse NoCost (runAbility a 3) | Just enemy <- [attachmentEnemy a]]
    _ -> []
instance RunMessage StreetAttachment where
  runMessage msg e@(StreetAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      enemies <- if toCardCode a == "60191" then selectList (NotEnemy $ EnemyWithTrait T.Aerial) else map EnemyMinionId <$> selectList MinionWithHighestPrintedHitPoints
      if null enemies then removeFromPlay a >> surge you else chooseOne you [targeting (enemyRef enemy) (attachTo aid enemy >> when (toCardCode a /= "60191") (giveStatus enemy ToughStatus)) | enemy <- enemies]
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> removeFromPlay a >> pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> selectJust FirstPlayer >>= confuseIdentity >> removeFromPlay a >> pure e
    RanAbility _ (isTarget a -> True) 3 windows _ -> do
      for_ (attachmentEnemy a) $ \enemy -> do
        attack <- getEnemyAttack enemy
        for_ attack $ \atk -> giveStatus (attackCharacter atk) StunnedStatus
      for_ [target | W.EnemyAttackOnEnemy _ target <- windows] (`giveStatus` StunnedStatus)
      removeFromPlay a
      pure e
    _ -> StreetAttachment <$> runMessage msg a

newtype SwoopDown = SwoopDown (Attrs Treachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsTreachery
swoopDown :: TreacheryCard SwoopDown
swoopDown = treachery SwoopDown Cards.street60194
instance RunMessage SwoopDown where
  runMessage msg e@(SwoopDown a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      villain <- fmap EnemyVillainId <$> selectOne ActiveVillain
      minions <- map EnemyMinionId <$> selectList (MinionEngagedWith $ IdentityWithId you)
      cardSpecific a "swoopEnemies" (you,maybe [] pure villain <> minions)
      pure e
    CardSpecific (isTarget a -> True) "swoopEnemies" (fromJSON -> Success (you,enemies :: [EnemyId])) -> do
      chooseOne you [targeting (enemyRef enemy) (cardSpecific a "swoopOne" (you,enemy) >> cardSpecific a "swoopEnemies" (you,filter (/=enemy) enemies)) | enemy <- enemies]
      pure e
    CardSpecific (isTarget a -> True) "swoopOne" (fromJSON -> Success (you,enemy :: EnemyId)) -> do
      hero <- identityMatches HeroIdentity you
      aerial <- enemyMatches (EnemyWithTrait T.Aerial) enemy
      status <- characterHasCondition (if hero then StunnedStatus else ConfusedStatus) (toCharacter enemy)
      forbidden <- elem CannotActivate <$> getModifiers (enemyRef enemy)
      when (aerial && not status && not forbidden) $ enemyActivationBonus enemy 2
      if hero then enemyAttacks enemy you else case enemy of
        EnemyVillainId vid -> villainSchemesAgainst vid you
        EnemyMinionId mid -> minionSchemesAgainst mid you
        _ -> pure ()
      pure e
    _ -> SwoopDown <$> runMessage msg a

newtype HitList = HitList (Attrs SideScheme `With` Maybe ([AllyId],[AllyId]))
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass HasModifiersFor
instance IsSideScheme HitList where
  toSideSchemeAttrs (HitList (a `With` _)) = a
  sideSchemeAbilities = getAbilities
hitList :: SideSchemeCard HitList
hitList = sideScheme (HitList . (`With` Nothing)) Cards.street60199 (PerPlayer 3)
instance HasAbilities HitList where
  getAbilities (HitList (a `With` _)) = [windowAbility a 1 (W.PhaseBegins W.After VillainPhase) ForcedResponse NoCost (runAbility a 1)]
instance RunMessage HitList where
  runMessage msg (HitList (a `With` checking)) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      you <- selectJust FirstPlayer
      dealIndirectDamage you a 3
      cardSpecific a "hitListAfterDamage" you
      pure $ HitList (a `With` Just ([],[]))
    CheckWindows windows | Just (defeated,damaged) <- checking -> do
      let defeated' = [aid | W.Window W.After (W.DefeatedAlly aid _) <- windows]
          damaged' = [aid | W.Window W.After (W.AllyTookDamage aid source _) <- windows,source == toSource a]
      pure $ HitList (a `With` Just (defeated<>defeated',damaged<>damaged'))
    CardSpecific (isTarget a -> True) "hitListAfterDamage" (fromJSON -> Success you) -> do
      for_ checking $ \(defeated,damaged) -> do
        n <- fromIntegral <$> getPlayerCount
        let count = length $ filter (`elem` defeated) $ nub damaged
        when (count>0) $ removeThreat you a (n * fromIntegral count) (SchemeWithId $ SchemeSideSchemeId $ sideSchemeId a)
      pure $ HitList (a `With` Nothing)
    _ -> (\attrs -> HitList (attrs `With` checking)) <$> runMessage msg a
