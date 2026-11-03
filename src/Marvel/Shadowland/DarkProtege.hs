module Marvel.Shadowland.DarkProtege where

import Marvel.Prelude
import Marvel.Ability
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
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
import Marvel.Resource
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype DarkMinion = DarkMinion (Attrs Minion `With` Maybe ([MinionId],Bool))
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance IsMinion DarkMinion where
  toMinionAttrs (DarkMinion (a `With` _)) = a
  overMinionAttrs f (DarkMinion (a `With` checking)) = DarkMinion (f a `With` checking)
darkMinions :: [MinionCard DarkMinion]
darkMinions = [minion (DarkMinion . (`With` Nothing)) def (Sch sch) (Atk atk) (HP 5) | (def,sch,atk) <-
  [(Cards.street65020,2,2),(Cards.street65021,0,1),(Cards.street65022,1,2),(Cards.street65023,1,2)]]
instance HasModifiersFor DarkMinion where
  getModifiersFor _ target (DarkMinion (a `With` _)) = pure [KeywordModifier Ranged | target == toRef a, toCardCode a == "65022"]
instance HasAbilities DarkMinion where
  getAbilities (DarkMinion (a `With` _)) =
    [windowAbility a 1 (W.EnemyTookDamage (EnemyWithId $ toEnemyId a) AnyAttack) ForcedResponse NoCost (runAbility a 1) | toCardCode a == "65021"]
    <> [windowAbility a 2 (W.EnemyWouldAttack (EnemyWithId $ toEnemyId a) AnyIdentity) ForcedInterrupt NoCost (runAbility a 2) | toCardCode a == "65022"]
instance RunMessage DarkMinion where
  runMessage msg e@(DarkMinion (a `With` checking)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      case toCardCode a of
        "65020" -> do
          others <- selectList (MinionEngagedWith (IdentityWithId you) <> NotMinion (MinionWithId mid))
          traverse_ (\m -> enemyAttacks m you) others
          cardSpecific a "archpriestessAttacksResolved" ()
        "65023" -> dealIndirectDamage you a 2
        _ -> pure ()
      attrs <- runMessage msg a
      others <- selectList (MinionEngagedWith (IdentityWithId you) <> NotMinion (MinionWithId mid))
      pure $ DarkMinion (attrs `With` if toCardCode a == "65020" then Just (others,False) else checking)
    CheckWindows windows | Just (others,done) <- checking -> do
      let madeAttack = any (\case
            W.Window W.After (W.EnemyAttack (EnemyMinionId mid) _) -> mid `elem` others
            W.Window W.After (W.EnemyAttackAlly (EnemyMinionId mid) _ _) -> mid `elem` others
            W.Window W.After (W.EnemyAttackOnEnemy (EnemyMinionId mid) _) -> mid `elem` others
            _ -> False) windows
      pure $ DarkMinion (a `With` Just (others,done || madeAttack))
    CardSpecific (isTarget a -> True) "archpriestessAttacksResolved" _ -> do
      when (maybe True (not . snd) checking) $ discardTopOfEncounterDeck 3
      pure $ DarkMinion (a `With` Nothing)
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [source | W.EnemyTookDamageFrom _ _ source <- windows] $ \source -> case source of
        AllyRef aid -> giveStatus aid StunnedStatus
        MinionRef mid -> giveStatus mid StunnedStatus
        VillainRef vid -> giveStatus vid StunnedStatus
        _ -> getSourceController source >>= traverse_ stunIdentity
      pure e
    RanAbility you (isTarget a -> True) 2 _ _ -> do
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      costs <- traverse (\aid -> (aid,) . fromMaybe 0 . cdCost <$> getsGame (fromMaybe (error "missing ally") . fmap getCardDef . (`cardInPlay` AllyRef aid))) allies
      unless (null costs) $ chooseOne you [targeting aid (redirectEnemyAttackToAlly (toEnemyId a) you aid) | (aid,cost) <- costs,cost == maximum (map snd costs)]
      pure e
    _ -> (\attrs -> DarkMinion (attrs `With` checking)) <$> runMessage msg a

newtype DarkScheme = DarkScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass HasModifiersFor
instance IsSideScheme DarkScheme where
  sideSchemeAbilities = getAbilities
darkSchemes :: [SideSchemeCard DarkScheme]
darkSchemes = [sideScheme (\a -> DarkScheme a {sideSchemeCrisis=True}) Cards.street65024 (Static 1),sideScheme DarkScheme Cards.street65025 (Static 2)]
instance HasAbilities DarkScheme where
  getAbilities (DarkScheme a) = [windowAbility a 1 (W.SideSchemeDefeated W.When $ SideSchemeWithId $ sideSchemeId a) WhenRevealed NoCost (runAbility a 1)]
instance RunMessage DarkScheme where
  runMessage msg e@(DarkScheme a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      let who = fromMaybe you $ headMay [p | W.DefeatedSideSchemeBy _ p _ <- windows]
      if toCardCode a == "65024" then do
        allies <- selectList (AllyControlledBy $ IdentityWithId who)
        if null allies then damageIdentity who a (toDamage 3 FromAbility)
          else chooseOne who [targeting aid (removeFromGame aid) | aid <- allies]
      else do
        cards <- take 3 <$> getEncounterDeck
        chooseOne who [cardLabeled (toCardCode c) (revealFoundEncounterCard who c >> traverse_ discardCard (filter (/=c) cards)) | c <- cards]
      pure e
    _ -> DarkScheme <$> runMessage msg a

newtype DarkTreachery = DarkTreachery (Attrs Treachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsTreachery
darkTreacheries :: [TreacheryCard DarkTreachery]
darkTreacheries = map (treachery DarkTreachery) [Cards.street65026,Cards.street65027]
instance RunMessage DarkTreachery where
  runMessage msg e@(DarkTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      if toCardCode a == "65026" then changeToHero you >> (selectOne ActiveVillain >>= traverse_ (\v -> enemyAttacks v you))
        else do
          scheme <- selectJust EncounterMainScheme
          before <- getSchemeThreatAmount scheme
          selectOne ActiveVillain >>= traverse_ (\v -> villainSchemesAgainst v you)
          cardSpecific a "embracedThreat" (scheme,before)
      pure e
    CardSpecific (isTarget a -> True) "embracedThreat" (fromJSON -> Success (scheme,before :: Natural)) -> do
      after <- getSchemeThreatAmount scheme
      when (after <= before + 2) $ discardTopOfEncounterDeck 3
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      attacking <- isJust <$> getEnemyAttack enemy
      when attacking $ if toCardCode a == "65026" then attackGains enemy Overkill
        else selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn a mid 2)
      pure e
    _ -> DarkTreachery <$> runMessage msg a

data DarkAttachment = DarkAttachment (Attrs Attachment) (Maybe EnemyId)
  deriving stock (Show, Eq)
instance IsAttachment DarkAttachment where
  toAttachmentAttrs (DarkAttachment a _) = a
instance HasCardCode DarkAttachment where
  toCardCode (DarkAttachment a _) = toCardCode a
instance IsRef DarkAttachment where
  toRef (DarkAttachment a _) = toRef a
instance HasModifiersFor DarkAttachment
instance ToJSON DarkAttachment where
  toJSON (DarkAttachment a attack) = case toJSON a of
    Object o -> Object $ KeyMap.insert "darkAttack" (toJSON attack) o
    value -> value
instance FromJSON DarkAttachment where
  parseJSON = withObject "DarkAttachment" $ \o -> DarkAttachment <$> parseJSON (Object o) <*> o .:? "darkAttack"
darkAttachments :: [AttachmentCard DarkAttachment]
darkAttachments = map (attachment (`DarkAttachment` Nothing)) [Cards.street65018,Cards.street65019]
instance HasAbilities DarkAttachment where
  getAbilities (DarkAttachment a tracked) = case attachmentIdentity a of
    Just you -> [windowAbility a 1 (W.EnemyWouldAttack AnyEnemy $ IdentityWithId you) ForcedInterrupt NoCost (runAbility a 1)
      ,ability a 2 AlterEgoAction (SelfMatches $ IdentityWithId you) (MultiResourceCost $ map Just [Energy,Mental,Physical]) (runAbility a 2)
      ] <> [windowAbility a 4 (W.CharacterDefeatedByEnemyAttack W.After $ EnemyWithId enemy) ForcedResponse NoCost (runAbility a 4) | Just enemy <- [tracked]]
      <> [windowAbility a 5 (W.EnemyAttackedAnyTarget W.After $ EnemyWithId enemy) ForcedResponse NoCost (runAbility a 5) | Just enemy <- [tracked]]
    Nothing -> [windowAbility a 3 (W.EnemyWouldTakeDamage $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 3) | enemy <- maybe [] pure $ attachmentEnemy a]
instance RunMessage DarkAttachment where
  runMessage msg e@(DarkAttachment a tracked) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      if toCardCode a == "65018" then attachToIdentity aid you else selectOne ActiveVillain >>= traverse_ (attachTo aid)
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [enemy | W.EnemyAttack enemy _ <- windows] $ \enemy -> changeAttack enemy 1
      pure $ DarkAttachment a $ headMay [enemy | W.EnemyAttack enemy _ <- windows]
    RanAbility _ (isTarget a -> True) 2 _ _ -> removeFromPlay a >> pure e
    RanAbility you (isTarget a -> True) 3 windows _ -> do
      for_ [(vid,source) | W.EnemyWouldBeDamaged (EnemyVillainId vid) source _ <- windows] $ \(vid,source) -> do
        target <- case source of
          AllyRef aid -> pure $ Just $ AllyCharacter aid
          IdentityRef p -> pure $ Just $ IdentityCharacter p
          MinionRef mid -> pure $ Just $ MinionCharacter mid
          VillainRef v -> pure $ Just $ VillainCharacter v
          _ -> fmap IdentityCharacter <$> getSourceController source
        for_ target $ \character -> do
          preventEnemyDamage (EnemyVillainId vid) 3
          villainAttacksCharacter vid you character
          removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) 4 _ _ -> do
      selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn a mid 1)
      pure $ DarkAttachment a Nothing
    RanAbility _ (isTarget a -> True) 5 _ _ -> pure $ DarkAttachment a Nothing
    _ -> (\attrs -> DarkAttachment attrs tracked) <$> runMessage msg a
