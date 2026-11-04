module Marvel.FearNoEvil.Kingpin where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.EncounterCard (genEncounterCard)
import Marvel.EncounterSet (getNemesisSet)
import Marvel.Entity
import Marvel.Environment.Types
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.Name (title)
import Marvel.Phase
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Villain.Types qualified as V
import Marvel.Window qualified as W

newtype Kingpin = Kingpin (Attrs V.Villain)
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
  deriving anyclass V.IsVillain
kingpinFaces :: [V.VillainCard Kingpin]
kingpinFaces = [V.villain Kingpin def (Sch sch) (Atk atk) (HP $ PerPlayer hp) | (def,sch,atk,hp) <-
  [(Cards.street60159a,2,0,25),(Cards.street60159b,2,2,25),(Cards.street60160a,3,0,28),(Cards.street60160b,3,3,28)]]
behindScenes :: Attrs V.Villain -> Bool
behindScenes a = toCardCode (getCardDef a) `elem` ["60159a","60160a"]
instance HasModifiersFor Kingpin where
  getModifiersFor _ target (Kingpin a) = pure [modifier | target == toRef a,behindScenes a,modifier <- [CannotBeDealtDamage,CannotHaveAttachedUpgrades]]
instance HasAbilities Kingpin where
  getAbilities (Kingpin a) = if behindScenes a
    then [windowAbility a 1 (W.EnemyWouldAttack (EnemyWithId $ V.toEnemyId a) AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)]
    else [windowAbility a 2 (W.EnemyActivates W.When (EnemyWithId $ V.toEnemyId a) AnyIdentity) ForcedInterrupt NoCost (runAbility a 2)]
instance RunMessage Kingpin where
  runMessage msg e@(Kingpin a) = runQueueT $ case msg of
    VillainMessage vid FlipVillain | vid == V.villainId a -> do
      let index = case toCardCode (getCardDef a) of "60159a" -> 1; "60159b" -> 0; "60160a" -> 3; _ -> 2
      pure $ V.flipVillainTo (kingpinFaces !! index) a
    RanAbility _ (isTarget a -> True) 1 _ _ -> replaceVillainAttackWithScheme (V.villainId a) >> pure e
    RanAbility _ (isTarget a -> True) 2 windows _ -> do
      for_ [you | W.EnemyActivation enemy you <- windows,enemy == V.toEnemyId a] $ \you -> do
        engaged <- selectAny $ MinionEngagedWith $ IdentityWithId you
        unless engaged $ do
          dealBoost $ V.villainId a
          when (toCardCode (getCardDef a) == "60160b" && isJust (V.villainAttacking a)) $ attackGains (V.villainId a) Overkill
      pure e
    _ -> Kingpin <$> runMessage msg a

newtype KingpinScheme = KingpinScheme (Attrs MainScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
kingpinSchemes :: [MainSchemeCard KingpinScheme]
kingpinSchemes = [mainScheme KingpinScheme Cards.street60161b (PerPlayer 11) (PerPlayer 3) (PerPlayer 1),mainScheme KingpinScheme Cards.street60162b (PerPlayer 7) (Static 0) (PerPlayer 1)]
instance IsMainScheme KingpinScheme where
  mainSchemeAbilities = getAbilities
  mainSchemeModifiersFor _ (MinionRef _) (KingpinScheme a) = pure [KeywordModifier Guard | toCardCode a == "60162b"]
  mainSchemeModifiersFor _ _ _ = pure []
instance HasAbilities KingpinScheme where
  getAbilities (KingpinScheme a) = [ability a 1 HeroAction NoCriteria NoCost (runAbility a 1) | toCardCode a == "60161b"]
instance RunMessage KingpinScheme where
  runMessage msg e@(KingpinScheme a) = runQueueT $ case msg of
    SetupMainScheme | toCardCode a == "60161b" -> getAllPlayers >>= traverse_ (\you -> cardSpecific GameRef "kingpinNemesis" (you,True)) >> pure e
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId a -> do
      first <- selectJust FirstPlayer
      if toCardCode a == "60161b" then genEncounterCard Cards.street60163a >>= putEncounterCardIntoPlay first else do
        getAllPlayers >>= traverse_ (\you -> cardSpecific GameRef "kingpinNemesis" (you,False))
        whenM ((== Expert) <$> getDifficulty) $ searchEverywhereAndRevealEncounterCard first (CardIs Cards.street60174)
      pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      expert <- (== Expert) <$> getDifficulty
      removeSchemeThreat a (SchemeMainSchemeId $ mainSchemeId a) (if expert then 4 else 5)
      shuffleEncounterDiscardIntoDeck
      discardUntilAndReveal you $ CardWithType MinionType
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId a -> gameOver Lost >> pure e
    _ -> KingpinScheme <$> runMessage msg a

newtype PublicSupport = PublicSupport (Attrs Environment `With` Natural)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass HasModifiersFor
instance IsEnvironment PublicSupport where toEnvironmentAttrs (PublicSupport (a `With` _)) = a
publicSupportFaces :: [EnvironmentCard PublicSupport]
publicSupportFaces = map (environment $ \a -> PublicSupport $ a `With` 1) [Cards.street60163a,Cards.street60163b]
instance HasAbilities PublicSupport where
  getAbilities (PublicSupport (a `With` players)) =
    [windowAbility a 1 (W.MinionDefeated W.After AnyMinion) ForcedResponse NoCost (runAbility a 1)]
    <> [windowAbility a 2 (W.PhaseBegins W.When VillainPhase) ForcedInterrupt NoCost (runAbility a 2) | toCardCode a == "60163a",environmentCounters a >= 2*players+2]
    <> [windowAbility a 3 (W.EnemyWouldReceiveBoost AnyEnemy) Interrupt (ExactCountersCost players) (runAbility a 3) | toCardCode a == "60163b",environmentCounters a >= players]
instance RunMessage PublicSupport where
  runMessage msg e@(PublicSupport (a `With` players)) = runQueueT $ case msg of
    EnvironmentMessage eid EnvironmentEntered | eid == environmentId a -> do
      count <- fromIntegral <$> fromGameValue (PerPlayer 1)
      pure $ PublicSupport (a `With` count)
    RanAbility _ (isTarget a -> True) 1 _ _ -> placeCounters a 1 >> pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      villains <- selectList $ VillainWithTitle "Kingpin"
      traverse_ flipVillain villains
      selectList EncounterMainScheme >>= traverse_ (\case SchemeMainSchemeId mid -> nextMainScheme mid; _ -> pure ())
      pure $ PublicSupport (a {environmentCardDef=Cards.street60163b} `With` players)
    RanAbility _ (isTarget a -> True) 3 windows _ -> traverse_ preventBoostAssignment [enemy | W.EnemyReceivingBoost enemy <- windows] >> pure e
    _ -> (\attrs -> PublicSupport $ attrs `With` players) <$> runMessage msg a

-- Find follows the physical nemesis card across ordinary encounter zones. If it
-- cannot enter, an underling from this encounter set fills its place instead.
revealKingpinNemesis :: ReverseQueue m => IdentityId -> Bool -> m ()
revealKingpinNemesis you initial = do
  identityCode <- getIdentityCode you
  cards <- filter (MinionCards.isNemesisMinion . getCardDef) <$> findEncounterCards (CardInEncounterSet $ getNemesisSet identityCode)
  case headMay cards of
    Nothing -> findUnderling you
    Just card -> do
      characters <- selectList AnyCharacter
      defs <- traverse (getTargetCardDef . characterRef) characters
      let duplicate = any (maybe False ((== title (cdName $ getCardDef card)) . title . cdName)) defs
      if initial && duplicate then removeFromGame card >> findUnderling you else do
        inPlay <- selectAny $ MinionWithId $ MinionId $ unCardId $ ecCardId card
        if inPlay then revealFoundEncounterCard you card >> findUnderling you else do
          revealFoundEncounterCard you card
          cardSpecific GameRef "kingpinNemesisEntered" (you,ecCardId card)
findUnderling :: ReverseQueue m => IdentityId -> m ()
findUnderling you = do
  cards <- findEncounterCards $ CardWithType MinionType <> CardWithTrait T.Underling
  available <- filterM (fmap not . selectAny . MinionWithId . MinionId . unCardId . ecCardId) cards
  unless (null available) $ chooseOne you [cardLabeled (toCardCode card) (revealFoundEncounterCard you card) | card <- available]

newtype KingpinSideScheme = KingpinSideScheme (Attrs SideScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
instance IsSideScheme KingpinSideScheme where sideSchemeAbilities = getAbilities
kingpinSideSchemes :: [SideSchemeCard KingpinSideScheme]
kingpinSideSchemes = [sideScheme KingpinSideScheme Cards.street60173 (PerPlayer 2),sideScheme KingpinSideScheme Cards.street60174 (PerPlayer 3)]
instance HasAbilities KingpinSideScheme where
  getAbilities (KingpinSideScheme a) = [windowAbility a 1 (W.SideSchemeDefeated W.When $ SideSchemeWithId $ sideSchemeId a) WhenRevealed NoCost (runAbility a 1) | toCardCode a == "60173"]
instance HasModifiersFor KingpinSideScheme where
  getModifiersFor _ (MinionRef _) (KingpinSideScheme a) = pure [HitPointModifier 2 | toCardCode a == "60174"]
  getModifiersFor _ _ _ = pure []
instance RunMessage KingpinSideScheme where
  runMessage msg e@(KingpinSideScheme a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      you <- getDefeatedBy a
      identityCode <- getIdentityCode you
      let matcher = CardInEncounterSet (getNemesisSet identityCode) <> CardWithType SideSchemeType
      cards <- findEncounterCards matcher
      schemes <- selectList $ SideSchemeWithCard matcher
      if null schemes then for_ (headMay cards) $ revealFoundEncounterCard you else traverse_ (\sid -> placeThreatOn a sid 3) schemes
      pure e
    _ -> KingpinSideScheme <$> runMessage msg a
