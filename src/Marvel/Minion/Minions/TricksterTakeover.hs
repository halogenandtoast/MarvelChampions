module Marvel.Minion.Minions.TricksterTakeover where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.TricksterTakeover.Cards qualified as Cards
import Marvel.TricksterTakeover.Helpers
import Marvel.Window qualified as W
import Marvel.Minion.Types
import Marvel.Minion.Runner ()

newtype TTMinion = TTMinion (Attrs Minion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsMinion

tt55017 :: MinionCard TTMinion
tt55017 = minion TTMinion Cards.tt55017 (Sch 2) (Atk 1) (HP 3)

tt55018 :: MinionCard TTMinion
tt55018 = minion TTMinion Cards.tt55018 (Sch 1) (Atk 2) (HP 4)

tt55019 :: MinionCard TTMinion
tt55019 = minion TTMinion Cards.tt55019 (Sch 1) (Atk 4) (HP 6)

tt55020 :: MinionCard TTMinion
tt55020 = minion TTMinion Cards.tt55020 (Sch 0) (Atk 3) (HP 5)

tt55037 :: MinionCard TTMinion
tt55037 = minion TTMinion Cards.tt55037 (Sch 0) (Atk 1) (HP 2)

tt55038 :: MinionCard TTMinion
tt55038 = minion TTMinion Cards.tt55038 (Sch 1) (Atk 1) (HP 6)

tt55039 :: MinionCard TTMinion
tt55039 = minion TTMinion Cards.tt55039 (Sch 2) (Atk 2) (HP 5)

tt55040 :: MinionCard TTMinion
tt55040 = minion TTMinion Cards.tt55040 (Sch 2) (Atk 2) (HP 4)

tt55041 :: MinionCard TTMinion
tt55041 = minion TTMinion Cards.tt55041 (Sch 3) (Atk 4) (HP 10)

tt55042 :: MinionCard TTMinion
tt55042 = minion TTMinion Cards.tt55042 (Sch 1) (Atk 2) (HP 7)

tt55043 :: MinionCard TTMinion
tt55043 = minion TTMinion Cards.tt55043 (Sch 2) (Atk 2) (HP 7)

tt55044 :: MinionCard TTMinion
tt55044 = minion TTMinion Cards.tt55044 (Sch 2) (Atk 3) (HP 7)

tt55056 :: MinionCard TTMinion
tt55056 = minion TTMinion Cards.tt55056 (Sch 2) (Atk 1) (HP 7)

tt55057 :: MinionCard TTMinion
tt55057 = minion TTMinion Cards.tt55057 (Sch 1) (Atk 3) (HP 8)

tt55058 :: MinionCard TTMinion
tt55058 = minion TTMinion Cards.tt55058 (Sch 2) (Atk 1) (HP 6)

tt55059 :: MinionCard TTMinion
tt55059 = minion TTMinion Cards.tt55059 (Sch 1) (Atk 2) (HP 6)

instance HasModifiersFor TTMinion where
  getModifiersFor _ target (TTMinion a)
    | target == toRef a = do
        enslaved <- enthralled (minionEngagedIdentity a)
        hard <- expert
        costs <- highestCostCardsControlled (minionEngagedIdentity a)
        amounts <- traverse (\t -> getsGame $ maybe 0 (fromMaybe 0 . cdCost . getCardDef) . (`cardInPlay` t)) costs
        pure $ case toCardCode a of
          "55018" -> [HitPointModifier 3 | enslaved]
          "55020" -> [KeywordModifier Toughness | hard]
          "55056" -> [AttackModifier (fromIntegral $ max 0 $ maximum $ 0 : amounts)]
          "55057" -> [KeywordModifier Overkill]
          _ -> []
  getModifiersFor _ _ _ = pure []

instance HasAbilities TTMinion where
  getAbilities (TTMinion a) =
    [windowAbility a 1 (W.MinionDefeated W.When $ MinionWithId $ minionId a) WhenRevealed NoCost (runAbility a 1) | toCardCode a `notElem` ["55017", "55018", "55019"]]
    <> [windowAbility a 2 (W.EnemyAttacked W.After (EnemyWithId $ toEnemyId a) AnyIdentity) ForcedResponse NoCost (runAbility a 2) | toCardCode a `elem` ["55017", "55039", "55058"]]
    <> [windowAbility a 3 (W.EnemyWouldAttack (EnemyWithId $ toEnemyId a) AnyIdentity) ForcedInterrupt NoCost (runAbility a 3) | toCardCode a `elem` ["55019", "55038"]]
    <> [windowAbility a 4 (W.VillainDamaged W.After AnyVillain) ForcedResponse NoCost (runAbility a 4) | toCardCode a == "55020"]
    <> [windowAbility a 5 (W.EnemyWasAttacked W.After (EnemyWithId $ toEnemyId a)) ForcedResponse NoCost (runAbility a 5) | toCardCode a == "55044"]

instance RunMessage TTMinion where
  runMessage msg e@(TTMinion a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      case toCardCode a of
        "55019" -> do
          enslaved <- enthralled you
          if enslaved then enemyBecomesTough mid else minionActivatesAgainst mid you
        "55020" -> whenM expert $ enemyBecomesTough mid
        "55040" -> do
          cards <- getEncounterDiscard
          when (length (filter isTreachery cards) >= 5) $ minionActivatesAgainst mid you
        "55043" -> discardTopOfEncounterDeckThen 3 $ \cards -> when (any isTreachery cards) $ dealIndirectDamage you a 2
        "55059" -> discardFromDeck you 4 a
        _ -> pure ()
      TTMinion <$> runMessage msg a
    WithDiscarded (isTarget a -> True) _ cards | toCardCode a == "55059" -> do
      let n = fromIntegral $ length [() | c <- cards, Energy <- printedResources $ getCardDef c]
      dealIndirectDamage (minionEngagedIdentity a) a n
      pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      who <- maybe (pure you) (fmap (fromMaybe you) . getSourceController) $ headMay [s | W.DefeatedMinion _ _ s _ <- windows]
      case toCardCode a of
        "55020" -> selectOne ActiveVillain >>= traverse_ (\v -> enemyActivationBonus v 1 >> enemyAttacks v who)
        "55037" -> discardTopOfEncounterDeckThen 1 $ \cards -> when (any isTreachery cards) $
          afterMatching (\case MinionMessage other MinionDefeated -> other == minionId a; _ -> False) $
            dealFoundEncounterCard who (MkEncounterCard (CardId $ unMinionId $ minionId a) $ getCardDef a)
        "55038" -> shatter 3
        "55039" -> shatter 2
        "55040" -> shatter 2
        "55041" -> shatter 3 >> chooseSynergy who 1
        "55042" -> shatter 3 >> synergy "55054" 1
        "55043" -> shatter 3 >> synergy "55052" 1
        "55044" -> shatter 3 >> synergy "55055" 1
        "55056" -> recruitLinkedAlly who "55063"
        "55057" -> recruitLinkedAlly who "55064"
        "55058" -> recruitLinkedAlly who "55065"
        "55059" -> recruitLinkedAlly who "55066"
        _ -> pure ()
      pure e
    RanAbility you (isTarget a -> True) 2 windows _ -> do
      let who = fromMaybe you $ headMay [p | W.EnemyAttack _ p <- windows]
      case toCardCode a of
        "55017" -> charm who
        "55039" -> discardTopOfEncounterDeckThen 1 $ traverse_ (\c -> when (isTreachery c) $ dealFoundEncounterCard who c)
        "55058" -> selectList AnySideScheme >>= dealIndirectDamage who a . fromIntegral . length
        _ -> pure ()
      pure e
    RanAbility _ (isTarget a -> True) 3 _ _ -> do
      if toCardCode a == "55019" then makeAttackIndirect (minionId a)
        else discardTopOfEncounterDeckThen 3 $ \cards -> minionStep (minionId a) (MinionAttackChanged $ length $ filter isTreachery cards)
      pure e
    RanAbility _ (isTarget a -> True) 4 windows _ -> do
      for_ [s | W.VillainDamageFrom _ s _ <- windows] $ \s ->
        getSourceController s >>= traverse_ (enemyAttacks (minionId a))
      pure e
    RanAbility _ (isTarget a -> True) 5 windows _ -> do
      for_ [source | W.PlayerAttackResolved source _ _ <- windows] $ \source ->
        discardTopOfEncounterDeckThen 1 $ \cards -> when (any isTreachery cards) $ case source of
          AllyRef aid -> giveStatus aid StunnedStatus
          _ -> getSourceController source >>= traverse_ stunIdentity
      for_ [enemy | W.EnemyAttackOnEnemy enemy _ <- windows] $ \enemy ->
        discardTopOfEncounterDeckThen 1 $ \cards -> when (any isTreachery cards) $ giveStatus enemy StunnedStatus
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "55017" -> chooseOne you [labeled "$label.ttCharm" (charm you), labeled "$label.confuseYourIdentity" (confuseIdentity you)]
        "55018" -> chooseOne you [labeled "$label.ttCharm" (charm you), labeled "$label.exhaustYourIdentity" (exhaust you)]
        _ -> pure ()
      pure e
    _ -> TTMinion <$> runMessage msg a
