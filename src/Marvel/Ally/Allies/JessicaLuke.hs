{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.JessicaLuke where
import Marvel.Prelude
import Control.Monad (replicateM_)
import Data.List (find)
import Data.Maybe (maybeToList)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.JessicaJones.Cards qualified as Cards
import Marvel.JessicaJones.Helpers
import Marvel.Identity.Types
import Marvel.Phase
import Marvel.Ally.Types
import Marvel.Ally.Runner ()

newtype JLAlly = JLAlly (Attrs Ally)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAlly JLAlly where
  allyHasThwart (JLAlly a) = toCardCode a /= "62020"

jl61003 :: AllyCard JLAlly
jl61003 = ally JLAlly Cards.jl61003 (Thw 2, 1) (Atk 2, 1) (HP 3)

jl61015 :: AllyCard JLAlly
jl61015 = ally JLAlly Cards.jl61015 (Thw 2, 1) (Atk 3, 1) (HP 3)

jl61016 :: AllyCard JLAlly
jl61016 = ally JLAlly Cards.jl61016 (Thw 2, 1) (Atk 2, 1) (HP 3)

jl61017 :: AllyCard JLAlly
jl61017 = ally JLAlly Cards.jl61017 (Thw 1, 1) (Atk 1, 1) (HP 2)

jl61025 :: AllyCard JLAlly
jl61025 = ally JLAlly Cards.jl61025 (Thw 2, 1) (Atk 2, 1) (HP 3)

jl61038 :: AllyCard JLAlly
jl61038 = ally JLAlly Cards.jl61038 (Thw 1, 1) (Atk 1, 1) (HP 3)

jl62002 :: AllyCard JLAlly
jl62002 = ally JLAlly Cards.jl62002 (Thw 1, 1) (Atk 2, 1) (HP 3)

jl62012 :: AllyCard JLAlly
jl62012 = ally JLAlly Cards.jl62012 (Thw 1, 1) (Atk 2, 1) (HP 3)

jl62013 :: AllyCard JLAlly
jl62013 = ally JLAlly Cards.jl62013 (Thw 1, 1) (Atk 1, 1) (HP 2)

jl62014 :: AllyCard JLAlly
jl62014 = ally JLAlly Cards.jl62014 (Thw 1, 1) (Atk 2, 1) (HP 3)

jl62020 :: AllyCard JLAlly
jl62020 = ally JLAlly Cards.jl62020 (Thw 0, 1) (Atk 2, 1) (HP 5)

jl62021 :: AllyCard JLAlly
jl62021 = ally JLAlly Cards.jl62021 (Thw 2, 1) (Atk 2, 1) (HP 3)

instance HasModifiersFor JLAlly where
  getModifiersFor _ target (JLAlly a)
    | target == toRef a = case toCardCode a of
      "61038" -> do
        n <- heroDefense (allyController a)
        pure [EnemyAttackDamageReduction n]
      "62002" -> do
        n <- min 4 <$> toughCount (allyController a)
        pure [ThwartModifier n]
      "62013" -> pure $ if allyTough a then [AttackModifier 2, ThwartModifier 2] else []
      "62020" -> pure [AttackModifier $ allyCounters a]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities JLAlly where
  getAbilities (JLAlly a) = case toCardCode a of
    "61003" -> [ability a 1 Action OwnsThis (ExhaustCost <> DealDamageThisCost 1) (runAbility a 1)]
    "61015" -> [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]
    "61016" -> [limitedWindowAbility a 1 (W.WindowOneOf [W.HeroThwarted W.After self, W.AllyThwarted W.After (AllyControlledBy self) AnyScheme]) Response OwnsThis NoCost (runAbility a 1)]
    "61017" ->
      [limitedWindowAbility a 1 (W.PlayedFromHand self $ CardIs Cards.jl61017) Response OwnsThis NoCost (runAbility a 1)
      , ability a 2 Action (OwnsThis <> SchemeExists ThwartableScheme) (ExactCountersCost 1) (runAbility a 2)]
    "61025" -> [ability ref 1 Action
      (ExtendedCardExists (InHandOf self $ CardWithId cid) <> SelfMatches (IdentityWithTrait T.Defender)) NoCost (runAbility ref 1)]
    "62012" -> [limitedWindowAbility a 1 (W.AllyDefendsAgainst W.When (AllyWithId $ allyId a) AnyEnemy) Interrupt OwnsThis NoCost (runAbility a 1)]
    "62014" -> [limitedWindowAbility a 1 (W.AllyDefeated W.When (NotAlly $ AllyWithId $ allyId a)) Interrupt OwnsThis (DiscardCost $ toTarget a) (runAbility a 1)]
    "62020" -> [limitedWindowAbility a 1 (W.EnemyAttacked W.After VillainEnemy self) Response OwnsThis NoCost (runAbility a 1)]
    "62021" -> [limitedWindowAbility a 1 W.EntersPlayThis Response (OwnsThis <> CharacterExists (CharacterWithId (IdentityCharacter $ allyController a) <> CharacterCanGainTough)) NoCost (runAbility a 1)]
    _ -> []
   where
    self = IdentityWithId $ allyController a
    cid = CardId $ unAllyId $ allyId a
    ref = CardIdRef cid

instance RunMessage JLAlly where
  runMessage msg e@(JLAlly a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      case toCardCode a of
        "61003" -> giveStatus (allyId a) ToughStatus
        "61015" -> discardFromDeck you 4 a
        "61016" -> do
          let n = min 8 (allyCounters a + 1)
          when (allyCounters a < 8) $ placeCounters a 1
          chooseOne you
            [labeled "$label.spiderWomanRelease" (removeFromPlay a >> chooseDamage you a (toDamage n FromAbility) AnyEnemy), labeled "$label.done" (pure ())]
        "61017" -> do
          hand <- projectMap PlayerIdentityHand unHand you
          placeCounters a (min 4 $ fromIntegral $ length hand)
        "62012" -> do
          atk <- getModifiedAttack a
          for_ [enemy | W.AllyDefendedAgainst _ enemy <- windows] $ \enemy -> preventEnemyAttackDamage enemy atk
        "62014" -> for_ [aid | W.DefeatedAlly aid _ <- windows] $ \aid -> do
          damage <- characterStoredDamage (AllyCharacter aid)
          (base,_,_) <- getAllyPrintedStats aid
          modifiers <- getModifiers (AllyRef aid)
          let hp = base + sum [n | HitPointModifier n <- modifiers]
              amount = if damage + 1 > hp then damage + 1 - hp else 0
          cancelAllyDefeat aid
          heal aid amount
        "62020" -> placeCounters a 1
        "62021" -> becomeTough you
        _ -> pure ()
      pure e
    RanAbility you (CardIdRef cid) 1 _ _ | toCardCode a == "61025", cid == CardId (unAllyId $ allyId a) -> do
      hand <- projectMap PlayerIdentityHand unHand you
      for_ (find ((== cid) . pcCardId) hand) $ \card -> do
        putIntoPlay you card
        atEndOfPhase $ removeFromPlay (AllyRef $ allyId a)
      pure e
    RanAbility you (isTarget a -> True) 2 _ _ | toCardCode a == "61017" -> do
      removeThreat you a 1 ThwartableScheme
      pure e
    WithDiscarded (isTarget a -> True) _ cards | toCardCode a == "61015" -> do
      let n = length [() | card <- onlyPlayerCards cards, (_,R.Energy) <- cdResources (getCardDef card)]
      let you = allyController a
      when (n > 0) $ removeThreat you a 2 ThwartableScheme
      when (n > 1) $ do
        enemies <- selectList EnemyCanBeConfused
        chooseOne you [targeting (enemyRef enemy) (confuse enemy a) | enemy <- enemies]
      pure e
    _ -> JLAlly <$> runMessage msg a
