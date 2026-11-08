{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.JessicaLuke where
import Marvel.Prelude
import Marvel.Name qualified as Name
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
  allyHasThwart (JLAlly a) = Name.title (cdName (allyCardDef a)) /= "Hulk"

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
    | target == toRef a = case Name.title (cdName (allyCardDef a)) of
      "Echo" -> do
        n <- heroDefense (allyController a)
        pure [EnemyAttackDamageReduction n]
      "Jessica Jones" -> do
        n <- min 4 <$> toughCount (allyController a)
        pure [ThwartModifier n]
      "Misty Knight" -> pure $ if allyTough a then [AttackModifier 2, ThwartModifier 2] else []
      "Hulk" -> pure [AttackModifier $ allyCounters a]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities JLAlly where
  getAbilities (JLAlly a) = case Name.title (cdName (allyCardDef a)) of
    "Luke Cage" -> [ability a 1 Action OwnsThis (ExhaustCost <> DealDamageThisCost 1) (runAbility a 1)]
    "Captain Marvel" -> [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]
    "Spider-Woman" -> [limitedWindowAbility a 1 (W.WindowOneOf [W.HeroThwarted W.After self, W.AllyThwarted W.After (AllyControlledBy self) AnyScheme]) Response OwnsThis NoCost (runAbility a 1)]
    "Squirrel Girl" ->
      [limitedWindowAbility a 1 (W.PlayedFromHand self $ CardIs Cards.jl61017) Response OwnsThis NoCost (runAbility a 1)
      , ability a 2 Action (OwnsThis <> SchemeExists ThwartableScheme) (ExactCountersCost 1) (runAbility a 2)]
    "Hellcat" -> [ability ref 1 Action
      (ExtendedCardExists (InHandOf self $ CardWithId cid) <> SelfMatches (IdentityWithTrait T.Defender)) NoCost (runAbility ref 1)]
    "Iron Fist" -> [limitedWindowAbility a 1 (W.AllyDefendsAgainst W.When (AllyWithId $ allyId a) AnyEnemy) Interrupt OwnsThis NoCost (runAbility a 1)]
    "Valkyrie" -> [limitedWindowAbility a 1 (W.AllyDefeated W.When (NotAlly (AllyWithId $ allyId a) <> AllyCanBeHealed)) Interrupt OwnsThis (DiscardCost $ toTarget a) (runAbility a 1)]
    "Hulk" -> [limitedWindowAbility a 1 (W.EnemyAttacked W.After VillainEnemy self) Response OwnsThis NoCost (runAbility a 1)]
    "She-Hulk" -> [limitedWindowAbility a 1 W.EntersPlayThis Response (OwnsThis <> CharacterExists (CharacterWithId (IdentityCharacter $ allyController a) <> CharacterCanGainTough)) NoCost (runAbility a 1)]
    _ -> []
   where
    self = IdentityWithId $ allyController a
    cid = CardId $ unAllyId $ allyId a
    ref = CardIdRef cid

instance RunMessage JLAlly where
  runMessage msg e@(JLAlly a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      case Name.title (cdName (allyCardDef a)) of
        "Luke Cage" -> giveStatus (allyId a) ToughStatus
        "Captain Marvel" -> discardFromDeck you 4 a
        "Spider-Woman" -> do
          let n = min 8 (allyCounters a + 1)
          when (allyCounters a < 8) $ placeCounters a 1
          chooseOne you
            [labeled "$label.spiderWomanRelease" (removeFromPlay a >> chooseDamage you a (toDamage n FromAbility) AnyEnemy), labeled "$label.done" (pure ())]
        "Squirrel Girl" -> do
          hand <- projectMap PlayerIdentityHand unHand you
          placeCounters a (min 4 $ fromIntegral $ length hand)
        "Iron Fist" -> do
          atk <- getModifiedAttack a
          for_ [enemy | W.AllyDefendedAgainst _ enemy <- windows] $ \enemy -> preventEnemyAttackDamage enemy atk
        "Valkyrie" -> for_ [aid | W.DefeatedAlly aid _ <- windows] $ \aid -> do
          damage <- characterStoredDamage (AllyCharacter aid)
          (base,_,_) <- getAllyPrintedStats aid
          modifiers <- getModifiers (AllyRef aid)
          let hp = base + sum [n | HitPointModifier n <- modifiers]
              amount = if damage + 1 > hp then damage + 1 - hp else 0
          cancelAllyDefeat aid
          heal aid amount
        "Hulk" -> placeCounters a 1
        "She-Hulk" -> becomeTough you
        _ -> pure ()
      pure e
    RanAbility you (CardIdRef cid) 1 _ _ | Name.title (cdName (allyCardDef a)) == "Hellcat", cid == CardId (unAllyId $ allyId a) -> do
      hand <- projectMap PlayerIdentityHand unHand you
      for_ (find ((== cid) . pcCardId) hand) $ \card -> do
        putIntoPlay you card
        atEndOfPhase $ removeFromPlay (AllyRef $ allyId a)
      pure e
    RanAbility you (isTarget a -> True) 2 _ _ | Name.title (cdName (allyCardDef a)) == "Squirrel Girl" -> do
      removeThreat you a 1 ThwartableScheme
      pure e
    WithDiscarded (isTarget a -> True) _ cards | Name.title (cdName (allyCardDef a)) == "Captain Marvel" -> do
      let n = length [() | card <- onlyPlayerCards cards, (_,R.Energy) <- cdResources (getCardDef card)]
      let you = allyController a
      when (n > 0) $ removeThreat you a 2 ThwartableScheme
      when (n > 1) $ do
        enemies <- selectList EnemyCanBeConfused
        chooseOne you [targeting (enemyRef enemy) (confuse enemy a) | enemy <- enemies]
      pure e
    _ -> JLAlly <$> runMessage msg a
