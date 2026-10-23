-- | Risky Business's villain: each stage is double-sided, Norman Osborn (A) and Green
-- Goblin (B), turned by the Criminal Enterprise environment as its counters run out.
module Marvel.Villain.Villains.NormanOsborn where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Id (EnemyId (..))
import Marvel.Question (runAbility)
import Marvel.Cost
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Environment.Environments.CriminalEnterprise (placeInfamy)
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Stats
import Marvel.Trait (Trait (Goblin))
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window qualified as W

newtype NormanOsborn = NormanOsborn (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- Norman Osborn has no ATK and Green Goblin no SCH: each is 0, as the FAQ (#1B) has it.
normanOsborn1, greenGoblin1, normanOsborn2, greenGoblin2, normanOsborn3, greenGoblin3 :: VillainCard NormanOsborn
normanOsborn1 = villain NormanOsborn Cards.normanOsborn1 (Sch 2) (Atk 0) (HP $ PerPlayer 14)
greenGoblin1 = villain NormanOsborn Cards.greenGoblin1 (Sch 0) (Atk 3) (HP $ PerPlayer 14)
normanOsborn2 = villainWith NormanOsborn Cards.normanOsborn2 (Sch 2) (Atk 0) (HP $ PerPlayer 18) (stageL .~ 2)
greenGoblin2 = villainWith NormanOsborn Cards.greenGoblin2 (Sch 0) (Atk 4) (HP $ PerPlayer 18) (stageL .~ 2)
normanOsborn3 = villainWith NormanOsborn Cards.normanOsborn3 (Sch 3) (Atk 0) (HP $ PerPlayer 22) (stageL .~ 3)
greenGoblin3 = villainWith NormanOsborn Cards.greenGoblin3 (Sch 0) (Atk 4) (HP $ PerPlayer 22) (stageL .~ 3)

-- | The stage's face: Green Goblin's or Norman Osborn's.
face :: Bool -> Natural -> VillainCard NormanOsborn
face goblin = \case
  1 -> if goblin then greenGoblin1 else normanOsborn1
  2 -> if goblin then greenGoblin2 else normanOsborn2
  _ -> if goblin then greenGoblin3 else normanOsborn3

isGoblin :: Attrs Villain -> Bool
isGoblin a = Goblin `member` cdTraits (getCardDef a)

instance HasAbilities NormanOsborn where
  getAbilities (NormanOsborn a)
    | isGoblin a =
        [ windowAbility a 3 (W.VillainRevealed W.When (VillainWithId $ villainId a) W.RevealedFromVillain) WhenRevealed NoCost $
            runAbility a 3
        ]
    | otherwise =
        [ windowAbility a 1 (W.EnemyWouldAttack self AnyIdentity) ForcedInterrupt NoCost $ runAbility a 1
        , windowAbility a 2 (W.VillainDamaged W.When $ VillainWithId $ villainId a) ForcedInterrupt NoCost $ runAbility a 2
        ]
   where
    self = EnemyWithId $ EnemyVillainId $ villainId a

-- | Norman Osborn takes no damage: as much infamy is removed from Criminal Enterprise
-- (his "Forced Interrupt"), unless a tough status card prevents it first.
replacesDamage :: Attrs Villain -> Damage -> Bool
replacesDamage a damage = not (isGoblin a) && not (villainTough a && preventedByTough damage)

removeInfamy :: (ReverseQueue m) => Natural -> m ()
removeInfamy n = traverse_ (`removeCounters` n) =<< selectOne (EnvironmentIs Environments.criminalEnterprise)

instance RunMessage NormanOsborn where
  runMessage msg e@(NormanOsborn attrs) = runQueueT $ case msg of
    VillainMessage ident msg' | ident == villainId attrs -> case msg' of
      -- the next stage enters on the side the defeated one was
      VillainDefeated -> do
        difficulty <- getDifficulty
        let stage = villainStage attrs
        if stage >= 3 || (difficulty == Normal && stage >= 2)
          then NormanOsborn <$> runMessage msg attrs
          else do
            advanceVillain ident
            pure $ advanceVillainTo (face (isGoblin attrs) (stage + 1)) attrs
      FlipVillain -> pure $ flipVillainTo (face (not $ isGoblin attrs) (villainStage attrs)) attrs
      -- damage that comes without the window: overkill, or damage dealt to him as a character
      VillainDamaged _ damage | replacesDamage attrs damage -> do
        removeInfamy (damageAmount damage)
        pure e
      -- Green Goblin: "When Green Goblin would scheme, remove madness counters from State
      -- of Madness instead"; a confusion takes the activation first
      VillainSchemes | isGoblin attrs && not (villainConfused attrs) -> do
        madness <- selectOne (EnvironmentIs Environments.stateOfMadness)
        for_ madness \m -> removeCounters m (if villainStage attrs == 3 then 2 else 1)
        pure e
      _ -> NormanOsborn <$> runMessage msg attrs
    -- "When Norman Osborn would attack, place infamy counters on Criminal Enterprise instead"
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      flip insteadOf (void $ placeInfamy (villainStage attrs)) \case
        VillainMessage vid (VillainBeginAttack _) -> vid == villainId attrs
        _ -> False
      pure e
    RanAbility _ (isTarget attrs -> True) 2 windows _ -> do
      for_ [d | W.DamagedVillain _ d <- windows, replacesDamage attrs d] \damage ->
        flip insteadOf (removeInfamy (damageAmount damage)) \case
          VillainMessage vid (VillainDamaged _ _) -> vid == villainId attrs
          _ -> False
      pure e
    -- Green Goblin's "When Revealed", as he is flipped to or advanced to
    RanAbility _ (isTarget attrs -> True) 3 _ _ -> do
      case villainStage attrs of
        1 -> do
          heroes <- selectList HeroIdentity
          for_ heroes \p -> dealIndirectDamage p attrs 3
        2 -> do
          players <- getPlayers
          for_ players \p -> dealIndirectDamage p attrs 3
        _ -> do
          players <- getPlayers
          for_ players \p -> damageIdentity p attrs (toDamage 4 FromAbility)
      pure e
    _ -> NormanOsborn <$> runMessage msg attrs
