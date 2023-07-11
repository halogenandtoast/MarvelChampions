module Marvel.Villain.Villains.Klaw where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window

newtype Klaw = Klaw (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

klaw :: VillainCard Klaw
klaw = klaw1

klaw1 :: VillainCard Klaw
klaw1 = villain Klaw Cards.klaw1 (Sch 2) (Atk 0) (HP $ PerPlayer 12)

klaw2 :: VillainCard Klaw
klaw2 =
  villainWith Klaw Cards.klaw2 (Sch 2) (Atk 1) (HP $ PerPlayer 18) (stageL .~ 2)

klaw3 :: VillainCard Klaw
klaw3 =
  villainWith
    Klaw
    Cards.klaw3
    (Sch 3)
    (Atk 2)
    (HP $ PerPlayer 22)
    ((stageL .~ 3) . (toughL .~ True))

instance HasAbilities Klaw where
  getAbilities (Klaw a) = case villainStage a of
    1 ->
      [ windowAbility
          a
          1
          (EnemyWouldAttack (EnemyIs Cards.klaw1) AnyIdentity)
          ForcedInterrupt
          NoCost
          $ RunAbility (toTarget a) 1
      ]
    2 ->
      [ windowAbility
          a
          1
          (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain)
          ForcedResponse
          NoCost
          (RunAbility (toTarget a) 1)
      , windowAbility
          a
          1
          (EnemyWouldAttack (EnemyIs Cards.klaw2) AnyIdentity)
          ForcedInterrupt
          NoCost
          $ RunAbility (toTarget a) 2
      ]
    3 ->
      [ windowAbility
          a
          1
          (EnemyWouldAttack (EnemyIs Cards.klaw3) AnyIdentity)
          ForcedInterrupt
          NoCost
          $ RunAbility (toTarget a) 1
      ]
    _ -> error "Invalid stage"

instance RunMessage Klaw where
  runMessage msg e@(Klaw attrs) = case msg of
    VillainMessage ident msg' | ident == villainId attrs -> case msg' of
      VillainDefeated -> do
        difficulty <- getDifficulty
        case (difficulty, villainStage attrs) of
          (_, 1) -> do
            push (VillainMessage ident VillainAdvanced)
            pure $ advanceVillainTo klaw2 attrs
          (Normal, 2) -> Klaw <$> runMessage msg attrs
          (Expert, 2) -> do
            push (VillainMessage ident VillainAdvanced)
            pure $ advanceVillainTo klaw3 attrs
          (_, 3) -> Klaw <$> runMessage msg attrs
          (_, _) -> error "Invalid rhino progression"
      _ -> Klaw <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      push $ case villainStage attrs of
        1 -> DealBoost (toRef attrs)
        2 -> SearchForAndRevealScheme Cards.theImmortalKlaw
        3 -> DealBoost (toRef attrs)
        _ -> error "Invalid stage"
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      push $ case villainStage attrs of
        2 -> DealBoost (toRef attrs)
        _ -> error "Invalid stage"
      pure e
    _ -> Klaw <$> runMessage msg attrs
