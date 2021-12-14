module Marvel.Villain.Villains.Klaw where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Stats
import Marvel.Target
import Marvel.Villain.Attrs
import Marvel.Villain.Cards qualified as Cards
import Marvel.Window

newtype Klaw = Klaw VillainAttrs
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

klaw :: VillainCard Klaw
klaw = klaw1

klaw1 :: VillainCard Klaw
klaw1 = villain Klaw Cards.klaw (Sch 2) (Atk 0) (HP $ PerPlayer 12)

klaw2 :: VillainCard Klaw
klaw2 =
  villainWith Klaw Cards.klaw (Sch 2) (Atk 1) (HP $ PerPlayer 18) (stageL .~ 2)

klaw3 :: VillainCard Klaw
klaw3 =
  villainWith
    Klaw
    Cards.klaw
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
          (EnemyWouldAttack (EnemyIs Cards.klaw) AnyIdentity)
          ForcedInterrupt
          NoCost
          $ RunAbility (toTarget a) 1
      ]
    2 ->
      [ windowAbility
          a
          1
          (VillainRevealed When (VillainWithId $ toId a) RevealedFromVillain)
          ForcedResponse
          NoCost
          (RunAbility (toTarget a) 1)
      , windowAbility
          a
          1
          (EnemyWouldAttack (EnemyIs Cards.klaw) AnyIdentity)
          ForcedInterrupt
          NoCost
          $ RunAbility (toTarget a) 2
      ]
    3 ->
      [ windowAbility
          a
          1
          (EnemyWouldAttack (EnemyIs Cards.klaw) AnyIdentity)
          ForcedInterrupt
          NoCost
          $ RunAbility (toTarget a) 1
      ]
    _ -> error "Invalid stage"

instance RunMessage Klaw where
  runMessage msg e@(Klaw attrs) = case msg of
    RanAbility target 1 _ | isTarget attrs target -> case villainStage attrs of
      1 -> e <$ push (DealBoost target)
      2 -> e <$ push (SearchForAndRevealScheme Cards.theImmortalKlaw)
      3 -> e <$ push (DealBoost target)
      _ -> error "Invalid stage"
    RanAbility target 2 _ | isTarget attrs target -> case villainStage attrs of
      2 -> e <$ push (DealBoost target)
      _ -> error "Invalid stage"
    _ -> Klaw <$> runMessage msg attrs
