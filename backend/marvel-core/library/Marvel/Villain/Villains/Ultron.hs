module Marvel.Villain.Villains.Ultron where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Choice
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source -- Would like to remove
import Marvel.GameValue.Types
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window

newtype Ultron = Ultron (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

ultron :: VillainCard Ultron
ultron = ultron1

ultron1 :: VillainCard Ultron
ultron1 = villain Ultron Cards.ultron1 (Sch 1) (Atk 2) (HP $ PerPlayer 17)

ultron2 :: VillainCard Ultron
ultron2 = villain Ultron Cards.ultron2 (Sch 2) (Atk 2) (HP $ PerPlayer 22)

ultron3 :: VillainCard Ultron
ultron3 = villain Ultron Cards.ultron1 (Sch 2) (Atk 4) (HP $ PerPlayer 27)

instance HasAbilities Ultron where
  getAbilities (Ultron a) = case villainStage a of
    1 -> [windowAbility a 1 (EnemyAttacked After (EnemyWithId $ EnemyVillainId $ villainId a) You) ForcedResponse NoCost (RunAbility (toTarget a) 1)]
    2 -> []
    3 -> []
    _ -> error "Invalid stage"

instance RunMessage Ultron where
  runMessage msg e@(Ultron attrs) = case msg of
    VillainMessage ident msg' | ident == villainId attrs -> case msg' of
      VillainDefeated -> do
        difficulty <- getDifficulty
        case (difficulty, villainStage attrs) of
          (_, 1) -> do
            push (VillainMessage ident VillainAdvanced)
            pure $ advanceVillainTo ultron2 attrs
          (Normal, 2) -> Ultron <$> runMessage msg attrs
          (Expert, 2) -> do
            push (VillainMessage ident VillainAdvanced)
            pure $ advanceVillainTo ultron3 attrs
          (_, 3) -> Ultron <$> runMessage msg attrs
          (_, _) -> error "Invalid ultron progression"
      _ -> Ultron <$> runMessage msg attrs
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      case villainStage attrs of
        1 -> do
          chooseOne
            ident
            [ Label "Place 1 threat on the main scheme" [PlaceThreat (toRef attrs) 1 MainScheme]
            , Label "Put the top card of your deck into play facedown, engaged with you as a [[Drone]] minion" []
            ]
        _ -> error "Invalid choice"
      pure e
    _ -> Ultron <$> runMessage msg attrs
