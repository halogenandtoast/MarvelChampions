module Marvel.Villain.Villains.Rhino where

import Marvel.Prelude

import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Message
import Marvel.Queue
import Marvel.Stats
import Marvel.Villain.Attrs
import Marvel.Villain.Cards qualified as Cards

newtype Rhino = Rhino VillainAttrs
  deriving anyclass IsVillain
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rhino :: VillainCard Rhino
rhino = rhino1

rhino1 :: VillainCard Rhino
rhino1 = villain Rhino Cards.rhino1 (Sch 1) (Atk 2) (HP $ PerPlayer 14)

rhino2 :: VillainCard Rhino
rhino2 = villainWith
  Rhino
  Cards.rhino2
  (Sch 1)
  (Atk 3)
  (HP $ PerPlayer 15)
  (stageL .~ 2)

rhino3 :: VillainCard Rhino
rhino3 = villainWith
  Rhino
  Cards.rhino3
  (Sch 1)
  (Atk 4)
  (HP $ PerPlayer 16)
  (stageL .~ 3)

instance RunMessage Rhino where
  runMessage msg (Rhino attrs) = case msg of
    VillainMessage villainId msg' | villainId == toId attrs -> case msg' of
      VillainDefeated -> do
        difficulty <- getDifficulty
        case (difficulty, villainStage attrs) of
          (_, 1) -> do
            push (VillainMessage villainId VillainAdvanced)
            pure $ advanceVillainTo rhino2 attrs
          (Normal, 2) -> Rhino <$> runMessage msg attrs
          (Expert, 2) -> do
            push (VillainMessage villainId VillainAdvanced)
            pure $ advanceVillainTo rhino3 attrs
          (_, 3) -> Rhino <$> runMessage msg attrs
          (_, _) -> error "Invalid rhino progression"
      _ -> Rhino <$> runMessage msg attrs
    _ -> Rhino <$> runMessage msg attrs
