module Marvel.Villain.Villains.Rhino where

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
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Stats
import Marvel.Target
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window

newtype Rhino = Rhino (Attrs Villain)
  deriving anyclass IsVillain
  deriving newtype (Show, Eq, ToJSON, FromJSON)

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
  ((stageL .~ 3) . (toughL .~ True))

instance HasAbilities Rhino where
  getAbilities (Rhino a) = case villainStage a of
    1 -> []
    2 ->
      [ windowAbility
          a
          1
          (VillainRevealed
            When
            (VillainWithId $ villainId a)
            RevealedFromVillain
          )
          ForcedResponse
          NoCost
          (RunAbility (toTarget a) 1)
      ]
    3 ->
      [ windowAbility
          a
          1
          (VillainRevealed
            When
            (VillainWithId $ villainId a)
            RevealedFromVillain
          )
          ForcedResponse
          NoCost
          (RunAbility (toTarget a) 1)
      ]
    _ -> error "Invalid stage"

instance RunMessage Rhino where
  runMessage msg e@(Rhino attrs) = case msg of
    VillainMessage ident msg' | ident == villainId attrs -> case msg' of
      VillainDefeated -> do
        difficulty <- getDifficulty
        case (difficulty, villainStage attrs) of
          (_, 1) -> do
            push (VillainMessage ident VillainAdvanced)
            pure $ advanceVillainTo rhino2 attrs
          (Normal, 2) -> Rhino <$> runMessage msg attrs
          (Expert, 2) -> do
            push (VillainMessage ident VillainAdvanced)
            pure $ advanceVillainTo rhino3 attrs
          (_, 3) -> Rhino <$> runMessage msg attrs
          (_, _) -> error "Invalid rhino progression"
      _ -> Rhino <$> runMessage msg attrs
    RanAbility target 1 _ _ | isTarget attrs target -> case villainStage attrs of
      2 -> e <$ push (SearchForAndRevealScheme Cards.breakinAndTakin)
      3 -> do
        players <- getPlayers
        pushAll $ map (`IdentityMessage` IdentityStunned) players
        pure e
      _ -> error "Invalid choice"
    _ -> Rhino <$> runMessage msg attrs
