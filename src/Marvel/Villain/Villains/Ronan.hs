module Marvel.Villain.Villains.Ronan where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (Choice (RunAbility))
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window

-- | Toughness on each stage, and Retaliate 1 on III: their cards'.
newtype Ronan = Ronan (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

ronan1 :: VillainCard Ronan
ronan1 = villain Ronan Cards.ronan1 (Sch 2) (Atk 2) (HP $ PerPlayer 14)

ronan2 :: VillainCard Ronan
ronan2 = villainWith Ronan Cards.ronan2 (Sch 2) (Atk 3) (HP $ PerPlayer 18) (stageL .~ 2)

ronan3 :: VillainCard Ronan
ronan3 = villainWith Ronan Cards.ronan3 (Sch 3) (Atk 4) (HP $ PerPlayer 25) (stageL .~ 3)

-- | II: search the encounter deck and discard pile for Cut the Power and reveal it. III:
-- for Superior Tactics.
instance HasAbilities Ronan where
  getAbilities (Ronan a)
    | villainStage a >= 2 =
        [ windowAbility a 1 (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain) WhenRevealed NoCost $
            RunAbility (toTarget a) 1
        ]
    | otherwise = []

-- | [star] Forced Interrupt: when Ronan the Accuser activates against you, give him 1
-- additional boost card if you control the Power Stone. An attack "that does not get a
-- boost card" gets no additional one either.
instance RunMessage Ronan where
  runMessage msg e@(Ronan attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (_, 1) -> do
          advanceVillain ident
          pure $ advanceVillainTo ronan2 attrs
        (Expert, 2) -> do
          advanceVillain ident
          pure $ advanceVillainTo ronan3 attrs
        _ -> Ronan <$> runMessage msg attrs
    VillainMessage ident (VillainBeginAttack you) | ident == villainId attrs -> do
      whenM (controlsPowerStone you) $ dealBoost ident
      Ronan <$> runMessage msg attrs
    VillainMessage ident VillainBeginScheme | ident == villainId attrs -> do
      whenM (controlsPowerStone =<< getActivePlayerId) $ dealBoost ident
      Ronan <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      searchForAndRevealScheme if villainStage attrs == 2 then Cards.cutThePower else Cards.superiorTactics
      pure e
    _ -> Ronan <$> runMessage msg attrs
