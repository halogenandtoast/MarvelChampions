-- | Ebony Maw (The Mad Titan's Shadow, scenario 1).
module Marvel.Villain.Villains.EbonyMaw (
  ebonyMaw1,
  ebonyMaw2,
  ebonyMaw3,
  EbonyMaw (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Environment.Environments.EbonyMawSpells (eachPlayerPutsSpellIntoPlay, removeInvocationCounters)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (Choice (RunAbility))
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window
import Marvel.Window qualified as W
import Marvel.Id (EnemyId (..))

newtype EbonyMaw = EbonyMaw (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

ebonyMaw1 :: VillainCard EbonyMaw
ebonyMaw1 = villain EbonyMaw Cards.ebonyMaw1 (Sch 2) (Atk 1) (HP $ PerPlayer 14)

ebonyMaw2 :: VillainCard EbonyMaw
ebonyMaw2 = villainWith EbonyMaw Cards.ebonyMaw2 (Sch 2) (Atk 2) (HP $ PerPlayer 18) (stageL .~ 2)

ebonyMaw3 :: VillainCard EbonyMaw
ebonyMaw3 = villainWith EbonyMaw Cards.ebonyMaw3 (Sch 3) (Atk 2) (HP $ PerPlayer 23) (stageL .~ 3)

-- | II and III, When Revealed: each player discards cards from the top of the encounter
-- deck until they discard a Spell card and puts that card into play in their play area.
instance HasAbilities EbonyMaw where
  getAbilities (EbonyMaw a)
    | villainStage a >= 2 =
        [ windowAbility a 1 (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain) WhenRevealed NoCost $
            RunAbility (toTarget a) 1
        ]
    | otherwise = []

-- | Each stage, [star] Forced Interrupt: when Ebony Maw activates against you, remove an
-- invocation counter from each Spell in your play area. A stunned attack or confused
-- scheme is not resolved, its status card removed instead, and the interrupt does not
-- happen (MTS rulebook p.7).
instance RunMessage EbonyMaw where
  runMessage msg e@(EbonyMaw attrs) = runQueueT $ case msg of
    VillainMessage ident msg' | ident == villainId attrs -> case msg' of
      VillainDefeated -> do
        difficulty <- getDifficulty
        case (difficulty, villainStage attrs) of
          (_, 1) -> do
            advanceVillain ident
            pure $ advanceVillainTo ebonyMaw2 attrs
          (Expert, 2) -> do
            advanceVillain ident
            pure $ advanceVillainTo ebonyMaw3 attrs
          _ -> passOn
      _ -> passOn
    -- as his activation against you begins, before its boost cards are dealt: every
    -- attack and scheme opens it, a stunned attack or confused scheme none
    CheckWindows ws
      | you : _ <- [p | W.Window W.When (W.EnemyActivation en p) <- ws, en == EnemyVillainId (villainId attrs)] ->
          removeInvocationCounters you >> passOn
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ eachPlayerPutsSpellIntoPlay
    _ -> passOn
   where
    passOn = EbonyMaw <$> runMessage msg attrs
