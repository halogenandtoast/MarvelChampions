module Marvel.Villain.Villains.Thanos (
  Thanos (..),
  thanos1,
  thanos2,
  thanos3,
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Attachments
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
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window
import Marvel.Window qualified as W

-- | Stalwart on each stage, and Toughness on II and III: their cards'.
newtype Thanos = Thanos (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

thanos1 :: VillainCard Thanos
thanos1 = villain Thanos Cards.thanos1 (Sch 1) (Atk 2) (HP $ PerPlayer 16)

thanos2 :: VillainCard Thanos
thanos2 = villainWith Thanos Cards.thanos2 (Sch 2) (Atk 3) (HP $ PerPlayer 23) (stageL .~ 2)

thanos3 :: VillainCard Thanos
thanos3 = villainWith Thanos Cards.thanos3 (Sch 2) (Atk 4) (HP $ PerPlayer 28) (stageL .~ 3)

-- | II and III, When Revealed: search the encounter deck and discard pile for Thanos's
-- Helmet and reveal it.
instance HasAbilities Thanos where
  getAbilities (Thanos a)
    | villainStage a >= 2 =
        [ windowAbility a 1 (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain) WhenRevealed NoCost $
            RunAbility (toTarget a) 1
        ]
    | otherwise = []

-- | Forced Response: after the Infinity Stone deck runs out, give Thanos 1 facedown boost
-- card.
instance RunMessage Thanos where
  runMessage msg e@(Thanos attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (_, 1) -> do
          advanceVillain ident
          pure $ advanceVillainTo thanos2 attrs
        (Expert, 2) -> do
          advanceVillain ident
          pure $ advanceVillainTo thanos3 attrs
        _ -> Thanos <$> runMessage msg attrs
    CheckWindows ws | W.Window W.After W.InfinityStoneDeckRanOut `elem` ws -> do
      dealBoost (villainId attrs)
      pure e
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      searchForAndRevealScheme Attachments.thanossHelmet
      pure e
    _ -> Thanos <$> runMessage msg attrs
