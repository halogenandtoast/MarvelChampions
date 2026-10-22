module Marvel.Villain.Villains.Zola where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Def
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

-- | Retaliate 1 is on each stage's card.
newtype Zola = Zola (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

zola :: VillainCard Zola
zola = zola1

zola1 :: VillainCard Zola
zola1 = villain Zola Cards.zola1 (Sch 2) (Atk 1) (HP $ PerPlayer 12)

zola2 :: VillainCard Zola
zola2 = villainWith Zola Cards.zola2 (Sch 2) (Atk 2) (HP $ PerPlayer 14) (stageL .~ 2)

zola3 :: VillainCard Zola
zola3 = villainWith Zola Cards.zola3 (Sch 3) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 3)

-- | II: search the encounter deck and discard pile for Test Subjects and reveal it. III:
-- each player searches them for a minion and reveals it.
instance HasAbilities Zola where
  getAbilities (Zola a)
    | villainStage a >= 2 =
        [ windowAbility a 1 (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain) ForcedResponse NoCost $
            RunAbility (toTarget a) 1
        ]
    | otherwise = []

instance RunMessage Zola where
  runMessage msg e@(Zola attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (_, 1) -> do
          advanceVillain ident
          pure $ advanceVillainTo zola2 attrs
        (Expert, 2) -> do
          advanceVillain ident
          pure $ advanceVillainTo zola3 attrs
        _ -> Zola <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      case villainStage attrs of
        2 -> searchForAndRevealScheme Cards.testSubjects
        _ -> do
          players <- getPlayers
          for_ players \p -> searchAndRevealEncounterCard p (CardWithType MinionType)
      pure e
    _ -> Zola <$> runMessage msg attrs
