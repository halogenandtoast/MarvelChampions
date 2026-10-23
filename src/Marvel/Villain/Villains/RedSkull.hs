module Marvel.Villain.Villains.RedSkull where

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
import Marvel.Query
import Marvel.Question (Choice (RunAbility))
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window

newtype RedSkull = RedSkull (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

redSkull :: VillainCard RedSkull
redSkull = redSkull1

redSkull1 :: VillainCard RedSkull
redSkull1 = villain RedSkull Cards.redSkull1 (Sch 2) (Atk 0) (HP $ PerPlayer 12)

redSkull2 :: VillainCard RedSkull
redSkull2 = villainWith RedSkull Cards.redSkull2 (Sch 3) (Atk 1) (HP $ PerPlayer 16) (stageL .~ 2)

redSkull3 :: VillainCard RedSkull
redSkull3 = villainWith RedSkull Cards.redSkull3 (Sch 3) (Atk 2) (HP $ PerPlayer 20) (stageL .~ 3)

-- | Red Skull gets +1 ATK for each side scheme in play.
instance HasModifiersFor RedSkull where
  getModifiersFor _ (VillainRef vid) (RedSkull a) | vid == villainId a = do
    n <- selectListCount AnySideScheme
    pure [AttackModifier n | n > 0]
  getModifiersFor _ _ _ = pure []

-- | II and III: When Revealed, deal each player an encounter card.
instance HasAbilities RedSkull where
  getAbilities (RedSkull a)
    | villainStage a > 1 =
        [ windowAbility
            a
            1
            (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain)
            WhenRevealed
            NoCost
            (RunAbility (toTarget a) 1)
        ]
    | otherwise = []

instance RunMessage RedSkull where
  runMessage msg e@(RedSkull attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (_, 1) -> do
          advanceVillain ident
          pure $ advanceVillainTo redSkull2 attrs
        (Expert, 2) -> do
          advanceVillain ident
          pure $ advanceVillainTo redSkull3 attrs
        _ -> RedSkull <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      for_ players dealEncounterCard
      pure e
    _ -> RedSkull <$> runMessage msg attrs
