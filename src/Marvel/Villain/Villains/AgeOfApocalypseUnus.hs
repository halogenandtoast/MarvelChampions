module Marvel.Villain.Villains.AgeOfApocalypseUnus where

import Marvel.Prelude
import Marvel.Entity
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier
import Marvel.Keyword
import Marvel.SideScheme.SideSchemes.AgeOfApocalypseInfinites (genePoolThreat)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype AoaUnusVillain = AoaUnusVillain (Attrs Villain)
  deriving anyclass (IsVillain, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

unus1 :: VillainCard AoaUnusVillain
unus1 = villainWith AoaUnusVillain Cards.unus1 (Sch 1) (Atk 2) (HP $ PerPlayer 12) (stageL .~ 1)

unus2 :: VillainCard AoaUnusVillain
unus2 = villainWith AoaUnusVillain Cards.unus2 (Sch 2) (Atk 2) (HP $ PerPlayer 15) (stageL .~ 2)

unus3 :: VillainCard AoaUnusVillain
unus3 = villainWith AoaUnusVillain Cards.unus3 (Sch 2) (Atk 3) (HP $ PerPlayer 18) (stageL .~ 3)

instance HasModifiersFor AoaUnusVillain where
  getModifiersFor _ target (AoaUnusVillain a) | target == toRef a = do
    threat <- genePoolThreat
    pure $ [KeywordModifier (Retaliate 1) | threat >= 3]
      <> [KeywordModifier Stalwart | threat >= 6]
      <> [AmplifyIconModifier 1 | threat >= 9]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaUnusVillain where
  runMessage msg (AoaUnusVillain a) = AoaUnusVillain <$> runMessage msg a
