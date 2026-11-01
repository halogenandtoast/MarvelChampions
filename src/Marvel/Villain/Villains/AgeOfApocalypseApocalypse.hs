module Marvel.Villain.Villains.AgeOfApocalypseApocalypse where

import Marvel.Prelude
import Marvel.Game.Source ()
import Marvel.Keyword
import Marvel.Modifier
import Marvel.Card
import Marvel.Entity
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype AoaApocalypseVillain = AoaApocalypseVillain (Attrs Villain)
  deriving anyclass (IsVillain, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

apocalypse1 :: VillainCard AoaApocalypseVillain
apocalypse1 = villainWith AoaApocalypseVillain Cards.apocalypse1 (Sch 1) (Atk 2) (HP $ PerPlayer 8) (stageL .~ 1)

apocalypse2 :: VillainCard AoaApocalypseVillain
apocalypse2 = villainWith AoaApocalypseVillain Cards.apocalypse2 (Sch 2) (Atk 2) (HP $ PerPlayer 9) (stageL .~ 2)

apocalypse3 :: VillainCard AoaApocalypseVillain
apocalypse3 = villainWith AoaApocalypseVillain Cards.apocalypse3 (Sch 2) (Atk 3) (HP $ PerPlayer 10) (stageL .~ 3)

apocalypse4 :: VillainCard AoaApocalypseVillain
apocalypse4 = villainWith AoaApocalypseVillain Cards.apocalypse4 (Sch 3) (Atk 3) (HP $ PerPlayer 11) (stageL .~ 4)

instance HasModifiersFor AoaApocalypseVillain where
  getModifiersFor _ target (AoaApocalypseVillain attrs) = pure [KeywordModifier Overkill | isTarget attrs target, toCardCode (getCardDef attrs) == "45102b"]
instance RunMessage AoaApocalypseVillain where
  runMessage msg a@(AoaApocalypseVillain attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "aoaApocalypseHealAll" _ -> pure $ AoaApocalypseVillain attrs {villainHp = villainMaxHp attrs}
    CardSpecific (isTarget attrs -> True) "aoaApocalypseAdvanceStage" _ -> case villainStage attrs of
      1 -> advanceTo apocalypse2
      2 -> advanceTo apocalypse3
      3 -> advanceTo apocalypse4
      _ -> a <$ gameOver Lost
    _ -> AoaApocalypseVillain <$> runMessage msg attrs
   where
    advanceTo next = do
      cardSpecific GameRef "aoaApocalypseStageAdvanced" (toCardCode $ cbCardDef next)
      advanceVillain (villainId attrs)
      pure $ advanceVillainTo next attrs
