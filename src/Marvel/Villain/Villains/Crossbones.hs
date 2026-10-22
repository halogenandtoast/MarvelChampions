module Marvel.Villain.Villains.Crossbones where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Area (experimentalWeaponsDeck)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (Choice (RunAbility))
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window

newtype Crossbones = Crossbones (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

crossbones :: VillainCard Crossbones
crossbones = crossbones1

crossbones1 :: VillainCard Crossbones
crossbones1 = villain Crossbones Cards.crossbones1 (Sch 1) (Atk 1) (HP $ PerPlayer 12)

crossbones2 :: VillainCard Crossbones
crossbones2 = villainWith Crossbones Cards.crossbones2 (Sch 2) (Atk 2) (HP $ PerPlayer 14) (stageL .~ 2)

crossbones3 :: VillainCard Crossbones
crossbones3 = villainWith Crossbones Cards.crossbones3 (Sch 2) (Atk 3) (HP $ PerPlayer 16) (stageL .~ 3)

-- | While Crossbones has a Weapon attachment, his attacks gain piercing.
instance HasModifiersFor Crossbones where
  getModifiersFor _ (VillainRef vid) (Crossbones a) | vid == villainId a = do
    armed <- selectAny $ AttachmentOnEnemy (EnemyVillainId vid) <> AttachmentWithTrait Weapon
    pure [KeywordModifier Piercing | armed]
  getModifiersFor _ _ _ = pure []

-- | II: When Revealed, his Machine Gun is found and attached to him. III: When Revealed,
-- the top card of the Experimental Weapons deck is revealed.
instance HasAbilities Crossbones where
  getAbilities (Crossbones a)
    | villainStage a >= 2 =
        [ windowAbility a 1 (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain) ForcedResponse NoCost $
            RunAbility (toTarget a) 1
        ]
    | otherwise = []

instance RunMessage Crossbones where
  runMessage msg e@(Crossbones attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (_, 1) -> do
          advanceVillain ident
          pure $ advanceVillainTo crossbones2 attrs
        (Expert, 2) -> do
          advanceVillain ident
          pure $ advanceVillainTo crossbones3 attrs
        _ -> Crossbones <$> runMessage msg attrs
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      case villainStage attrs of
        2 -> searchAndRevealEncounterCard you (CardIs Attachments.crossbonesMachineGun)
        _ -> fromDeck experimentalWeaponsDeck (drawAndRevealEncounterCard you)
      pure e
    _ -> Crossbones <$> runMessage msg attrs
