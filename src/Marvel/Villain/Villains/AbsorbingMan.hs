module Marvel.Villain.Villains.AbsorbingMan where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (Choice (RunAbility))
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Stats
import Marvel.Trait
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types

import Marvel.Window (RevealSource (..), WindowMatcher (EnemyAttacked, VillainRevealed, WindowOneOf), WindowTiming (..))
import Marvel.Window qualified as W

newtype AbsorbingMan = AbsorbingMan (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

absorbingMan :: VillainCard AbsorbingMan
absorbingMan = absorbingMan1

absorbingMan1 :: VillainCard AbsorbingMan
absorbingMan1 = villain AbsorbingMan Cards.absorbingMan1 (Sch 1) (Atk 2) (HP $ PerPlayer 14)

absorbingMan2 :: VillainCard AbsorbingMan
absorbingMan2 = villainWith AbsorbingMan Cards.absorbingMan2 (Sch 2) (Atk 2) (HP $ PerPlayer 15) (stageL .~ 2)

absorbingMan3 :: VillainCard AbsorbingMan
absorbingMan3 = villainWith AbsorbingMan Cards.absorbingMan3 (Sch 2) (Atk 3) (HP $ PerPlayer 16) (stageL .~ 3)

-- | He gains the trait of each environment in play.
instance HasModifiersFor AbsorbingMan where
  getModifiersFor _ (VillainRef vid) (AbsorbingMan a)
    | vid == villainId a = map TraitModifier . toList <$> getEnvironmentTraits
  getModifiersFor _ _ _ = pure []

instance HasAbilities AbsorbingMan where
  getAbilities (AbsorbingMan a) = case villainStage a of
    2 ->
      [ windowAbility a 1 (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain) WhenRevealed NoCost $
          RunAbility (toTarget a) 1
      ]
    -- after he activates against you: he attacks you, or schemes in your activation
    3 ->
      [ windowAbility
          a
          2
          (WindowOneOf [EnemyAttacked After (EnemyWithId $ EnemyVillainId $ villainId a) You, W.VillainSchemed After (VillainWithId $ villainId a)])
          ForcedResponse
          NoCost
          $ RunAbility (toTarget a) 2
      ]
    _ -> []

instance RunMessage AbsorbingMan where
  runMessage msg e@(AbsorbingMan attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (_, 1) -> do
          advanceVillain ident
          pure $ advanceVillainTo absorbingMan2 attrs
        (Expert, 2) -> do
          advanceVillain ident
          pure $ advanceVillainTo absorbingMan3 attrs
        _ -> AbsorbingMan <$> runMessage msg attrs
    -- II: with Super Absorbing Power in play each player is dealt an encounter card;
    -- otherwise it is found and revealed
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      inPlay <- selectAny $ SideSchemeIs SideSchemes.superAbsorbingPower
      if inPlay
        then traverse_ dealEncounterCard =<< getPlayers
        else searchForAndRevealScheme SideSchemes.superAbsorbingPower
      pure e
    -- III: Ice or Stone, 1 threat on the main scheme; Metal or Wood, 1 indirect damage
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      let has t = villainMatches (VillainWithTrait t) (villainId attrs)
      whenM (orM [has Ice, has Stone]) $ placeThreatOnEach attrs 1 EncounterMainScheme
      whenM (orM [has Metal, has Wood]) $ dealIndirectDamage you attrs 1
      pure e
    _ -> AbsorbingMan <$> runMessage msg attrs
