module Marvel.Villain.Villains.SmVenomGoblin where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Window
import Data.Aeson ()
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Query
import Marvel.MainScheme.MainSchemes.SmVenomGoblin qualified as Goblin
import Marvel.Window qualified as W
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types

newtype SmVenomGoblinVillain = SmVenomGoblinVillain (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

smVenomGoblin1 :: VillainCard SmVenomGoblinVillain
smVenomGoblin1 = villainWith SmVenomGoblinVillain Cards.smVenomGoblin1 (Sch 2) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 1)

smVenomGoblin2 :: VillainCard SmVenomGoblinVillain
smVenomGoblin2 = villainWith SmVenomGoblinVillain Cards.smVenomGoblin2 (Sch 2) (Atk 3) (HP $ PerPlayer 18) (stageL .~ 2)

smVenomGoblin3 :: VillainCard SmVenomGoblinVillain
smVenomGoblin3 = villainWith SmVenomGoblinVillain Cards.smVenomGoblin3 (Sch 3) (Atk 3) (HP $ PerPlayer 21) (stageL .~ 3)

instance HasAbilities SmVenomGoblinVillain where
  getAbilities (SmVenomGoblinVillain attrs) =
    [windowAbility attrs 1 (VillainRevealed When (VillainWithId $ villainId attrs) RevealedFromVillain) WhenRevealed NoCost (runAbility attrs 1) | villainStage attrs >= 2]

instance RunMessage SmVenomGoblinVillain where
  runMessage msg e@(SmVenomGoblinVillain attrs) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (Normal, 1) -> advanceVillain vid >> pure (advanceVillainTo smVenomGoblin2 attrs)
        (Expert, 2) -> advanceVillain vid >> pure (advanceVillainTo smVenomGoblin3 attrs)
        _ -> SmVenomGoblinVillain <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (if villainStage attrs == 2 then [1,2 :: Int] else if villainStage attrs == 3 then [1,2,3] else []) $ \_ ->
        selectList AnyIdentity >>= traverse_ dealEncounterCard
      SmVenomGoblinVillain <$> runMessage msg attrs
    CheckWindows windows -> do
      for_ [you | W.Window W.After (W.EnemyActivation enemy you) <- windows, enemy == EnemyVillainId (villainId attrs)] $ \you ->
        forcedResponse attrs $ Goblin.chooseGliderScheme False you "goblinAfterActivation"
      SmVenomGoblinVillain <$> runMessage msg attrs
    CardSpecific GameRef "goblinAfterActivation" (fromJSON -> Success (you, sid)) -> do
      case villainStage attrs of
        1 -> chooseOne you
          [ targeting sid $ placeThreat sid 2
          , targeting sid $ Goblin.resolveGoblinSpecial you sid
          ]
        3 -> placeThreat sid 1 >> Goblin.resolveGoblinSpecial you sid
        _ -> Goblin.resolveGoblinSpecial you sid
      pure e
    _ -> SmVenomGoblinVillain <$> runMessage msg attrs
