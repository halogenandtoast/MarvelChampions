module Marvel.Environment.Environments.MutantGenesis where

import Marvel.Id
import Marvel.Prelude
import Marvel.Entity
import Marvel.Card
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Environment.Types
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Villain.Cards qualified as Villains
import Marvel.Ally.Cards qualified as Allies
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Environment.Cards qualified as Cards

newtype MutantGenesisEnvironment = MutantGenesisEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mgProtectTheSenator :: EnvironmentCard MutantGenesisEnvironment
mgProtectTheSenator = environment MutantGenesisEnvironment Cards.mgProtectTheSenator

mgSaveTheSchool :: EnvironmentCard MutantGenesisEnvironment
mgSaveTheSchool = environment MutantGenesisEnvironment Cards.mgSaveTheSchool

instance HasModifiersFor MutantGenesisEnvironment where
  getModifiersFor _ (AllyRef aid) (MutantGenesisEnvironment a) | toCardCode a == "32065b" = do
    senator <- selectAny (AllyWithId aid <> AllyIs Allies.mgRobertKelly)
    pure $ if senator then [CannotHaveAttachedPlayerCards, CannotBeHealedByPlayerCards] else []
  getModifiersFor _ _ _ = pure []

instance HasAbilities MutantGenesisEnvironment where
  getAbilities (MutantGenesisEnvironment a)
    | toCardCode a == "32065b" =
        [(windowAbility a 1 (W.WindowOneOf [W.HeroDefended W.After You (EnemyIs def) | def <- [Villains.mgSabretooth60, Villains.mgSabretooth61, Villains.mgSabretooth62]]) HeroResponse (MultiResourceCost [Nothing, Nothing]) (runAbility a 1)) {abilityCriteria = AllyExists (AllyIs Allies.mgRobertKelly <> AllyControlledBy You)}]
    | otherwise = []

instance RunMessage MutantGenesisEnvironment where
  runMessage msg e@(MutantGenesisEnvironment attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      senators <- selectList (AllyIs Allies.mgRobertKelly <> AllyControlledBy (IdentityWithId you))
      unless (null senators) $ ready you
      pure e
    _ -> MutantGenesisEnvironment <$> runMessage msg attrs
