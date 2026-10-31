module Marvel.SideScheme.SideSchemes.TheAssassinsGuild where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.GameValue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theAssassinsGuild :: SideSchemeCard TheAssassinsGuild
theAssassinsGuild = sideScheme TheAssassinsGuild Cards.theAssassinsGuild (Static 4)

newtype TheAssassinsGuild = TheAssassinsGuild (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme TheAssassinsGuild where
  toSideSchemeAttrs (TheAssassinsGuild attrs) = attrs
  sideSchemeAbilities = getAbilities

instance HasAbilities TheAssassinsGuild where
  getAbilities (TheAssassinsGuild attrs) =
    [windowAbility attrs 1 (W.CharacterDefeatedByEnemyAttack W.After $ MinionEnemy <> EnemyWithTrait Trait.Assassin)
      ForcedResponse NoCost (runAbility attrs 1)]

instance RunMessage TheAssassinsGuild where
  runMessage msg s@(TheAssassinsGuild attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      placeThreat (sideSchemeId attrs) 2
      pure s
    _ -> TheAssassinsGuild <$> runMessage msg attrs
