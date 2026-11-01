module Marvel.Minion.Minions.Skullbuster where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

skullbuster :: MinionCard Skullbuster
skullbuster = minion Skullbuster Cards.skullbuster (Sch 2) (Atk 2) (HP 5)

newtype Skullbuster = Skullbuster (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Skullbuster where
  getAbilities (Skullbuster attrs) = [windowAbility attrs 1 (W.EngagesMinion W.After AnyIdentity $ MinionWithId $ minionId attrs) ForcedResponse NoCost (runAbility attrs 1)]

instance RunMessage Skullbuster where
  runMessage msg m@(Skullbuster attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      let you = minionEngagedIdentity attrs
      minions <- selectList (MinionWithTrait Trait.Reaver <> MinionEngagedWith (IdentityWithId you))
      resolveAs you $ PlaceThreat (toSource attrs) (fromIntegral $ length minions) EncounterMainScheme
      pure m
    _ -> Skullbuster <$> runMessage msg attrs
