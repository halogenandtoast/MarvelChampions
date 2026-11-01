module Marvel.Minion.Minions.DonaldPierce where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

donaldPierce :: MinionCard DonaldPierce
donaldPierce = minion DonaldPierce Cards.donaldPierce (Sch 1) (Atk 1) (HP 6)

newtype DonaldPierce = DonaldPierce (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DonaldPierce where
  getAbilities (DonaldPierce attrs) = [windowAbility attrs 1 (W.EngagesMinion W.After AnyIdentity $ MinionWithId $ minionId attrs) ForcedResponse NoCost (runAbility attrs 1)]

instance RunMessage DonaldPierce where
  runMessage msg m@(DonaldPierce attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      let you = minionEngagedIdentity attrs
      key <- getDeckKeyFor you
      pile <- getPile key
      for_ (headMay $ filter (cardMatch $ CardWithTrait Trait.Reaver <> CardWithType MinionType) $ pileDiscard pile) (revealEncounterCard you)
      pure m
    _ -> DonaldPierce <$> runMessage msg attrs
