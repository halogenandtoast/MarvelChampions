module Marvel.Support.Supports.AssaultTraining where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W
import Marvel.Aspect (Aspect (Aggression))
assaultTraining :: SupportCard AssaultTraining
assaultTraining = supportWith AssaultTraining Cards.assaultTraining ((usesL .~ 2) . (discardIfNoUsesL .~ True))

newtype AssaultTraining = AssaultTraining (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AssaultTraining where
  getAbilities (AssaultTraining a) =
    [ ability a 1 AlterEgoAction
        (OwnsThis <> ExtendedCardExists (InDiscardOf (IdentityWithId $ supportController a) $ BasicCardMatches $ CardWithType EventType <> CardWithAspect Aggression))
        (ExhaustCost <> UseCost) (runAbility a 1)
    ]

instance RunMessage AssaultTraining where
  runMessage msg s@(AssaultTraining attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ CardWithType EventType <> CardWithAspect Aggression)
      unless (null cards) $ chooseOne you [targeting c (shuffleIntoDeck you [c]) | c <- cards]
      pure s
    _ -> AssaultTraining <$> runMessage msg attrs
