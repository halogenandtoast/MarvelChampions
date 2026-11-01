module Marvel.AlterEgo.AlterEgos.ScottSummers where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Window qualified as W ()
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Ally.Cards qualified as Allies
import Marvel.Event.Cards qualified as Events
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Trait qualified as Trait

scottSummers :: AlterEgoCard ScottSummers
scottSummers = alterEgo ScottSummers Cards.scottSummers (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.lostVisor]

newtype ScottSummers = ScottSummers (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ScottSummers where
  getAbilities a =
    [label "$label.constantTraining" $ limitedAbility a 1 (PerRound 1) Action IsSelf NoCost (runAbility a 1)]

instance RunMessage ScottSummers where
  runMessage msg a@(ScottSummers attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeck you AllOfDeck) (CardWithType UpgradeType <> CardWithTrait Trait.Tactic) SearchDrawOne ShuffleBackIn
      pure a
    _ -> ScottSummers <$> runMessage msg attrs

-- Cyclops's signature set, not every card shipped in his hero pack.
cyclopsCard :: CardMatcher
cyclopsCard = CardMatchesAny $ map CardIs
  [Allies.phoenixJeanGrey, Upgrades.rubyQuartzVisor, Upgrades.fieldCommander,
   Upgrades.exploitWeakness, Upgrades.practicedDefense, Upgrades.priorityTarget,
   Events.fullBlast, Events.ricochetBeam, Events.tacticalBrilliance]
