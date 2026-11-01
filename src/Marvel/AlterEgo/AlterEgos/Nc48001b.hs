module Marvel.AlterEgo.AlterEgos.Nc48001b where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.AlterEgo.Runner
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.Obligation.Cards qualified as Obligations

nc48001b :: AlterEgoCard Nc48001b
nc48001b = alterEgo Nc48001b Cards.nc48001b (HP $ Static 9) (HandSize 6) (Rec 3) [Obligations.nc48026]

newtype Nc48001b = Nc48001b (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Nc48001b where
  getAbilities a = [label "$label.nightcrawlerSearchBamf" $ limitedAbility a 1 (PerRound 1) Action IsSelf NoCost (runAbility a 1)]

instance RunMessage Nc48001b where
  runMessage msg a@(Nc48001b attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeck you AllOfDeck) (CardWithTitle "Bamf!") SearchDrawOne ShuffleBackIn
      pure a
    _ -> Nc48001b <$> runMessage msg attrs
