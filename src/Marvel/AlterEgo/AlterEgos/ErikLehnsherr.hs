module Marvel.AlterEgo.AlterEgos.ErikLehnsherr where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Identity.Types (Field(..))

import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Obligation.Cards qualified as Obligations
erikLehnsherr :: AlterEgoCard ErikLehnsherr
erikLehnsherr = alterEgo ErikLehnsherr Cards.erikLehnsherr (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.oldGrievances]

newtype ErikLehnsherr = ErikLehnsherr (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ErikLehnsherr where
  getAbilities a = [limitedWindowAbility a 1 (W.IdentityChangedToForm W.After You) Response IsSelf NoCost (runAbility a 1)]

instance RunMessage ErikLehnsherr where
  runMessage msg e@(ErikLehnsherr attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- take 3 <$> projectMap PlayerIdentityDiscard unDiscard you
      unless (null cards) $ shuffleIntoDeck you cards
      pure e
    _ -> ErikLehnsherr <$> runMessage msg attrs
