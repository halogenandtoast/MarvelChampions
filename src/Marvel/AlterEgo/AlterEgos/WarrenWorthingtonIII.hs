module Marvel.AlterEgo.AlterEgos.WarrenWorthingtonIII where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Obligations

warrenWorthingtonIII :: AlterEgoCard WarrenWorthingtonIII
warrenWorthingtonIII = alterEgo WarrenWorthingtonIII Cards.warrenWorthingtonIII (HP $ Static 12) (HandSize 6) (Rec 3) [Obligations.apocalypticInfluence]

newtype WarrenWorthingtonIII = WarrenWorthingtonIII (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WarrenWorthingtonIII where
  getAbilities a@(WarrenWorthingtonIII attrs) =
    [label "Regrowth" $ limitedAbility a 1 (PerRound 1) Action
      (IsSelf <> SelfMatches (IdentityCanBeHealed <> IdentityWithAnyDamage)) NoCost (Heal (IdentityCharacter $ alterEgoIdentityId attrs) 1)]

instance RunMessage WarrenWorthingtonIII where
  runMessage msg (WarrenWorthingtonIII attrs) = runQueueT $ WarrenWorthingtonIII <$> runMessage msg attrs
