module Marvel.Support.Supports.TheXjet where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

theXjet :: SupportCard TheXjet
theXjet = support TheXjet Cards.theXjet

newtype TheXjet = TheXjet (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TheXjet where
  getAbilities (TheXjet a) =
    [ability a 1 Resource (ForAnyPlayer <> SelfMatches (IdentityWithTrait Trait.XMen)) ExhaustCost (Pay $ ResourcePayment Wild)]

instance RunMessage TheXjet where
  runMessage msg e@(TheXjet attrs) = runQueueT $ case msg of
    _ -> TheXjet <$> runMessage msg attrs
