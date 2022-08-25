module Marvel.Support.Supports.AlphaFlightStation
  ( alphaFlightStation
  , AlphaFlightStation(..)
  )
where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Support.Types
import Marvel.Support.Cards qualified as Cards
import Marvel.Target

alphaFlightStation :: SupportCard AlphaFlightStation
alphaFlightStation = support AlphaFlightStation Cards.alphaFlightStation

newtype AlphaFlightStation = AlphaFlightStation SupportAttrs
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities AlphaFlightStation where
  getAbilities (AlphaFlightStation a) = [ability a 1 Action (OwnsThis) (ExhaustCost <> DiscardHandCardCost 1) $ undefined ]

instance RunMessage AlphaFlightStation where
  runMessage msg (AlphaFlightStation attrs) =
    AlphaFlightStation <$> runMessage msg attrs
