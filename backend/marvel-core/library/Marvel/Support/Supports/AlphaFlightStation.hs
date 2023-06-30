module Marvel.Support.Supports.AlphaFlightStation (
  alphaFlightStation,
  AlphaFlightStation (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

alphaFlightStation :: SupportCard AlphaFlightStation
alphaFlightStation = support AlphaFlightStation Cards.alphaFlightStation

newtype AlphaFlightStation = AlphaFlightStation (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AlphaFlightStation where
  getAbilities (AlphaFlightStation a) =
    [ ability a 1 Action (OwnsThis) (ExhaustCost <> DiscardHandCardCost 1) $
        RunAbility (toTarget a) 1
    ]

instance RunMessage AlphaFlightStation where
  runMessage msg s@(AlphaFlightStation attrs) = case msg of
    RanAbility target 1 _ _ | isTarget attrs target -> do
      isCarolDanvers <- supportController attrs `match` IdentityWithTitle "Carol Danvers"
      let drawCount = if isCarolDanvers then 2 else 1
      push $ IdentityMessage (supportController attrs) $ DrawCards FromHand drawCount
      pure s
    _ -> AlphaFlightStation <$> runMessage msg attrs
