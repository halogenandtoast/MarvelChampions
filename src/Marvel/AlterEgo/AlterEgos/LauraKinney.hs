module Marvel.AlterEgo.AlterEgos.LauraKinney where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Card.Def (CardMatcher (..))
import Marvel.Ally.Cards qualified as Allies
import Marvel.Event.Cards qualified as Events
import Marvel.Question (runAbility)
import Marvel.Ability
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

lauraKinney :: AlterEgoCard LauraKinney
lauraKinney = alterEgo LauraKinney Cards.lauraKinney (HP $ Static 10) (HandSize 6) (Rec 6) [Obligations.selfIsolation]

newtype LauraKinney = LauraKinney (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LauraKinney where
  getAbilities a =
    [ ability a 1 Setup IsSelf NoCost (runAbility a 1)
    , label "$label.lauraKinneyShuffle" $ limitedAbility a 2 (PerRound 1) AlterEgoAction IsSelf
        (ShuffleDiscardCardIntoDeckCost $ CardMatchesAny [CardIs Allies.honeyBadger, CardIs Events.sisterlyBond]) (runAbility a 2)
    ]

instance RunMessage LauraKinney where
  runMessage msg ae@(LauraKinney attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> ae <$ putCardFromSetAsideIntoPlay you "43002"
    RanAbility you (isTarget attrs -> True) 2 _ _ -> ae <$ drawCards you 1
    _ -> LauraKinney <$> runMessage msg attrs
