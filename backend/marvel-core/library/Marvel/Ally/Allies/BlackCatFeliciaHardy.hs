module Marvel.Ally.Allies.BlackCatFeliciaHardy
  ( BlackCatFeliciaHardy
  , blackCatFeliciaHardy
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Resource
import Marvel.Window

blackCatFeliciaHardy :: AllyCard BlackCatFeliciaHardy
blackCatFeliciaHardy = ally
  BlackCatFeliciaHardy
  Cards.blackCatFeliciaHardy
  (Thw 1, 1)
  (Atk 1, 0)
  (HP 2)

newtype BlackCatFeliciaHardy = BlackCatFeliciaHardy (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, FromJSON, ToJSON, IsTarget, IsSource, HasController)

instance HasAbilities BlackCatFeliciaHardy where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) ForcedResponse OwnsThis NoCost
        $ runAbility a 1
    ]

relevantCards :: [Card] -> [PlayerCard]
relevantCards = filter (cardMatch (CardWithResource Mental)) . onlyPlayerCards

instance RunMessage BlackCatFeliciaHardy where
  runMessage msg x@(BlackCatFeliciaHardy a) = case msg of
    RanAbility (isTarget x -> True) 1 _ _ -> do
      push $ controllerMessage x $ DiscardFrom FromDeck 2 (Just $ toTarget x)
      pure x
    WithDiscarded (isTarget a -> True) _ (relevantCards -> cards) -> do
      pushAll $ map (controllerMessage x . AddToHand) cards
      pure x
    _ -> BlackCatFeliciaHardy <$> runMessage msg a
