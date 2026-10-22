module Marvel.Ally.Allies.FalconSamWilson where

import Marvel.Prelude

import Data.List (genericReplicate)

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Window

falconSamWilson :: AllyCard FalconSamWilson
falconSamWilson = ally FalconSamWilson Cards.falconSamWilson (Thw 2, 1) (Atk 2, 1) (HP 3)

instance HasAbilities FalconSamWilson where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response OwnsThis NoCost $
        runAbility a 1
    ]

newtype FalconSamWilson = FalconSamWilson (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | The top 3 cards of the encounter deck are looked at, and stay where they are: 1
-- threat is removed for each treachery among them.
instance RunMessage FalconSamWilson where
  runMessage msg a@(FalconSamWilson attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      top <- take 3 <$> getEncounterDeck
      let treacheries = count ((== TreacheryType) . cdCardType . getCardDef) top
      focusing (EncounterCard <$> top) $
        chooseOne ident
          [labeled "$label.done" $ sequence_ $ genericReplicate treacheries $ removeThreat ident a 1 ThwartableScheme]
      pure a
    _ -> FalconSamWilson <$> runMessage msg attrs
