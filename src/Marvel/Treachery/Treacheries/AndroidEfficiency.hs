module Marvel.Treachery.Treacheries.AndroidEfficiency (
  androidEfficiencyA,
  androidEfficiencyB,
  androidEfficiencyC,
  AndroidEfficiency (..),
) where

import Marvel.Prelude

import Data.Text qualified as T

import Marvel.ActiveCost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Choice
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message
import Marvel.Payment
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

androidEfficiencyA :: TreacheryCard AndroidEfficiency
androidEfficiencyA = treachery AndroidEfficiency Cards.androidEfficiencyA

androidEfficiencyB :: TreacheryCard AndroidEfficiency
androidEfficiencyB = treachery AndroidEfficiency Cards.androidEfficiencyB

androidEfficiencyC :: TreacheryCard AndroidEfficiency
androidEfficiencyC = treachery AndroidEfficiency Cards.androidEfficiencyC

newtype AndroidEfficiency = AndroidEfficiency (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The resource each copy's boost asks for.
boostResource :: Attrs Treachery -> Resource
boostResource attrs = case cdCardCode (getCardDef attrs) of
  "01144a" -> Energy
  "01144b" -> Mental
  _ -> Physical

instance RunMessage AndroidEfficiency where
  runMessage msg t@(AndroidEfficiency attrs) = case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      players <- getPlayers
      pushAll $ map PutTopOfDeckIntoPlayAsDrone players
      pure t
    -- Boost: spend the resource or put the top card of your deck into play as a Drone
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      ident <- getActivePlayerId
      resources <- getAvailableResourcesFor ident Nothing
      activeCostId <- getRandom
      let resource = boostResource attrs
      chooseOrRunOne ident $
        Label "Put the top card of your deck into play as a Drone" [Run [PutTopOfDeckIntoPlayAsDrone ident]]
          : [ Label
              ("Spend a {" <> T.toLower (tshow resource) <> "} resource")
              [ Run
                  [ SetActiveCost $
                      ActiveCost
                        activeCostId
                        ident
                        ForTreachery
                        (MultiResourceCost [Just resource])
                        NoPayment
                        Nothing
                        mempty
                  ]
              ]
            | any (`elem` resources) [resource, Wild]
            ]
      pure t
    _ -> AndroidEfficiency <$> runMessage msg attrs
