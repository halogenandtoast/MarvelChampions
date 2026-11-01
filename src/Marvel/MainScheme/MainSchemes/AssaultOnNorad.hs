module Marvel.MainScheme.MainSchemes.AssaultOnNorad where

import Marvel.Prelude

import Marvel.Id ()

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted

assaultOnNorad :: MainSchemeCard AssaultOnNorad
assaultOnNorad =
  mainScheme AssaultOnNorad Cards.assaultOnNorad (PerPlayer 10) (Static 0) (PerPlayer 1)

newtype AssaultOnNorad = AssaultOnNorad (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage AssaultOnNorad where
  runMessage msg ms@(AssaultOnNorad attrs) = runQueueT $ case msg of
    -- 2A: each player puts the top card of their deck into play as a Drone
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      players <- getPlayers
      for_ players putTopOfDeckIntoPlayAsDrone
      pure ms
    -- Forced Response: after threat is placed here in step one of the villain phase, each
    -- player places 2 more or puts a Drone into play: the threat first; should that
    -- complete the scheme, threat placed on it goes nowhere.
    AccelerateMainScheme -> do
      ms' <- AssaultOnNorad <$> runMessage msg attrs
      forcedResponse (mainSchemeId attrs) do
        players <- getPlayers
        for_ players \p ->
          chooseOne
            p
            [ labeled (countLabel "placeThreatOnAssaultOnNorad" 2) $ placeThreat (mainSchemeId attrs) 2
            , labeled "$label.topCardAsDrone" $ putTopOfDeckIntoPlayAsDrone p
            ]
      pure ms'
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ nextMainScheme (mainSchemeId attrs)
    _ -> AssaultOnNorad <$> runMessage msg attrs
