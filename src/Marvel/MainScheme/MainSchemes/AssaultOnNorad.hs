module Marvel.MainScheme.MainSchemes.AssaultOnNorad where

import Marvel.Prelude

import Marvel.Id (SchemeId (..))

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message
import Marvel.Question
import Marvel.Queue

assaultOnNorad :: MainSchemeCard AssaultOnNorad
assaultOnNorad =
  mainScheme AssaultOnNorad Cards.assaultOnNorad (PerPlayer 10) (Static 0) (PerPlayer 1)

newtype AssaultOnNorad = AssaultOnNorad (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage AssaultOnNorad where
  runMessage msg ms@(AssaultOnNorad attrs) = case msg of
    -- 2A: each player puts the top card of their deck into play as a Drone
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      players <- getPlayers
      pushAll $ map PutTopOfDeckIntoPlayAsDrone players
      pure ms
    -- Forced Response: after threat is placed here in step one of the villain phase, each
    -- player places 2 more or puts a Drone into play. Pushed first, so it follows the
    -- threat; should that complete the scheme, threat placed on it goes nowhere.
    AccelerateMainScheme -> do
      players <- getPlayers
      pushAll
        [ Ask p $
          ChooseOne
            [ Label "$label.placeTwoThreatOnAssaultOnNorad" [Run $ placeThreatOn (SchemeMainSchemeId $ mainSchemeId attrs) 2]
            , Label "$label.topCardAsDrone" [Run [PutTopOfDeckIntoPlayAsDrone p]]
            ]
        | p <- players
        ]
      AssaultOnNorad <$> runMessage msg attrs
    AdvanceMainScheme -> ms <$ push NextMainScheme
    _ -> AssaultOnNorad <$> runMessage msg attrs
