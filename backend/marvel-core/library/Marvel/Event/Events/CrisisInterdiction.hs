module Marvel.Event.Events.CrisisInterdiction
  ( crisisInterdiction
  , CrisisInterdiction(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target

crisisInterdiction :: EventCard CrisisInterdiction
crisisInterdiction = event CrisisInterdiction Cards.crisisInterdiction

newtype CrisisInterdiction = CrisisInterdiction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)


instance RunMessage CrisisInterdiction where
  runMessage msg e@(CrisisInterdiction attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        let
          handleRest [] = []
          handleRest xs =
            [ Run
                [ Ask identityId $ ChooseOne
                    [ TargetLabel
                        target
                        [ThwartScheme target (toSource attrs) 2]
                    | x <- xs
                    , let target = SchemeTarget x
                    ]
                ]
            ]
        schemesWithRest <- removeEach <$> selectList AnyScheme
        push $ Ask identityId $ ChooseOne
          [ TargetLabel target
            $ ThwartScheme target (toSource attrs) 2
            : handleRest rest
          | (x, rest) <- schemesWithRest
          , let target = SchemeTarget x
          ]
        pure e
      _ -> CrisisInterdiction <$> runMessage msg attrs
    _ -> CrisisInterdiction <$> runMessage msg attrs
