module Marvel.Event.Events.CrisisInterdiction (
  crisisInterdiction,
  CrisisInterdiction (..),
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
import Marvel.Ref

crisisInterdiction :: EventCard CrisisInterdiction
crisisInterdiction = event CrisisInterdiction Cards.crisisInterdiction

newtype CrisisInterdiction = CrisisInterdiction (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage CrisisInterdiction where
  runMessage msg e@(CrisisInterdiction attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        let
          handleRest [] = []
          handleRest xs =
            [ Run
                [ Ask identityId $
                    ChooseOne
                      [ TargetLabel
                        target
                        [ThwartScheme target (toSource attrs) 2]
                      | x <- xs
                      , let target = toRef x
                      ]
                ]
            ]
        schemesWithRest <- removeEach <$> selectList AnyScheme
        push $
          Ask identityId $
            ChooseOne
              [ TargetLabel target $
                ThwartScheme target (toSource attrs) 2
                  : handleRest rest
              | (x, rest) <- schemesWithRest
              , let target = toRef x
              ]
        pure e
      _ -> CrisisInterdiction <$> runMessage msg attrs
    _ -> CrisisInterdiction <$> runMessage msg attrs
