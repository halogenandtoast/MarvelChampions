module Marvel.Obligation.Obligations.ManOutOfTime (
  manOutOfTime,
  ManOutOfTime (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

manOutOfTime :: ObligationCard ManOutOfTime
manOutOfTime = obligation ManOutOfTime Cards.manOutOfTime

newtype ManOutOfTime = ManOutOfTime (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage ManOutOfTime where
  runMessage msg o@(ManOutOfTime attrs) = case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        pushAll $
          [ Ask identityId $
            ChooseOne
              [ Label "$label.doNotFlipToAlterEgo" []
              , Label "$label.changeToB" [ChangeForm]
              ]
          | isHero
          ]
            <> [ObligationMessage (obligationId attrs) $ ResolveObligation identityId]
        pure o
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        handSize <- projectMap PlayerIdentityHand (length . unHand) identityId
        let half = fromIntegral handSize `div` 2
        chooseOrRunOne identityId $
          [ Label
            "$label.exhaustSteveRogersRemoveManOutOfTime"
            [ Run
                [ IdentityMessage identityId ExhaustedIdentity
                , RemoveFromGame (toTarget attrs)
                ]
            ]
          | isUnexhaustedAlterEgo
          ]
            <> [ Label
                  "$label.discardHalfYourHandDiscardObligation"
                  [Run [IdentityMessage identityId $ DiscardFrom FromHand half Nothing | half > 0]]
               ]
        pure o
      _ -> ManOutOfTime <$> runMessage msg attrs
    _ -> ManOutOfTime <$> runMessage msg attrs
