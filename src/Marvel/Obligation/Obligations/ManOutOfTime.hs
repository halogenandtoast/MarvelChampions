module Marvel.Obligation.Obligations.ManOutOfTime (
  manOutOfTime,
  ManOutOfTime (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ability.Types (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Projection

manOutOfTime :: ObligationCard ManOutOfTime
manOutOfTime = obligation ManOutOfTime Cards.manOutOfTime

newtype ManOutOfTime = ManOutOfTime (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage ManOutOfTime where
  runMessage msg o@(ManOutOfTime attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        when isHero $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        pure o
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        handSize <- projectMap PlayerIdentityHand (length . unHand) identityId
        let half = fromIntegral handSize `div` 2
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustSteveRogersRemoveManOutOfTime" (exhaust identityId >> removeFromGame attrs)
          | isUnexhaustedAlterEgo
          ]
            <> [ labeled "$label.discardHalfYourHandDiscardObligation" $
                  when (half > 0) $ discardFromHand identityId half
               ]
        pure o
      _ -> ManOutOfTime <$> runMessage msg attrs
    _ -> ManOutOfTime <$> runMessage msg attrs
