module Marvel.Obligation.Obligations.UnfulfilledDestiny (
  unfulfilledDestiny,
  UnfulfilledDestiny (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Card
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Query

unfulfilledDestiny :: ObligationCard UnfulfilledDestiny
unfulfilledDestiny = obligation UnfulfilledDestiny Cards.unfulfilledDestiny

newtype UnfulfilledDestiny = UnfulfilledDestiny (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form. Choose: exhaust your alter-ego → remove this from the
-- game (only while a ready alter-ego: RR Choose (Option)); or choose and discard 2 events
-- from your hand and discard this obligation. With fewer than 2 events, the ones there are
-- are discarded (ruling, docs/rules/hero-gamora.md).
instance RunMessage UnfulfilledDestiny where
  runMessage msg o@(UnfulfilledDestiny attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        canFlip <- identityMatches (HeroIdentity <> IdentityCanChangeForm) identityId
        when canFlip $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        UnfulfilledDestiny <$> runMessage msg attrs
      ResolveObligation identityId -> do
        readyAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        events <- filter (cardMatch $ CardWithType EventType) <$> projectMap PlayerIdentityHand unHand identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustGamoraRemoveUnfulfilledDestiny" (exhaust identityId >> removeFromGame attrs)
          | readyAlterEgo
          ]
            <> [ labeled (countLabel "discardEventsFromHand" 2) $
                  if length events <= 2
                    then for_ events discardCard
                    else
                      chooseOne identityId
                        [ targeting card do
                          discardCard card
                          chooseOne identityId [targeting other (discardCard other) | other <- events, other /= card]
                        | card <- events
                        ]
               ]
        pure o
      _ -> UnfulfilledDestiny <$> runMessage msg attrs
    _ -> UnfulfilledDestiny <$> runMessage msg attrs
