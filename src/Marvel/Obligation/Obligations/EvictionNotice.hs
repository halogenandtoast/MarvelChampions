module Marvel.Obligation.Obligations.EvictionNotice (
  evictionNotice,
  EvictionNotice (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ability.Types (HasAbilities)
import Marvel.Modifier (HasModifiersFor)

evictionNotice :: ObligationCard EvictionNotice
evictionNotice = obligation EvictionNotice Cards.evictionNotice

newtype EvictionNotice = EvictionNotice (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage EvictionNotice where
  runMessage msg o@(EvictionNotice attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        when isHero $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeForm identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        pure o
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustPeterParkerRemoveEvictionNotice" (exhaust identityId >> removeFromGame attrs)
          | isUnexhaustedAlterEgo
          ]
            <> [ labeled "$label.discardRandomCardSurgeDiscardObligation" do
                  discardFrom identityId RandomFromHand 1
                  gainSurge attrs
               ]
        pure o
      _ -> EvictionNotice <$> runMessage msg attrs
    _ -> EvictionNotice <$> runMessage msg attrs
