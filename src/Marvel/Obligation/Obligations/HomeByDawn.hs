module Marvel.Obligation.Obligations.HomeByDawn (
  homeByDawn,
  HomeByDawn (..),
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
import Marvel.Query
import Marvel.Trait

homeByDawn :: ObligationCard HomeByDawn
homeByDawn = obligation HomeByDawn Cards.homeByDawn

newtype HomeByDawn = HomeByDawn (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage HomeByDawn where
  runMessage msg o@(HomeByDawn attrs) = runQueueT $ case msg of
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
        personas <- selectList $ SupportMatches [SupportControlledBy (IdentityWithId identityId), SupportWithTrait Persona]
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustKamalaKhanRemoveHomeByDawn" (exhaust identityId >> removeFromGame attrs)
          | isUnexhaustedAlterEgo
          ]
            <> [ labeled "$label.discardPersonaSupportDiscardObligation" $
                  if null personas
                    then gainSurge attrs
                    else
                      chooseOnePrompted identityId "$label.discardAPersonaSupport"
                        [targeting s (removeFromPlay s) | s <- personas]
               ]
        pure o
      _ -> HomeByDawn <$> runMessage msg attrs
    _ -> HomeByDawn <$> runMessage msg attrs
