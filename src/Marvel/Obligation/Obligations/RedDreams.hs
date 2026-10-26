module Marvel.Obligation.Obligations.RedDreams (
  redDreams,
  RedDreams (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Resource

redDreams :: ObligationCard RedDreams
redDreams = obligation RedDreams Cards.redDreams

newtype RedDreams = RedDreams (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form. Choose: exhaust Nadia Van Dyne → remove Red Dreams
-- from the game; or discard each card with a printed [mental] resource from your hand
-- and take 1 damage, and discard this obligation.
instance RunMessage RedDreams where
  runMessage msg o@(RedDreams attrs) = runQueueT $ case msg of
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
        canExhaust <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        -- printed: a wild resource is not a [mental] one
        mental <- projectMap PlayerIdentityHand (filter (cardMatch $ CardWithResource Mental) . unHand) identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustNadiaVanDyneRemoveRedDreams" (exhaust identityId >> removeFromGame attrs)
          | canExhaust
          ]
            <> [ labeled "$label.discardMentalCardsTakeDamage" do
                  for_ mental discardCard
                  damageIdentity identityId attrs (toDamage 1 FromAbility)
               ]
        pure o
      _ -> RedDreams <$> runMessage msg attrs
    _ -> RedDreams <$> runMessage msg attrs
