module Marvel.Obligation.Obligations.Wilt (
  wilt,
  Wilt (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Card
import Marvel.Entity
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection

wilt :: ObligationCard Wilt
wilt = obligation Wilt Cards.wilt

newtype Wilt = Wilt (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form. Choose: exhaust your alter-ego → remove this from the
-- game (only while a ready alter-ego); or remove 3 growth counters from Groot (as many as
-- he has, up to 3) — with none removed this way it gains surge — and discard it.
instance RunMessage Wilt where
  runMessage msg o@(Wilt attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        canFlip <- identityMatches (HeroIdentity <> IdentityCanChangeForm) identityId
        when canFlip $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        Wilt <$> runMessage msg attrs
      ResolveObligation identityId -> do
        readyAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        counters <- project PlayerIdentityCounters identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustGrootRemoveWilt" (exhaust identityId >> removeFromGame attrs)
          | readyAlterEgo
          ]
            <> [ labeled (countLabel "removeGrowthCounters" 3) $
                  if counters == 0 then gainSurge attrs else removeCounters identityId 3
               ]
        pure o
      _ -> Wilt <$> runMessage msg attrs
    _ -> Wilt <$> runMessage msg attrs
