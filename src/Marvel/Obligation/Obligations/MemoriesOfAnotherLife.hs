module Marvel.Obligation.Obligations.MemoriesOfAnotherLife (
  memoriesOfAnotherLife,
  MemoriesOfAnotherLife (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Query ()

memoriesOfAnotherLife :: ObligationCard MemoriesOfAnotherLife
memoriesOfAnotherLife = obligation MemoriesOfAnotherLife Cards.memoriesOfAnotherLife

newtype MemoriesOfAnotherLife = MemoriesOfAnotherLife (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form. Choose: exhaust your alter-ego → remove this from the
-- game (only while a ready alter-ego: RR Choose (Option)); or you are stunned (already
-- stunned, it gains surge instead) and it is discarded.
instance RunMessage MemoriesOfAnotherLife where
  runMessage msg o@(MemoriesOfAnotherLife attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        canFlip <- identityMatches (HeroIdentity <> IdentityCanChangeForm) identityId
        when canFlip $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        MemoriesOfAnotherLife <$> runMessage msg attrs
      ResolveObligation identityId -> do
        readyAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        stunned <- identityMatches StunnedIdentity identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustDraxRemoveMemoriesOfAnotherLife" (exhaust identityId >> removeFromGame attrs)
          | readyAlterEgo
          ]
            <> [ labeled "$label.stunnedOrSurgeDiscardObligation" $
                  if stunned then gainSurge attrs else stunIdentity identityId
               ]
        pure o
      _ -> MemoriesOfAnotherLife <$> runMessage msg attrs
    _ -> MemoriesOfAnotherLife <$> runMessage msg attrs
