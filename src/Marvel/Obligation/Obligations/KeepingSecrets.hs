module Marvel.Obligation.Obligations.KeepingSecrets where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Id (SupportId (..))
import Marvel.Card.Id
import Marvel.Card.PlayerCard.Types (PlayerCard (..))
import Marvel.Discard
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Projection
import Marvel.Query
import Marvel.Support.Types (Field (SupportCardDef))

keepingSecrets :: ObligationCard KeepingSecrets
keepingSecrets = obligation KeepingSecrets Cards.keepingSecrets

newtype KeepingSecrets = KeepingSecrets (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage KeepingSecrets where
  runMessage msg o@(KeepingSecrets attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      hero <- identityMatches HeroIdentity you
      when hero $ chooseOne you
        [ labeled "$label.doNotFlipToAlterEgo" (pure ())
        , labeled "$label.changeToB" (changeToAlterEgo you) ]
      resolveObligation (obligationId attrs) you
      pure o
    ObligationMessage oid (ResolveObligation you) | oid == obligationId attrs -> do
      readyAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      chooseOrRunOne you $
        [labeled "$label.exhaustMilesMoralesRemoveKeepingSecrets" (exhaust you >> removeFromGame attrs) | readyAlterEgo]
          <> [labeled "$label.discardGankeLeeAndJeffersonDavis" do
                ganke <- selectList (SupportMatches [SupportControlledBy (IdentityWithId you), SupportNamed "Ganke Lee"])
                jefferson <- selectList (SupportMatches [SupportControlledBy (IdentityWithId you), SupportNamed "Jefferson Davis"])
                let supports = ganke <> jefferson
                for_ supports removeFromPlay
                cardSpecific attrs "checkDiscardedSupports" (you, map (CardId . unSupportId) supports)]
      pure o
    CardSpecific (isTarget attrs -> True) "checkDiscardedSupports" (fromJSON -> Success (you, supportCards :: [CardId])) -> do
      discarded <- projectMap PlayerIdentityDiscard unDiscard you
      unless (any ((`elem` supportCards) . pcCardId) discarded) (gainSurge attrs)
      pure o
    _ -> KeepingSecrets <$> runMessage msg attrs
