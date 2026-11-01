module Marvel.Obligation.Obligations.Homesick where

import Marvel.Prelude
import Control.Monad (replicateM_)
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Identity.Types (Field (PlayerIdentityTough, PlayerIdentityAdditionalTough))

homesick :: ObligationCard Homesick
homesick = obligation Homesick Cards.homesick

newtype Homesick = Homesick (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Homesick where
  runMessage msg e@(Homesick attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      hero <- identityMatches HeroIdentity you
      when hero $ chooseOne you
        [ labeled "$label.doNotFlipToAlterEgo" (pure ())
        , labeled "$label.changeToB" (changeToAlterEgo you)
        ]
      resolveObligation oid you
      pure $ Homesick attrs {obligationOwner = Just you}
    ObligationMessage oid (ResolveObligation you) | oid == obligationId attrs -> do
      ae <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      chooseOne you $
        [labeled "$label.exhaustPiotrRemoveHomesick" (exhaust you >> removeFromGame attrs) | ae]
        <> [labeled "$label.homesickDiscardTough" (cardSpecific attrs "discardTough" you)]
      pure e
    CardSpecific (isTarget attrs -> True) "discardTough" (fromJSON -> Success you) -> do
      tough <- project PlayerIdentityTough you
      additional <- project PlayerIdentityAdditionalTough you
      let n = (if tough then 1 else 0) + additional
      replicateM_ (fromIntegral n) (removeStatus you ToughStatus)
      removeFromPlay attrs
      when (n == 0) $ surge you
      pure e
    _ -> Homesick <$> runMessage msg attrs
