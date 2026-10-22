module Marvel.Obligation.Obligations.OdinsAnger (
  odinsAnger,
  OdinsAnger (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ability.Types (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Projection
import Marvel.Query
import Marvel.Upgrade.Cards qualified as Upgrades

odinsAnger :: ObligationCard OdinsAnger
odinsAnger = obligation OdinsAnger Cards.odinsAnger

newtype OdinsAnger = OdinsAnger (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage OdinsAnger where
  runMessage msg o@(OdinsAnger attrs) = runQueueT $ case msg of
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
      -- the second choice may be made with no Mjolnir to discard: the stun is had
      -- anyway
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        inHand <- projectMap PlayerIdentityHand (filter (cardMatch $ CardIs Upgrades.mjolnir) . unHand) identityId
        inPlay <- selectList $ UpgradeIs Upgrades.mjolnir <> upgradeControlledBy identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustOdinsonRemoveOdinsAnger" (exhaust identityId >> removeFromGame attrs)
          | isUnexhaustedAlterEgo
          ]
            <> [ labeled "$label.discardMjolnirStunned" do
                  for_ inHand discardCard
                  for_ inPlay removeFromPlay
                  stunIdentity identityId
               ]
        pure o
      _ -> OdinsAnger <$> runMessage msg attrs
    _ -> OdinsAnger <$> runMessage msg attrs
