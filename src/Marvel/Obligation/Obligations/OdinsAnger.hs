module Marvel.Obligation.Obligations.OdinsAnger (
  odinsAnger,
  OdinsAnger (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Choice
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Upgrades

odinsAnger :: ObligationCard OdinsAnger
odinsAnger = obligation OdinsAnger Cards.odinsAnger

newtype OdinsAnger = OdinsAnger (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage OdinsAnger where
  runMessage msg o@(OdinsAnger attrs) = case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        pushAll $
          [ Ask identityId $
            ChooseOne
              [ Label "$label.doNotFlipToAlterEgo" []
              , Label "$label.changeToB" [ChangeForm]
              ]
          | isHero
          ]
            <> [ObligationMessage (obligationId attrs) $ ResolveObligation identityId]
        pure o
      -- the second choice may be made with no Mjolnir to discard: the stun is had
      -- anyway
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        inHand <- projectMap PlayerIdentityHand (filter (cardMatch $ CardIs Upgrades.mjolnir) . unHand) identityId
        inPlay <- selectList $ UpgradeIs Upgrades.mjolnir <> upgradeControlledBy identityId
        chooseOrRunOne identityId $
          [ Label
            "$label.exhaustOdinsonRemoveOdinsAnger"
            [Run [IdentityMessage identityId ExhaustedIdentity, RemoveFromGame (toTarget attrs)]]
          | isUnexhaustedAlterEgo
          ]
            <> [ Label
                  "$label.discardMjolnirStunned"
                  [ Run $
                      map (DiscardedCard . PlayerCard) inHand
                        <> map (RemoveFromPlay . toTarget) inPlay
                        <> [IdentityMessage identityId IdentityStunned]
                  ]
               ]
        pure o
      _ -> OdinsAnger <$> runMessage msg attrs
    _ -> OdinsAnger <$> runMessage msg attrs
