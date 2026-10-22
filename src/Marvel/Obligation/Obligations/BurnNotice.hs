module Marvel.Obligation.Obligations.BurnNotice (
  burnNotice,
  BurnNotice (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Trait

burnNotice :: ObligationCard BurnNotice
burnNotice = obligation BurnNotice Cards.burnNotice

newtype BurnNotice = BurnNotice (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage BurnNotice where
  runMessage msg o@(BurnNotice attrs) = case msg of
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
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        -- the highest cost may be shared: the player chooses among those
        highest <-
          selectRefs $
            UpgradeWithHighestCostAmong
              (UpgradeWithTrait Preparation <> UpgradeControlledBy (IdentityWithId identityId))
        chooseOrRunOne identityId $
          [ Label
            "$label.exhaustNatashaRomanoffRemoveBurnNotice"
            [Run [IdentityMessage identityId ExhaustedIdentity, RemoveFromGame (toTarget attrs)]]
          | isUnexhaustedAlterEgo
          ]
            <> [ Label
                  "$label.discardHighestCostPreparationDiscardObligation"
                  [ Run
                      [ if null highest
                          then GainSurge (toTarget attrs)
                          else Ask identityId $ ChooseOne [TargetLabel target [DiscardTarget target] | target <- highest]
                      ]
                  ]
               ]
        pure o
      _ -> BurnNotice <$> runMessage msg attrs
    _ -> BurnNotice <$> runMessage msg attrs
