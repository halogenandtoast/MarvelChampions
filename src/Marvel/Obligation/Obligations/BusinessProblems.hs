module Marvel.Obligation.Obligations.BusinessProblems (
  businessProblems,
  BusinessProblems (..),
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

businessProblems :: ObligationCard BusinessProblems
businessProblems = obligation BusinessProblems Cards.businessProblems

newtype BusinessProblems = BusinessProblems (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage BusinessProblems where
  runMessage msg o@(BusinessProblems attrs) = case msg of
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
            <> [ ObligationMessage (obligationId attrs) $
                  ResolveObligation identityId
               ]
        pure o
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <-
          identityMatches
            (AlterEgoIdentity <> UnexhaustedIdentity)
            identityId
        ironManUpgrades <-
          selectList $
            UpgradeControlledBy (IdentityWithId identityId)
        chooseOrRunOne identityId $
          [ Label
            "$label.exhaustTonyStarkRemoveBusinessProblems"
            [ Run
                [ IdentityMessage identityId ExhaustedIdentity
                , RemoveFromGame (toTarget attrs)
                ]
            ]
          | isUnexhaustedAlterEgo
          ]
            <> [ Label
                  "$label.exhaustEachUpgradeDiscardObligation"
                  [ Run
                      [ UpgradeMessage u ExhaustedUpgrade
                      | u <- ironManUpgrades
                      ]
                  ]
               ]
        pure o
      _ -> BusinessProblems <$> runMessage msg attrs
    _ -> BusinessProblems <$> runMessage msg attrs
