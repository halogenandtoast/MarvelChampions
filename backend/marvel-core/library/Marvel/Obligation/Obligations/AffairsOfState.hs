module Marvel.Obligation.Obligations.AffairsOfState
  ( affairsOfState
  , AffairsOfState(..)
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
import Marvel.Source
import Marvel.Target
import Marvel.Trait

affairsOfState :: ObligationCard AffairsOfState
affairsOfState = obligation AffairsOfState Cards.affairsOfState

newtype AffairsOfState = AffairsOfState (Attrs Obligation)
  deriving anyclass IsObligation
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage AffairsOfState where
  runMessage msg o@(AffairsOfState attrs) = case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        pushAll
          $ [ Ask identityId $ ChooseOne
                [ Label "Do not flip to alter-ego" []
                , Label "Flip to alter-ego" [ChangeForm]
                ]
            | isHero
            ]
          <> [ ObligationMessage (obligationId attrs)
                 $ ResolveObligation identityId
             ]
        pure o
      ResolveObligation identityId -> do
        isUnexhaustedAlterEgo <- identityMatches
          (AlterEgoIdentity <> UnexhaustedIdentity)
          identityId
        blackPantherUpgrades <-
          selectMap UpgradeTarget
          $ UpgradeWithTrait BlackPanther
          <> UpgradeControlledBy (IdentityWithId identityId)
        chooseOrRunOne identityId
          $ [ Label
                "Exhaust T'Challa -> remove Affairs of State from the game."
                [ Run
                    [ IdentityMessage identityId ExhaustedIdentity
                    , RemoveFromGame (toTarget attrs)
                    ]
                ]
            | isUnexhaustedAlterEgo
            ]
          <> [ Label
                 "Choose and discard a Black Panther upgrade you control. Discard this obligation"
                 [ Run
                     [ Ask identityId $ ChooseOne
                         [ TargetLabel target [DiscardTarget target]
                         | target <- blackPantherUpgrades
                         ]
                     ]
                 ]
             ]
        pure o
      _ -> AffairsOfState <$> runMessage msg attrs
    _ -> AffairsOfState <$> runMessage msg attrs
