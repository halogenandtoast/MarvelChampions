module Marvel.Obligation.Obligations.HomeByDawn (
  homeByDawn,
  HomeByDawn (..),
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

homeByDawn :: ObligationCard HomeByDawn
homeByDawn = obligation HomeByDawn Cards.homeByDawn

newtype HomeByDawn = HomeByDawn (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage HomeByDawn where
  runMessage msg o@(HomeByDawn attrs) = case msg of
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
        personas <- selectList $ SupportMatches [SupportControlledBy (IdentityWithId identityId), SupportWithTrait Persona]
        chooseOrRunOne identityId $
          [ Label
            "$label.exhaustKamalaKhanRemoveHomeByDawn"
            [Run [IdentityMessage identityId ExhaustedIdentity, RemoveFromGame (toTarget attrs)]]
          | isUnexhaustedAlterEgo
          ]
            <> [ Label "$label.discardPersonaSupportDiscardObligation" $
                  if null personas
                    then [Run [GainSurge (toTarget attrs)]]
                    else
                      [ Run
                          [ Ask identityId $
                              prompted "$label.discardAPersonaSupport" $
                                ChooseOne [TargetLabel (toRef s) [DiscardTarget (toRef s)] | s <- personas]
                          ]
                      ]
               ]
        pure o
      _ -> HomeByDawn <$> runMessage msg attrs
    _ -> HomeByDawn <$> runMessage msg attrs
