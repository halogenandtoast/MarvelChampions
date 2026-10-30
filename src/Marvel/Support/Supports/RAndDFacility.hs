module Marvel.Support.Supports.RAndDFacility where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Effect.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

rAndDFacility :: SupportCard RAndDFacility
rAndDFacility = supportWith RAndDFacility Cards.rAndDFacility $
  (usesL .~ 3) . (discardIfNoUsesL .~ True)

newtype RAndDFacility = RAndDFacility (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RAndDFacility where
  getAbilities a = [ability a 1 HeroAction OwnsThis (ExhaustCost <> UseCost) (runAbility a 1)]

instance RunMessage RAndDFacility where
  runMessage msg a@(RAndDFacility attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      characters <- selectList (CharacterControlledBy AnyIdentity)
      chooseOne you
        [ targeting (characterRef character) $ case character of
            IdentityCharacter ident -> createEffect Cards.rAndDFacility attrs (IdentityEntity $ IdentityWithId ident)
            AllyCharacter ident -> createEffect Cards.rAndDFacility attrs (AllyEntity $ AllyWithId ident)
            _ -> pure ()
        | character <- characters
        ]
      pure a
    _ -> RAndDFacility <$> runMessage msg attrs

newtype RAndDFacilityEffect = RAndDFacilityEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

rAndDFacilityEffect :: CardEffect RAndDFacilityEffect
rAndDFacilityEffect = effectWith RAndDFacilityEffect Cards.rAndDFacility $
  (modifiersL .~ [ThwartModifier 1, AttackModifier 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage RAndDFacilityEffect where
  runMessage msg (RAndDFacilityEffect attrs) = RAndDFacilityEffect <$> runMessage msg attrs
