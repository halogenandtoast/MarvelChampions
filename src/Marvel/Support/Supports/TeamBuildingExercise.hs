module Marvel.Support.Supports.TeamBuildingExercise (
  teamBuildingExercise,
  teamBuildingExerciseEffect,
  TeamBuildingExercise (..),
  TeamBuildingExerciseEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Id (CharacterId (IdentityCharacter))
import Marvel.Game.Source (getCharacterTraits)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

teamBuildingExercise :: SupportCard TeamBuildingExercise
teamBuildingExercise = support TeamBuildingExercise Cards.teamBuildingExercise

newtype TeamBuildingExercise = TeamBuildingExercise (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Team-Building Exercise → play a card from your hand that shares
-- a trait with your hero, reducing its resource cost by 1.
instance HasAbilities TeamBuildingExercise where
  getAbilities a = [ability a 1 HeroAction (OwnsThis <> PlayCardSharingIdentityTraitWithReductionHasTarget 1) ExhaustCost $ runAbility a 1]

-- | The reduction is an effect on the player, gone once they play a card; which cards
-- may be played is asked once it is there, so a card it makes affordable is offered.
instance RunMessage TeamBuildingExercise where
  runMessage msg s@(TeamBuildingExercise attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      createEffect Cards.teamBuildingExercise attrs (IdentityEntity $ IdentityWithId you)
      cardSpecific attrs "play" you
      pure s
    CardSpecific (isTarget attrs -> True) "play" (fromJSON -> Success you) -> do
      traits <- toList <$> getCharacterTraits (IdentityCharacter you)
      cards <-
        selectList $
          PlayableFromHandOf (IdentityWithId you) $
            AffordableCardBy (IdentityWithId you) $
              BasicCardMatches (CardMatchesAny $ map CardWithTrait traits)
      chooseOne you $
        labeled "$label.done" (cardSpecific attrs "done" ())
          : [pure (PlayCard c Nothing) | notNull traits, c <- cards]
      pure s
    _ -> TeamBuildingExercise <$> runMessage msg attrs

newtype TeamBuildingExerciseEffect = TeamBuildingExerciseEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

teamBuildingExerciseEffect :: CardEffect TeamBuildingExerciseEffect
teamBuildingExerciseEffect =
  effectWith TeamBuildingExerciseEffect Cards.teamBuildingExercise $
    (modifiersL .~ [ResourceCostReduction 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage TeamBuildingExerciseEffect where
  runMessage msg e@(TeamBuildingExerciseEffect attrs) = runQueueT $ case msg of
    IdentityMessage ident (PlayedCard _ _) -> do
      whenM (effectValidFor attrs (toRef ident)) $ disableEffect (effectId attrs)
      pure e
    -- no card played after all
    CardSpecific target "done" _ | target == refocus (effectSource attrs) -> do
      disableEffect (effectId attrs)
      pure e
    _ -> TeamBuildingExerciseEffect <$> runMessage msg attrs
