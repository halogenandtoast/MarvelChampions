module Marvel.Support.Supports.AvengersTower (
  avengersTower,
  avengersTowerEffect,
  AvengersTower (..),
  AvengersTowerEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait

avengersTower :: SupportCard AvengersTower
avengersTower = support AvengersTower Cards.avengersTower

newtype AvengersTower = AvengersTower (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | One more ally while each of the player's allies is an Avenger.
instance HasModifiersFor AvengersTower where
  getModifiersFor _ (IdentityRef ident) (AvengersTower a) | ident == supportController a = do
    let yours = AllyControlledBy (IdentityWithId ident)
    others <- selectAny $ AllyMatches [yours, NotAlly (AllyWithTrait Avenger)]
    pure [AllyLimitModifier 1 | not others]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AvengersTower where
  getAbilities (AvengersTower a) =
    [ ability a 1 Action OwnsThis ExhaustCost $
        Run [CreatedEffect Cards.avengersTower (toSource a) (IdentityEntity $ IdentityWithId $ supportController a)]
    ]

instance RunMessage AvengersTower where
  runMessage msg (AvengersTower a) = AvengersTower <$> runMessage msg a

avengerAlly :: CardMatcher
avengerAlly = CardWithType AllyType <> CardWithTrait Avenger

-- | The next Avenger ally the player plays this phase costs 1 less.
newtype AvengersTowerEffect = AvengersTowerEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

avengersTowerEffect :: CardEffect AvengersTowerEffect
avengersTowerEffect =
  effectWith AvengersTowerEffect Cards.avengersTower $
    (modifiersL .~ [ResourceCostReductionFor avengerAlly 1])
      . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage AvengersTowerEffect where
  runMessage msg e@(AvengersTowerEffect attrs) = runQueueT $ case msg of
    IdentityMessage ident (PlayedCard card _) | cardMatch avengerAlly card -> do
      whenM (effectValidFor attrs (toRef ident)) $ disableEffect (effectId attrs)
      pure e
    _ -> AvengersTowerEffect <$> runMessage msg attrs
