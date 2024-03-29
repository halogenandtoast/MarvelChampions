module Marvel.Support.Supports.StarkTower (
  starkTower,
  StarkTower (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait

starkTower :: SupportCard StarkTower
starkTower = support StarkTower Cards.starkTower

newtype StarkTower = StarkTower (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities StarkTower where
  getAbilities (StarkTower a) =
    [ ability
        a
        1
        AlterEgoAction
        ( OwnsThis
            <> ExtendedCardExists
              (TopmostCardInDiscardOf AnyIdentity (CardWithTrait Tech))
        )
        ExhaustCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage StarkTower where
  runMessage msg s@(StarkTower attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      pushChoice ident $ ChoosePlayer AnyIdentity (toRef s)
      pure s
    ChosePlayer identityId target | isTarget attrs target -> do
      mTechCard <-
        selectOne $
          TopmostCardInDiscardOf
            (IdentityWithId identityId)
            (CardWithTrait Tech)
      case mTechCard of
        Nothing -> error "should have targetted a card"
        Just x -> push $ IdentityMessage identityId (AddToHand x)
      pure s
    _ -> StarkTower <$> runMessage msg attrs
