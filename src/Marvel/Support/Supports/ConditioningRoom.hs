module Marvel.Support.Supports.ConditioningRoom (
  conditioningRoom,
  ConditioningRoom (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait qualified as Trait

conditioningRoom :: SupportCard ConditioningRoom
conditioningRoom = support ConditioningRoom Cards.conditioningRoom

newtype ConditioningRoom = ConditioningRoom (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

attackOrThwartEvent :: CardMatcher
attackOrThwartEvent = CardWithType EventType <> CardMatchesAny [CardWithTrait Trait.Attack, CardWithTrait Trait.Thwart]

-- | Alter-Ego Action: exhaust → return the bottommost Attack or Thwart event from your
-- discard pile to your hand, and heal 1 damage from Gamora. Only with such an event there.
instance HasAbilities ConditioningRoom where
  getAbilities a@(ConditioningRoom attrs) =
    [ ability
        a
        1
        AlterEgoAction
        ( OwnsThis
            <> ExtendedCardExists
              (InDiscardOf (IdentityWithId $ supportController attrs) (BasicCardMatches attackOrThwartEvent))
        )
        ExhaustCost
        (runAbility a 1)
    ]

-- | The discard pile's head is its top, so the bottommost is the last match. "Gamora" is
-- the controller's identity in either form (hero-gamora.md).
instance RunMessage ConditioningRoom where
  runMessage msg s@(ConditioningRoom attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch attackOrThwartEvent) <$> projectMap PlayerIdentityDiscard unDiscard ident
      for_ (lastMay cards) (addToHand ident)
      heal ident 1
      pure s
    _ -> ConditioningRoom <$> runMessage msg attrs
