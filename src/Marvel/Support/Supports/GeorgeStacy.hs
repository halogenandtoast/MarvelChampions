module Marvel.Support.Supports.GeorgeStacy where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

georgeStacy :: SupportCard GeorgeStacy
georgeStacy = supportWith GeorgeStacy Cards.georgeStacy (\a -> a {supportCanPlayHeldEvents = True})

newtype GeorgeStacy = GeorgeStacy (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GeorgeStacy where
  getAbilities a@(GeorgeStacy attrs) =
    [ ability a 1 Action
        (OwnsThis <> ExtendedCardExists (InHandOf You $ BasicCardMatches $ CardWithType EventType))
        ExhaustCost (runAbility a 1)
    | length (supportHeldCards attrs) < 3
    ]

instance RunMessage GeorgeStacy where
  runMessage msg s@(GeorgeStacy attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch $ CardWithType EventType) <$> projectMap PlayerIdentityHand unHand you
      when (length (supportHeldCards attrs) < 3 && notNull cards) $
        chooseOne you [targeting c (attachPlayableEventToSupport (supportId attrs) c) | c <- cards]
      pure s
    _ -> GeorgeStacy <$> runMessage msg attrs
