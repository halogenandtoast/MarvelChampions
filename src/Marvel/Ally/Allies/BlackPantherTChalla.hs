module Marvel.Ally.Allies.BlackPantherTChalla (blackPantherTChalla, BlackPantherTChalla (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Aspect (Aspect (Leadership))
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Window

blackPantherTChalla :: AllyCard BlackPantherTChalla
blackPantherTChalla = ally BlackPantherTChalla Cards.blackPantherTChalla (Thw 2, 2) (Atk 2, 2) (HP 4)

newtype BlackPantherTChalla = BlackPantherTChalla (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

leadershipEvents :: Attrs Ally -> ExtendedCardMatcher
leadershipEvents attrs = InDiscardOf (IdentityWithId $ allyController attrs) $
  BasicCardMatches (CardWithType EventType <> CardWithAspect Leadership)

instance HasAbilities BlackPantherTChalla where
  getAbilities a@(BlackPantherTChalla attrs) =
    [limitedWindowAbility a 1 EntersPlayThis Response
      (OwnsThis <> ExtendedCardExists (leadershipEvents attrs)) NoCost (runAbility a 1)]

instance RunMessage BlackPantherTChalla where
  runMessage msg a@(BlackPantherTChalla attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (leadershipEvents attrs)
      unless (null cards) $ chooseOne ident
        [targeting c (attachPlayableEventToAlly (allyId attrs) c) | c <- cards]
      pure a
    _ -> BlackPantherTChalla <$> runMessage msg attrs
