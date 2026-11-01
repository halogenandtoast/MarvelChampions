module Marvel.Ally.Allies.Pixie where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity ()
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Modifier ()
import Marvel.Query
import Marvel.Ref ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

pixie :: AllyCard Pixie
pixie = ally Pixie Cards.pixie (Thw 1, 1) (Atk 2, 1) (HP 2)

newtype Pixie = Pixie (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

pixieAllies :: Attrs Ally -> ExtendedCardMatcher
pixieAllies attrs = InDiscardOf (IdentityWithId $ allyController attrs)
  (BasicCardMatches $ CardWithType AllyType <> CardWithTrait Trait.XMen)

instance HasAbilities Pixie where
  getAbilities (Pixie a) =
    [limitedWindowAbility a 1 (W.PlayedFromHand (IdentityWithId $ allyController a) $ CardIs Cards.pixie)
      Response (OwnsThis <> ExtendedCardExists (pixieAllies a)) NoCost (runAbility a 1)]

instance RunMessage Pixie where
  runMessage msg a@(Pixie attrs) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      cards <- selectList (pixieAllies attrs)
      chooseOne you [targeting card (addToHand you card) | card <- cards]
      pure a
    _ -> Pixie <$> runMessage msg attrs
