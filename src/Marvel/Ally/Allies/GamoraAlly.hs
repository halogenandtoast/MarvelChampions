module Marvel.Ally.Allies.GamoraAlly (
  gamoraAlly,
  GamoraAlly (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window hiding (AllyAttacked, AllyThwarted)
import Marvel.Window qualified as W

gamoraAlly :: AllyCard GamoraAlly
gamoraAlly = ally GamoraAlly Cards.gamoraAlly (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype GamoraAlly = GamoraAlly (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef, HasController)

-- | Hero Response: After Gamora attacks or thwarts, discard cards from the top of your
-- deck until you discard an event, then add that card to your hand.
instance HasAbilities GamoraAlly where
  getAbilities a@(GamoraAlly attrs) =
    [ limitedWindowAbility
        a
        1
        (WindowOneOf [W.AllyAttacked After self AnyEnemy, W.AllyThwarted After self AnyScheme])
        HeroResponse
        OwnsThis
        NoCost
        (runAbility a 1)
    ]
   where
    self = AllyWithId (allyId attrs)

instance RunMessage GamoraAlly where
  runMessage msg a@(GamoraAlly attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      discardFromDeckUntil ident (CardWithType EventType) a
      pure a
    WithDiscardedMatch (isTarget a -> True) _ (PlayerCard card) -> do
      addToHand (controller a) card
      pure a
    _ -> GamoraAlly <$> runMessage msg attrs
