module Marvel.Ally.Allies.Ironheart (
  ironheart,
  Ironheart (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Def ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window

ironheart :: AllyCard Ironheart
ironheart = ally Ironheart Cards.ironheart (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Ironheart = Ironheart (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Response: After you play Ironheart from your hand, draw 1 card.
instance HasAbilities Ironheart where
  getAbilities a@(Ironheart attrs) =
    [ limitedWindowAbility a 1 (PlaysCard After (IdentityWithId $ allyController attrs) (CardIs Cards.ironheart)) Response OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage Ironheart where
  runMessage msg a@(Ironheart attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      drawCards ident 1
      pure a
    _ -> Ironheart <$> runMessage msg attrs
