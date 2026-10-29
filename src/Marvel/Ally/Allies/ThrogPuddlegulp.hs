module Marvel.Ally.Allies.ThrogPuddlegulp (throgPuddlegulp, ThrogPuddlegulp (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Window

throgPuddlegulp :: AllyCard ThrogPuddlegulp
throgPuddlegulp = ally ThrogPuddlegulp Cards.throgPuddlegulp (Thw 1, 1) (Atk 2, 1) (HP 2)

newtype ThrogPuddlegulp = ThrogPuddlegulp (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ThrogPuddlegulp where
  getAbilities a@(ThrogPuddlegulp attrs) =
    [ limitedWindowAbility a 1 EntersPlayThis Response
        (OwnsThis <> MinionExists (MinionEngagedWith $ IdentityWithId $ allyController attrs))
        NoCost (runAbility a 1)
    ]

instance RunMessage ThrogPuddlegulp where
  runMessage msg a@(ThrogPuddlegulp attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      whenM (selectAny $ MinionEngagedWith $ IdentityWithId $ allyController attrs) $
        giveStatus (allyId attrs) ToughStatus
      pure a
    _ -> ThrogPuddlegulp <$> runMessage msg attrs
