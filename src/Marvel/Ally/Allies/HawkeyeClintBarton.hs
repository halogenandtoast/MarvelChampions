module Marvel.Ally.Allies.HawkeyeClintBarton where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers
import Marvel.Window

hawkeyeClintBarton :: AllyCard HawkeyeClintBarton
hawkeyeClintBarton =
  allyWith
    HawkeyeClintBarton
    Cards.hawkeyeClintBarton
    (Thw 1, 1)
    (Atk 1, 1)
    (HP 3)
    (countersL .~ 4)

newtype HawkeyeClintBarton = HawkeyeClintBarton (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities HawkeyeClintBarton where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (MinionEntersPlay After AnyMinion)
        Response
        OwnsThis
        UseCost
        $ runAbility a 1
    ]

findMinion :: [WindowType] -> MinionId
findMinion = \case
  [] -> error "Invalid Call"
  MinionEnteredPlay minionId : _ -> minionId
  (_ : xs) -> findMinion xs

instance RunMessage HawkeyeClintBarton where
  runMessage msg x@(HawkeyeClintBarton a) = case msg of
    RanAbility _ (isTarget x -> True) 1 (findMinion -> minionId) _ -> do
      push . MinionMessage minionId $
        MinionDamaged
          (toRef x)
          (toDamage 2 FromAbility)
      pure x
    _ -> HawkeyeClintBarton <$> runMessage msg a
