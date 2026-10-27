module Marvel.Ally.Allies.StarhawkStakarOgord (
  starhawkStakarOgord,
  StarhawkStakarOgord (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Window qualified as W

starhawkStakarOgord :: AllyCard StarhawkStakarOgord
starhawkStakarOgord = ally StarhawkStakarOgord Cards.starhawkStakarOgord (Thw 1, 1) (Atk 2, 2) (HP 3)

newtype StarhawkStakarOgord = StarhawkStakarOgord (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Interrupt: When Starhawk takes damage exactly equal to his remaining hit points,
-- return him to your hand. Checked again as it resolves, against the damage still
-- pending (another interrupt may have changed it).
instance HasAbilities StarhawkStakarOgord where
  getAbilities a@(StarhawkStakarOgord attrs) =
    [ limitedWindowAbility
        a
        1
        (W.AllyWouldTakeDamageEqualToRemainingHitPoints $ AllyWithId $ allyId attrs)
        Interrupt
        OwnsThis
        NoCost
        (runAbility a 1)
    ]

instance RunMessage StarhawkStakarOgord where
  runMessage msg a@(StarhawkStakarOgord attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      remaining <- subtractNatural (allyDamage attrs) <$> getModifiedHitPoints attrs
      -- a tough status card would prevent the damage: he takes none
      let returns dmg = damageAmount dmg == remaining && not (allyTough attrs && preventedByTough dmg)
      replaceMatchingMessage
        ( \case
            AllyMessage _ (AllyDamaged _ dmg) | returns dmg -> [ReturnToHand (toTarget a)]
            m -> [m]
        )
        \case
          AllyMessage aid (AllyDamaged _ _) -> aid == allyId attrs
          _ -> False
      pure a
    _ -> StarhawkStakarOgord <$> runMessage msg attrs
