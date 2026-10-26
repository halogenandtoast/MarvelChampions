module Marvel.Ally.Allies.SpeedThomasShepherd (
  speedThomasShepherd,
  SpeedThomasShepherd (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window qualified as W

speedThomasShepherd :: AllyCard SpeedThomasShepherd
speedThomasShepherd = ally SpeedThomasShepherd Cards.speedThomasShepherd (Thw 2, 1) (Atk 1, 1) (HP 4)

newtype SpeedThomasShepherd = SpeedThomasShepherd (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Response: after Speed thwarts, ready him (limit once per round).
instance HasAbilities SpeedThomasShepherd where
  getAbilities (SpeedThomasShepherd a) =
    [ limited (PerRound 1) $
        limitedWindowAbility a 1 (W.AllyThwarted W.After (AllyWithId $ allyId a) AnyScheme) Response OwnsThis NoCost $
          runAbility a 1
    ]

instance RunMessage SpeedThomasShepherd where
  runMessage msg s@(SpeedThomasShepherd attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget s -> True) 1 _ _ -> do
      ready (allyId attrs)
      pure s
    _ -> SpeedThomasShepherd <$> runMessage msg attrs
