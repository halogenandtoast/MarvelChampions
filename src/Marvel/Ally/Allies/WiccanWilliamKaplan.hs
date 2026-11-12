module Marvel.Ally.Allies.WiccanWilliamKaplan (
  wiccanWilliamKaplan,
  WiccanWilliamKaplan (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window qualified as W

wiccanWilliamKaplan :: AllyCard WiccanWilliamKaplan
wiccanWilliamKaplan = ally WiccanWilliamKaplan Cards.wiccanWilliamKaplan (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype WiccanWilliamKaplan = WiccanWilliamKaplan (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Response: after Wiccan thwarts, discard the top card of the encounter deck; for
-- each boost icon discarded, deal 1 damage to an enemy. With no "choose" in it, the
-- "for each" is one enemy taking one instance of damage (RR "For Each").
instance HasAbilities WiccanWilliamKaplan where
  getAbilities (WiccanWilliamKaplan a) =
    [ limitedWindowAbility a 1 (W.AllyThwarted W.After (AllyWithId $ allyId a) AnyScheme) Response OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage WiccanWilliamKaplan where
  runMessage msg w@(WiccanWilliamKaplan attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget w -> True) 1 _ _ -> do
      discardTopOfEncounterDeckThen 1 (countBoostIcons attrs ())
      pure w
    WithBoostIcons (isTarget attrs -> True) _ (sum . map snd -> n) -> do
      when (n > 0) $ chooseDamage (allyController attrs) attrs (toDamage n FromAbility) AnyEnemy
      pure w
    _ -> WiccanWilliamKaplan <$> runMessage msg attrs
