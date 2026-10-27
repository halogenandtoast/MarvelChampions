module Marvel.Ally.Allies.MartyrPhylaVell (
  martyrPhylaVell,
  MartyrPhylaVell (..),
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

martyrPhylaVell :: AllyCard MartyrPhylaVell
martyrPhylaVell = ally (MartyrPhylaVell . (`With` Meta False)) Cards.martyrPhylaVell (Thw 1, 1) (Atk 2, 1) (HP 3)

-- | Whether the attack she is making has defeated an enemy.
newtype Meta = Meta {defeatedEnemy :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MartyrPhylaVell = MartyrPhylaVell (Attrs Ally `With` Meta)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | Offered only while the attack she is performing has defeated an enemy.
instance HasAbilities MartyrPhylaVell where
  getAbilities (MartyrPhylaVell (a `With` meta)) =
    [ limitedWindowAbility a 1 (W.AllyTookDamageOf (AllyWithId $ allyId a) FromConsequential) Response OwnsThis NoCost $
        runAbility a 1
    | defeatedEnemy meta
    ]

instance IsAlly MartyrPhylaVell where
  toAllyAttrs (MartyrPhylaVell (a `With` _)) = a

instance IsRef MartyrPhylaVell where
  toRef = toRef . toAllyAttrs

-- | Response: After Martyr takes consequential damage from performing an attack, if that
-- attack defeated an enemy, give her a tough status card. Her attack's damage defeats the
-- enemy before she takes its consequential damage; a basic thwart, whose consequential
-- damage this is not, starts afresh.
instance RunMessage MartyrPhylaVell where
  runMessage msg (MartyrPhylaVell (attrs `With` meta)) = runQueueT $ case msg of
    AllyMessage aid m
      | aid == self
      , isStart m ->
          MartyrPhylaVell . (`With` Meta False) <$> runMessage msg attrs
    CheckWindows ws
      | or [fromHer dmg | W.Window W.After (W.DefeatedMinion _ dmg _ _) <- ws]
          || or [fromHer dmg | W.Window W.After (W.DefeatedVillain _ dmg _ _) <- ws] ->
          MartyrPhylaVell . (`With` Meta True) <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      giveStatus self ToughStatus
      MartyrPhylaVell . (`With` Meta False) <$> runMessage msg attrs
    _ -> MartyrPhylaVell . (`With` meta) <$> runMessage msg attrs
   where
    self = allyId attrs
    fromHer dmg = damageSource dmg == FromAllyAttack self
    isStart = \case
      AllyAttacked -> True
      AllyThwarted -> True
      _ -> False
