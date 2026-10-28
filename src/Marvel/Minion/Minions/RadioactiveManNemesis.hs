module Marvel.Minion.Minions.RadioactiveManNemesis (
  radioactiveManNemesis,
  RadioactiveManNemesis (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

radioactiveManNemesis :: MinionCard RadioactiveManNemesis
radioactiveManNemesis = minion RadioactiveManNemesis Cards.radioactiveManNemesis (Sch 1) (Atk 1) (HP 6)

newtype RadioactiveManNemesis = RadioactiveManNemesis (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Response: after Radioactive Man activates against you (attacks you, or schemes
-- while engaged with you), deal 1 damage to each character you control.
instance HasAbilities RadioactiveManNemesis where
  getAbilities (RadioactiveManNemesis a) =
    [ windowAbility a 1 (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity) ForcedResponse NoCost
        $ RunAbility (toTarget a) 1
    , windowAbility a 2 (MinionHasSchemed After (MinionWithId $ minionId a)) ForcedResponse NoCost
        $ RunAbility (toTarget a) 2
    ]

-- | [star] Boost: deal 1 damage to each character you control.
instance RunMessage RadioactiveManNemesis where
  runMessage msg e@(RadioactiveManNemesis attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttack _ you] _ -> do
      irradiate you
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      irradiate (minionEngagedIdentity attrs)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      irradiate =<< getBoostedPlayer enemy
      pure e
    _ -> RadioactiveManNemesis <$> runMessage msg attrs
   where
    irradiate you = damageAllCharacters attrs (toDamage 1 FromAbility) (CharacterControlledBy $ IdentityWithId you)
