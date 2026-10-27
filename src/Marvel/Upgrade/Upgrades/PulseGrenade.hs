module Marvel.Upgrade.Upgrades.PulseGrenade (
  pulseGrenade,
  PulseGrenade (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

pulseGrenade :: UpgradeCard PulseGrenade
pulseGrenade = upgrade PulseGrenade Cards.pulseGrenade

newtype PulseGrenade = PulseGrenade (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): Discard Pulse Grenade and choose an enemy → discard the top 2
-- cards of the encounter deck. Deal 1 damage to the chosen enemy for each boost icon
-- discarded this way.
instance HasAbilities PulseGrenade where
  getAbilities a =
    [ subtype Ability.Attack $
        ability a 1 HeroAction (OwnsThis <> canAttack) (DiscardCost $ toTarget a) $
          runAbility a 1
    ]

instance RunMessage PulseGrenade where
  runMessage msg u@(PulseGrenade attrs) = runQueueT $ case msg of
    -- an attack: guard keeps the villain safe
    RanAbility you (isTarget attrs -> True) 1 _ _ -> attackGuard you u do
      enemies <- selectList $ AttackableBy (IdentityWithId you)
      chooseOne you
        [ targeting (enemyRef enemy) $ discardTopOfEncounterDeckThen 2 (countBoostIcons attrs (you, enemy))
        | enemy <- enemies
        ]
      pure u
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success (you, enemy :: EnemyId)) (sum . map snd -> n) -> do
      -- one instance of n, an attack even at 0: retaliate and "after you attack" answer it
      dealDamage enemy attrs (toDamage n $ FromPlayerAttack you)
      pure u
    _ -> PulseGrenade <$> runMessage msg attrs
