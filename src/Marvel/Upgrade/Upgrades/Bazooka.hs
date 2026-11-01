module Marvel.Upgrade.Upgrades.Bazooka where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Ability qualified as Ability
import Marvel.Damage

bazooka :: UpgradeCard Bazooka
bazooka = upgrade Bazooka Cards.bazooka

newtype Bazooka = Bazooka (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Bazooka where
  getAbilities a =
    [ subtype Ability.Attack $
        ability a 1 HeroAction (OwnsThis <> canAttack <> AnyCriteria [SelfMatches StunnedIdentity, EncounterIconExists])
          (DiscardCost $ toTarget a) (runAbility a 1)
    ]

instance RunMessage Bazooka where
  runMessage msg u@(Bazooka attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> attackGuard you u do
      amount <- getEncounterIconCount
      enemies <- selectList $ AttackableBy (IdentityWithId you)
      unless (null enemies) $ chooseOne you
        [targeting (enemyRef enemy) (dealDamage enemy attrs $ withRanged $ toDamage amount $ FromPlayerAttack you) | enemy <- enemies]
      pure u
    _ -> Bazooka <$> runMessage msg attrs
