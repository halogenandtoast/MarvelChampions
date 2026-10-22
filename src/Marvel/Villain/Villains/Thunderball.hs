module Marvel.Villain.Villains.Thunderball where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attack
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (Choice (RunAbility))
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window

newtype Thunderball = Thunderball (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

thunderball :: VillainCard Thunderball
thunderball = thunderballA

thunderballA :: VillainCard Thunderball
thunderballA = villain Thunderball Cards.thunderballA (Sch 3) (Atk 1) (HP $ PerPlayer 13)

thunderballB :: VillainCard Thunderball
thunderballB = villainWith Thunderball Cards.thunderballB (Sch 3) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 2)

-- | Forced Response: after Thunderball attacks you, deal 1 damage to each character you
-- control.
instance HasAbilities Thunderball where
  getAbilities (Thunderball a) =
    [ windowAbility a 1 (EnemyAttacked After (EnemyWithId $ EnemyVillainId $ villainId a) AnyIdentity) ForcedResponse NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage Thunderball where
  runMessage msg e@(Thunderball attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      b <- versionBEnters attrs
      if b
        then advanceVillain ident >> pure (advanceVillainTo thunderballB attrs)
        else Thunderball <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttack _ you] _ -> do
      damageAllCharacters attrs (toDamage 1 FromAbility) (CharacterControlledBy $ IdentityWithId you)
      pure e
    _ -> Thunderball <$> runMessage msg attrs
