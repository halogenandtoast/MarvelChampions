module Marvel.Support.Supports.BadBoy (
  badBoy,
  BadBoy (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window

badBoy :: SupportCard BadBoy
badBoy = support BadBoy Cards.badBoy

newtype BadBoy = BadBoy (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When you would take any amount of damage from the villain's attack,
-- discard this card → prevent all of that damage. Change to alter-ego form and draw 2
-- cards. The damage is an attack's while the villain's attack is under way.
instance HasAbilities BadBoy where
  getAbilities a@(BadBoy attrs) =
    [ limitedWindowAbility
        a
        1
        (WouldTakeDamage (IdentityWithId $ supportController attrs) FromAttack AnyValue)
        HeroInterrupt
        (OwnsThis <> InHeroForm <> EnemyExists (VillainEnemy <> AttackingEnemy))
        (DiscardCost $ toTarget a)
        (runAbility a 1)
    ]

instance RunMessage BadBoy where
  runMessage msg s@(BadBoy attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      cancelDamage ident
      changeToAlterEgo ident
      drawCards ident 2
      pure s
    _ -> BadBoy <$> runMessage msg attrs
