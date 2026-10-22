module Marvel.Villain.Villains.Wrecker where

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

newtype Wrecker = Wrecker (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

wrecker :: VillainCard Wrecker
wrecker = wreckerA

wreckerA :: VillainCard Wrecker
wreckerA = villain Wrecker Cards.wreckerA (Sch 2) (Atk 2) (HP $ PerPlayer 14)

wreckerB :: VillainCard Wrecker
wreckerB = villainWith Wrecker Cards.wreckerB (Sch 3) (Atk 3) (HP $ PerPlayer 18) (stageL .~ 2)

-- | When Wrecker schemes, the threat goes on his side scheme (his signature scheme).
-- While he is attacking, he gets +2 ATK if the attack is undefended.
instance RunMessage Wrecker where
  runMessage msg (Wrecker attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      b <- versionBEnters attrs
      if b
        then advanceVillain ident >> pure (advanceVillainTo wreckerB attrs)
        else Wrecker <$> runMessage msg attrs
    VillainMessage ident VillainAttacked
      | ident == villainId attrs
      , maybe False (not . attackDefended) (villainAttacking attrs) ->
          Wrecker <$> runMessage msg (attrs & attackingL . _Just . attackDamageL +~ 2)
    _ -> Wrecker <$> runMessage msg attrs
