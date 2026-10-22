module Marvel.Minion.Minions.FrostGiant (
  frostGiant,
  FrostGiant (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window qualified as W

frostGiant :: MinionCard FrostGiant
frostGiant = minion (FrostGiant . (`With` Meta False)) Cards.frostGiant (Sch 1) (Atk 3) (HP 4)

-- | Whether, as a boost card for the villain's attack, it stuns whom the attack damages.
newtype Meta = Meta {stuns :: Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype FrostGiant = FrostGiant (Attrs Minion `With` Meta)
  deriving anyclass (HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsMinion FrostGiant where
  toMinionAttrs (FrostGiant (attrs `With` _)) = attrs
  overMinionAttrs f (FrostGiant (attrs `With` meta)) = FrostGiant (f attrs `With` meta)

instance IsRef FrostGiant where
  toRef = toRef . toMinionAttrs

-- | [star] Boost: if the villain is attacking and this attack deals damage to a
-- character, stun that character. As a boost card it sees the attack through, until
-- the boost cards are discarded.
instance RunMessage FrostGiant where
  runMessage msg g@(FrostGiant (attrs `With` meta)) = case msg of
    Boost (RevealedAsBoost (isTarget attrs -> True) enemy) -> do
      attacking <- selectAny $ EnemyWithId enemy <> VillainEnemy <> AttackingEnemy
      pure $ FrostGiant (attrs `With` Meta attacking)
    Boost (CheckWindows [W.Window W.After (W.EnemyAttacksAndDamages (EnemyVillainId _) character)])
      | stuns meta -> do
          push $ case character of
            IdentityCharacter ident -> IdentityMessage ident IdentityStunned
            AllyCharacter ally -> AllyMessage ally AllyStunned
            VillainCharacter _ -> error "a villain attacks a hero or an ally"
            MinionCharacter _ -> error "a villain attacks a hero or an ally"
          pure g
    _ -> FrostGiant . (`With` meta) <$> runMessage msg attrs
