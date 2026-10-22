module Marvel.Minion.Minions.Shocker where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Damage
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

shocker :: MinionCard Shocker
shocker = minion Shocker Cards.shocker (Sch 1) (Atk 2) (HP 3)

newtype Shocker = Shocker (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage Shocker where
  runMessage msg e@(Shocker attrs) = runQueueT $ case msg of
    MinionMessage ident msg' | ident == minionId attrs -> case msg' of
      RevealMinion _ -> do
        players <- selectList HeroIdentity
        for_ players \player -> damageIdentity player attrs (toDamage 1 FromAbility)
        pure e
      _ -> Shocker <$> runMessage msg attrs
    _ -> Shocker <$> runMessage msg attrs
