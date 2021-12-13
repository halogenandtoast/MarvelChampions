module Marvel.Minion.Minions.Shocker where

import Marvel.Prelude

import Marvel.Damage
import Marvel.Game.Source
import Marvel.Minion.Attrs
import Marvel.Minion.Cards qualified as Cards
import Marvel.Window qualified as W

shocker :: MinionCard Shocker
shocker = minion Shocker Cards.shocker (Sch 1) (Atk 2) (HP 3)

newtype Shocker = Shocker MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Shocker where
  runMessage msg e@(Shocker attrs) = case msg of
    MinionMessage minionId msg' | minionId == toId attrs -> case msg' of
      RevealMinion _ -> do
        players <- getPlayers
        pushAll $ flip concatMap players $ \player ->
          [ CheckWindows
            [W.Window W.When $ W.IdentityTakeDamage player (toDamage 1 FromAbility)]
          , IdentityMessage player $ IdentityDamaged (toSource attrs) (toDamage 1 FromAbility)
          ]
        pure e
      _ -> Shocker <$> runMessage msg attrs
    _ -> Shocker <$> runMessage msg attrs
