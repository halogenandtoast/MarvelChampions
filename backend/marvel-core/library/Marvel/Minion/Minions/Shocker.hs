module Marvel.Minion.Minions.Shocker where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Message
import Marvel.Minion.Attrs
import Marvel.Minion.Cards qualified as Cards
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window qualified as W

shocker :: MinionCard Shocker
shocker = minion Shocker Cards.shocker (Sch 1) (Atk 2) (HP 3)

newtype Shocker = Shocker MinionAttrs
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Shocker where
  runMessage msg e@(Shocker attrs) = case msg of
    MinionMessage minionId msg' | minionId == toId attrs -> case msg' of
      RevealMinion -> do
        players <- getPlayers
        pushAll $ flip concatMap players $ \player ->
          [ CheckWindows
            [W.Window W.When $ W.IdentityTakeDamage player W.FromAbility 1]
          , IdentityMessage player $ IdentityDamaged (toSource attrs) 1
          ]
        pure e
      _ -> Shocker <$> runMessage msg attrs
    _ -> Shocker <$> runMessage msg attrs
