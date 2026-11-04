-- | The minions of Hela's set (The Mad Titan's Shadow, scenario 4): the guardians of the
-- road through Hel, each set aside until the side scheme before its own is defeated.
-- Each engages the first player; the side scheme each locks is told so on its own card
-- ('Marvel.SideScheme.SideSchemes.Hela').
module Marvel.Minion.Minions.Hela (
  garm,
  Garm (..),
  skurge,
  Skurge (..),
  nidhogg,
  Nidhogg (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Keyword
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

garm :: MinionCard Garm
garm = minion Garm Cards.garm (Sch 1) (Atk 2) (HP 4)

newtype Garm = Garm (Attrs Minion)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | 4[per hero] hit points.
instance HasModifiersFor Garm where
  getModifiersFor _ (MinionRef mid) (Garm a) | mid == minionId a = perPlayerHitPoints a 4
  getModifiersFor _ _ _ = pure []

-- | Garm engages the first player.
instance RunMessage Garm where
  runMessage msg (Garm attrs) = runQueueT do
    engagesFirstPlayer msg attrs
    Garm <$> runMessage msg attrs

skurge :: MinionCard Skurge
skurge = minion Skurge Cards.skurge (Sch 1) (Atk 3) (HP 5)

newtype Skurge = Skurge (Attrs Minion)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | 5[per hero] hit points. [star] Skurge's attacks gain piercing.
instance HasModifiersFor Skurge where
  getModifiersFor _ (MinionRef mid) (Skurge a) | mid == minionId a = (KeywordModifier Piercing :) <$> perPlayerHitPoints a 5
  getModifiersFor _ _ _ = pure []

-- | Skurge engages the first player.
instance RunMessage Skurge where
  runMessage msg (Skurge attrs) = runQueueT do
    engagesFirstPlayer msg attrs
    Skurge <$> runMessage msg attrs

nidhogg :: MinionCard Nidhogg
nidhogg = minion Nidhogg Cards.nidhogg (Sch 1) (Atk 4) (HP 6)

newtype Nidhogg = Nidhogg (Attrs Minion)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | 6[per hero] hit points. [star] Nidhogg's attacks gain overkill.
instance HasModifiersFor Nidhogg where
  getModifiersFor _ (MinionRef mid) (Nidhogg a) | mid == minionId a = (KeywordModifier Overkill :) <$> perPlayerHitPoints a 6
  getModifiersFor _ _ _ = pure []

-- | Nidhogg engages the first player.
instance RunMessage Nidhogg where
  runMessage msg (Nidhogg attrs) = runQueueT do
    engagesFirstPlayer msg attrs
    Nidhogg <$> runMessage msg attrs

instance IsMinion Garm where
  minionBaseHitPoints _ = perPlayerMinionBaseHitPoints 4

instance IsMinion Skurge where
  minionBaseHitPoints _ = perPlayerMinionBaseHitPoints 5

instance IsMinion Nidhogg where
  minionBaseHitPoints _ = perPlayerMinionBaseHitPoints 6
