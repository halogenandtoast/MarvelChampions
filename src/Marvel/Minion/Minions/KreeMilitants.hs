-- | The Kree Militants modular set's minions (The Galaxy's Most Wanted). Their keywords
-- (patrol; guard and stalwart; quickstrike) are their cards'.
module Marvel.Minion.Minions.KreeMilitants (
  kreeCommando,
  KreeCommando (..),
  kreeLieutenant,
  KreeLieutenant (..),
  kreePrivate,
  KreePrivate (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Id ()
import Marvel.Keyword (Keyword (Overkill, Piercing))
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Query ()
import Marvel.Game.Source (HasGame)

-- | Whether the enemy the boost card was turned up for is attacking.
attacking :: (HasGame m) => EnemyId -> m Bool
attacking enemy = selectAny $ EnemyWithId enemy <> AttackingEnemy

kreeCommando :: MinionCard KreeCommando
kreeCommando = minion KreeCommando Cards.kreeCommando (Sch 1) (Atk 2) (HP 6)

newtype KreeCommando = KreeCommando (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | [star] Boost: if this is an attack, this attack gains piercing.
instance RunMessage KreeCommando where
  runMessage msg e@(KreeCommando attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      whenM (attacking enemy) $ attackGains enemy Piercing
      pure e
    _ -> KreeCommando <$> runMessage msg attrs

kreeLieutenant :: MinionCard KreeLieutenant
kreeLieutenant = minion KreeLieutenant Cards.kreeLieutenant (Sch 2) (Atk 1) (HP 7)

newtype KreeLieutenant = KreeLieutenant (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | [star] Boost: if this activation is an attack, this card gets +3 boost icons for
-- this activation.
instance RunMessage KreeLieutenant where
  runMessage msg e@(KreeLieutenant attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      whenM (attacking enemy) case enemy of
        EnemyVillainId vid -> addBoost vid 3
        EnemyMinionId mid -> minionStep mid (MinionAddBoost 3)
        EnemyAttachmentId _ -> pure ()
      pure e
    _ -> KreeLieutenant <$> runMessage msg attrs

kreePrivate :: MinionCard KreePrivate
kreePrivate = minion KreePrivate Cards.kreePrivate (Sch 1) (Atk 1) (HP 5)

newtype KreePrivate = KreePrivate (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | [star] Boost: if this activation is an attack, this attack gains overkill.
instance RunMessage KreePrivate where
  runMessage msg e@(KreePrivate attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      whenM (attacking enemy) $ attackGains enemy Overkill
      pure e
    _ -> KreePrivate <$> runMessage msg attrs
