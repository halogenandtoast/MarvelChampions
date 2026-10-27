-- | Infiltrate the Museum's minion and the Menagerie Medley modular set's (The Galaxy's
-- Most Wanted).
module Marvel.Minion.Minions.MenagerieMedley (
  monarkStarstalker,
  MonarkStarstalker (..),
  psionicGhost,
  PsionicGhost (..),
  servantBot,
  ServantBot (..),
  starshark,
  Starshark (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attack (Attack (..))
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

-- | Villainous: its card's.
monarkStarstalker :: MinionCard MonarkStarstalker
monarkStarstalker = minion MonarkStarstalker Cards.monarkStarstalker (Sch 2) (Atk 2) (HP 7)

newtype MonarkStarstalker = MonarkStarstalker (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MonarkStarstalker where
  runMessage msg (MonarkStarstalker attrs) = MonarkStarstalker <$> runMessage msg attrs

psionicGhost :: MinionCard PsionicGhost
psionicGhost = minion PsionicGhost Cards.psionicGhost (Sch 2) (Atk 2) (HP 4)

-- | When Revealed: you are confused; if you are already confused, take 1 damage. [star]
-- Boost: put Psionic Ghost into play engaged with you.
newtype PsionicGhost = PsionicGhost (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PsionicGhost where
  runMessage msg e@(PsionicGhost attrs) = runQueueT $ case msg of
    MinionMessage ident (RevealMinion you) | ident == minionId attrs -> do
      confused <- identityMatches ConfusedIdentity you
      if confused then damageIdentity you attrs (toDamage 1 FromAbility) else confuseIdentity you
      PsionicGhost <$> runMessage msg attrs
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      e <$ putBoostIntoPlay target you
    _ -> PsionicGhost <$> runMessage msg attrs

-- | Guard. Patrol. Both its card's.
servantBot :: MinionCard ServantBot
servantBot = minion ServantBot Cards.servantBot (Sch 1) (Atk 1) (HP 3)

newtype ServantBot = ServantBot (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ServantBot where
  runMessage msg (ServantBot attrs) = ServantBot <$> runMessage msg attrs

-- | Quickstrike, its card's. [star] Starshark's attacks deal indirect damage: what is left
-- past a hero's defense is divided among the characters the attacked player controls
-- (RR Indirect Damage). [star] Boost: deal 1 damage to each character you control.
starshark :: MinionCard Starshark
starshark = minion Starshark Cards.starshark (Sch 1) (Atk 3) (HP 7)

newtype Starshark = Starshark (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Starshark where
  runMessage msg e@(Starshark attrs) = runQueueT $ case msg of
    -- the attack, as it is declared and as it begins, deals indirect damage
    MinionMessage ident msg' | ident == minionId attrs, attacks msg' -> do
      Starshark attrs' <- Starshark <$> runMessage msg attrs
      pure . Starshark $ attrs' & attackingL . _Just %~ \a -> a {attackIndirect = True}
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      for_ (IdentityCharacter you : map AllyCharacter allies) \c ->
        damageCharacter c attrs (toDamage 1 FromAbility)
      pure e
    _ -> Starshark <$> runMessage msg attrs
   where
    attacks = \case
      MinionAttacks _ -> True
      MinionBeginAttack _ -> True
      _ -> False
