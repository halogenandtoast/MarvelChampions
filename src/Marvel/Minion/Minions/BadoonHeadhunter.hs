-- | The Badoon Headhunter modular set's minions (The Galaxy's Most Wanted).
module Marvel.Minion.Minions.BadoonHeadhunter (
  badoonHeadhunter,
  BadoonHeadhunter (..),
  headhuntersHenchman,
  HeadhuntersHenchman (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Game.Source
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

-- | Victory 2. Villainous. Both keywords its card's.
badoonHeadhunter :: MinionCard BadoonHeadhunter
badoonHeadhunter = minion BadoonHeadhunter Cards.badoonHeadhunter (Sch 1) (Atk 1) (HP 7)

newtype BadoonHeadhunter = BadoonHeadhunter (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Boost: put Badoon Headhunter into play engaged with you.
instance RunMessage BadoonHeadhunter where
  runMessage msg e@(BadoonHeadhunter attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      e <$ putBoostIntoPlay target you
    _ -> BadoonHeadhunter <$> runMessage msg attrs

-- | Surge. Patrol. Both its card's.
headhuntersHenchman :: MinionCard HeadhuntersHenchman
headhuntersHenchman = minion HeadhuntersHenchman Cards.headhuntersHenchman (Sch 1) (Atk 2) (HP 8)

newtype HeadhuntersHenchman = HeadhuntersHenchman (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HeadhuntersHenchman where
  runMessage msg (HeadhuntersHenchman attrs) = HeadhuntersHenchman <$> runMessage msg attrs
