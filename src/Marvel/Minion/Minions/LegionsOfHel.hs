-- | The minion of the Legions of Hel modular set (The Mad Titan's Shadow).
module Marvel.Minion.Minions.LegionsOfHel (
  draugr,
  Draugr (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Damage
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

draugr :: MinionCard Draugr
draugr = minion Draugr Cards.draugr (Sch 1) (Atk 1) (HP 3)

newtype Draugr = Draugr (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Guard. When Revealed: choose to either take 1 damage or place 1 threat on the main
-- scheme.
instance RunMessage Draugr where
  runMessage msg (Draugr attrs) = runQueueT $ case msg of
    MinionMessage ident (RevealMinion you) | ident == minionId attrs -> do
      chooseOne you
        [ labeled (countLabel "takeDamage" 1) $ damageIdentity you attrs (toDamage 1 FromAbility)
        , labeled (countLabel "placeThreatOnMainScheme" 1) $ placeThreatOnEach attrs 1 MainScheme
        ]
      Draugr <$> runMessage msg attrs
    _ -> Draugr <$> runMessage msg attrs
