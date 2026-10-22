module Marvel.Minion.Minions.ThomasEdison (
  thomasEdison,
  ThomasEdison (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

thomasEdison :: MinionCard ThomasEdison
thomasEdison = minion ThomasEdison Cards.thomasEdison (Sch 3) (Atk 1) (HP 3)

newtype ThomasEdison = ThomasEdison (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | No damage while the player he is engaged with is engaged with another minion.
instance HasModifiersFor ThomasEdison where
  getModifiersFor _ (MinionRef mid) (ThomasEdison attrs) | mid == minionId attrs = do
    others <-
      selectAny $
        MinionEngagedWith (IdentityWithId $ minionEngagedIdentity attrs)
          <> NotMinion (MinionWithId mid)
    pure [CannotTakeDamage | others]
  getModifiersFor _ _ _ = pure []

instance RunMessage ThomasEdison where
  runMessage msg (ThomasEdison attrs) = ThomasEdison <$> runMessage msg attrs
