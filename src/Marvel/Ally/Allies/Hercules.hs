module Marvel.Ally.Allies.Hercules (
  hercules,
  Hercules (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Id ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Query

hercules :: AllyCard Hercules
hercules = ally Hercules Cards.hercules (Thw 1, 1) (Atk 3, 1) (HP 4)

newtype Hercules = Hercules (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | His cost, while he is played from hand, is 1 less for each minion engaged with
-- his player.
instance HasModifiersFor Hercules where
  getModifiersFor _ (CardIdRef cid) (Hercules a) | cid == CardId (unAllyId $ allyId a) = do
    n <- selectListCount $ MinionEngagedWith (IdentityWithId $ allyController a)
    pure [ResourceCostReduction n]
  getModifiersFor _ _ _ = pure []

instance RunMessage Hercules where
  runMessage msg (Hercules attrs) = Hercules <$> runMessage msg attrs
