module Marvel.Ally.Allies.CableDeadpool where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Area ()
import Marvel.Game.Source

cableDeadpool :: AllyCard CableDeadpool
cableDeadpool = ally CableDeadpool Cards.cableDeadpool (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype CableDeadpool = CableDeadpool (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor CableDeadpool where
  getModifiersFor _ (AllyRef aid) (CableDeadpool attrs) | aid == allyId attrs = do
    area <- getAreaOf (allyController attrs)
    tokens <- getMainSchemeTokenCountIn (areaId area)
    let bonus = min 3 tokens
    pure [ThwartModifier bonus, AttackModifier bonus]
  getModifiersFor _ _ _ = pure []

instance RunMessage CableDeadpool where
  runMessage msg (CableDeadpool attrs) = runQueueT $ CableDeadpool <$> runMessage msg attrs
