module Marvel.Ally.Allies.WaspNadiaVanDyneIronheart where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Code ()
import Marvel.Entity ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Message.Lifted ()
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.Stats ()

waspNadiaVanDyneIronheart :: AllyCard WaspNadiaVanDyneIronheart
waspNadiaVanDyneIronheart = ally WaspNadiaVanDyneIronheart Cards.waspNadiaVanDyneIronheart (Thw 2, 2) (Atk 1, 1) (HP 3)

newtype WaspNadiaVanDyneIronheart = WaspNadiaVanDyneIronheart (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage WaspNadiaVanDyneIronheart where
  runMessage msg (WaspNadiaVanDyneIronheart attrs) = WaspNadiaVanDyneIronheart <$> runMessage msg attrs

instance HasModifiersFor WaspNadiaVanDyneIronheart where
  getModifiersFor _ target (WaspNadiaVanDyneIronheart attrs) | target == toTarget attrs = pure [IgnoresGuard, IgnoresPatrol, IgnoresCrisis]
  getModifiersFor _ _ _ = pure []
