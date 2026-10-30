module Marvel.Ally.Allies.BombshellLanaBaumgartner where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats

bombshellLanaBaumgartner :: AllyCard BombshellLanaBaumgartner
bombshellLanaBaumgartner = ally BombshellLanaBaumgartner Cards.bombshellLanaBaumgartner (Thw 2, 2) (Atk 3, 1) (HP 3)

newtype BombshellLanaBaumgartner = BombshellLanaBaumgartner (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BombshellLanaBaumgartner where
  runMessage msg (BombshellLanaBaumgartner attrs) = BombshellLanaBaumgartner <$> runMessage msg attrs

instance HasModifiersFor BombshellLanaBaumgartner where
  getModifiersFor _ target (BombshellLanaBaumgartner attrs) | target == toTarget attrs = pure [DividesBasicAttackEvenly]
  getModifiersFor _ _ _ = pure []
