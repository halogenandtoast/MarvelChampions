module Marvel.Ally.Allies.AgeOfApocalypseBasicCampaign where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability (HasAbilities)
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner ()
import Marvel.Ally.Types
import Marvel.Hp ()
import Marvel.Modifier ()
import Marvel.Stats ()

newtype AoaBasicCampaignAlly = AoaBasicCampaignAlly (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

destiny :: AllyCard AoaBasicCampaignAlly
destiny = ally AoaBasicCampaignAlly Cards.destiny (Thw 3, 1) (Atk 1, 1) (HP 3)

blink :: AllyCard AoaBasicCampaignAlly
blink = ally AoaBasicCampaignAlly Cards.blink (Thw 2, 1) (Atk 2, 1) (HP 3)

morph :: AllyCard AoaBasicCampaignAlly
morph = ally AoaBasicCampaignAlly Cards.morph (Thw 2, 1) (Atk 2, 1) (HP 3)

xMan :: AllyCard AoaBasicCampaignAlly
xMan = ally AoaBasicCampaignAlly Cards.xMan (Thw 1, 1) (Atk 3, 1) (HP 3)

-- Hand-entry responses are dispatched by Identity through the mission handler;
-- there is no ally entity in play when the triggering card enters its owner's hand.
instance RunMessage AoaBasicCampaignAlly where
  runMessage msg (AoaBasicCampaignAlly a) = AoaBasicCampaignAlly <$> runMessage msg a
