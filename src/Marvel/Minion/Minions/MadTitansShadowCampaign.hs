-- | The minion of The Mad Titan's Shadow's campaign set.
module Marvel.Minion.Minions.MadTitansShadowCampaign (
  blackSwan,
  BlackSwan (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

blackSwan :: MinionCard BlackSwan
blackSwan = minion BlackSwan Cards.blackSwan (Sch 2) (Atk 2) (HP 4)

newtype BlackSwan = BlackSwan (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Toughness, Victory 2 (the def). Black Swan engages the first player, and moves to
-- each new first player ('engagesFirstPlayer'). Forced Response: after Black Swan
-- engages you, discard 1 card from your hand.
instance HasAbilities BlackSwan where
  getAbilities (BlackSwan a) =
    [windowAbility a 1 (EngagesMinion After AnyIdentity (MinionWithId $ minionId a)) ForcedResponse NoCost $ runAbility a 1]

instance RunMessage BlackSwan where
  runMessage msg e@(BlackSwan attrs) = runQueueT do
    -- a constant ability: she follows the first player token, engaging them each time
    engagesFirstPlayer msg attrs
    case msg of
      RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
        let you = minionEngagedIdentity attrs
        discardFor you attrs FromHand 1 1
        pure e
      _ -> BlackSwan <$> runMessage msg attrs
