-- | System Shock (The Mad Titan's Shadow campaign set): an obligation the campaign
-- shuffles into a player's deck, or Defensive Protocols puts in their hand. Its text
-- only works in hand: drawn, it stays there (project owner's ruling), and the engine
-- keeps the player from choosing to discard it ('Marvel.Identity').
module Marvel.Obligation.Obligations.SystemShock (
  systemShock,
  SystemShock (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id (unObligationId)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

systemShock :: ObligationCard SystemShock
systemShock = obligation SystemShock Cards.systemShock

newtype SystemShock = SystemShock (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | The card, as it is in its holder's hand.
handCard :: Attrs Obligation -> CardId
handCard = CardId . unObligationId . obligationId

-- | While this card is in your hand, it gains "Alter-Ego Action: Spend a [mental]
-- resource → remove this card from the game." Sourced on the card in hand, so it is
-- offered only from there ('Marvel.Game' handCardAbilities).
instance HasAbilities SystemShock where
  getAbilities (SystemShock a) =
    [ ability
        (CardIdRef (handCard a) :: Source)
        1
        AlterEgoAction
        NoCriteria
        (ResourceCost $ Just Mental)
        (RunAbility (CardIdRef $ handCard a) 1)
    ]

instance RunMessage SystemShock where
  runMessage msg o@(SystemShock attrs) = runQueueT $ case msg of
    RanAbility _ (CardIdRef cid) 1 _ _ | cid == handCard attrs -> o <$ removeFromGame (CardIdRef cid)
    _ -> SystemShock <$> runMessage msg attrs
