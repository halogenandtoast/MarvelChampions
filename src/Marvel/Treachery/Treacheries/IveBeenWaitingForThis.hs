module Marvel.Treachery.Treacheries.IveBeenWaitingForThis (
  iveBeenWaitingForThis,
  IveBeenWaitingForThis (..),
) where

import Marvel.Prelude

import Control.Monad ()
import Data.Maybe ()
import Data.Aeson ()
import Marvel.Attack ()
import Marvel.Card
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Data.List ()
import Marvel.Id
import Marvel.Identity.Types ()
import Marvel.Projection ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Status ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

iveBeenWaitingForThis :: TreacheryCard IveBeenWaitingForThis
iveBeenWaitingForThis = treachery IveBeenWaitingForThis Cards.iveBeenWaitingForThis

newtype IveBeenWaitingForThis = IveBeenWaitingForThis (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: the active villain heals 3 hit points ("heals", RR v1.8 errata). Give
-- that villain a tough status card.
-- [star] Boost: move the active counter to the villain with the least threat on his side
-- scheme. That villain schemes.
instance RunMessage IveBeenWaitingForThis where
  runMessage msg t@(IveBeenWaitingForThis attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      selectOne ActiveVillain >>= traverse_ \v -> do
        heal (VillainCharacter v) 3
        enemyBecomesTough v
      pure t
    -- the scheming is initiated now, and so follows the activation under way (RR
    -- "Activation")
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      withVillainBySideSchemeThreat LeastThreat \v -> do
        setActiveVillain v
        cardSpecific attrs "schemes" (v, enemy)
      pure t
    Boost (CardSpecific (isTarget attrs -> True) "schemes" (fromJSON -> Success (v, enemy))) -> do
      activationAfter enemy v (villainSchemes v)
      pure t
    _ -> IveBeenWaitingForThis <$> runMessage msg attrs
