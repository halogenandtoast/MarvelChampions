{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Treachery.Treacheries.AosP50033 where

import Marvel.Prelude
import Marvel.AgentsOfShield.NemesisHelpers
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

aosP50033 :: TreacheryCard AosP50033
aosP50033 = treachery AosP50033 Cards.aosP50033

newtype AosP50033 = AosP50033 (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50033 where
  runMessage msg e@(AosP50033 a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      removeSupportCounter you
      addControlledMinion you
      pure e
    _ -> AosP50033 <$> runMessage msg a
