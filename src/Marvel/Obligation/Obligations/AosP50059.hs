{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Obligation.Obligations.AosP50059 where

import Marvel.Prelude
import Marvel.AgentsOfShield.PlayerHelpers
import Marvel.Damage
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

aosP50059 :: ObligationCard AosP50059
aosP50059 = obligation AosP50059 Cards.aosP50059

newtype AosP50059 = AosP50059 (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50059 where
  runMessage msg e@(AosP50059 a) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> do
      changeSuit you False
      cardSpecific a "resolveDiscovered" you
      pure $ AosP50059 a {obligationOwner=Just you}
    CardSpecific (isTarget a -> True) "resolveDiscovered" (fromJSON -> Success you) -> do
      amount <- suitThreat you
      if amount == 0 then gainSurge a else chooseOne you
        [labeled "$label.aosTakeSuitDamage" (damageIdentity you a $ toDamage amount FromAbility)
        ,labeled "$label.aosRemoveSuitThreat" (removeSuitThreat you amount)]
      pure e
    _ -> AosP50059 <$> runMessage msg a
