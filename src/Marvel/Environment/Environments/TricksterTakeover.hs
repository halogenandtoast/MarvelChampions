module Marvel.Environment.Environments.TricksterTakeover where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.TricksterTakeover.Cards qualified as Cards
import Marvel.Window qualified as W
import Marvel.Environment.Types

newtype TTEnvironment = TTEnvironment (Attrs Environment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsEnvironment

tt55052 :: EnvironmentCard TTEnvironment
tt55052 = environment TTEnvironment Cards.tt55052

tt55053 :: EnvironmentCard TTEnvironment
tt55053 = environment TTEnvironment Cards.tt55053

tt55054 :: EnvironmentCard TTEnvironment
tt55054 = environment TTEnvironment Cards.tt55054

tt55055 :: EnvironmentCard TTEnvironment
tt55055 = environment TTEnvironment Cards.tt55055

instance HasModifiersFor TTEnvironment

instance HasAbilities TTEnvironment where
  getAbilities (TTEnvironment a) = case toCardCode a of
    "55052" ->
      [windowAbility a 1 (W.MakesBasicAttack W.When AnyIdentity) Interrupt (ExactCountersCost 1) (runAbility a 1)
      ,windowAbility a 1 (W.AllyAttacked W.When AnyAlly AnyEnemy) Interrupt (ExactCountersCost 1) (runAbility a 1)
      ,windowAbility a 1 (W.MakesKeywordAttack W.When AnyIdentity []) Interrupt (ExactCountersCost 1) (runAbility a 1)]
    "55053" -> [windowAbility a 2 (W.WouldTakeDamage AnyIdentity FromAnyDamageSource $ GreaterThan $ Static 0) Interrupt (ExactCountersCost 1) (runAbility a 2)]
    "55054" -> [windowAbility a 3 (W.ThreatRemovedByThwart W.When AnySource AnyScheme) Interrupt (ExactCountersCost 1) (runAbility a 3)]
    "55055" -> [ability a 4 Resource NoCriteria (ExactCountersCost 1) $ Pay (ResourcePayment Wild <> ResourcePayment Wild)]
    _ -> []

instance RunMessage TTEnvironment where
  runMessage msg e@(TTEnvironment a) = runQueueT $ case msg of
    CardSpecific (isTarget a -> True) "ttAddSynergy" (fromJSON -> Success (n :: Natural)) -> add n
    CardSpecific (isTarget a -> True) "ttAddNamedSynergy" (fromJSON -> Success (code :: CardCode, n :: Natural)) | code == toCardCode a -> add n
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      for_ [aid | W.AllyAttack aid _ <- windows] (`addToAllyBasicPower` 4)
      when (null [aid | W.AllyAttack aid _ <- windows]) $ modifyCurrentPlayerAttack you 4 []
      pure e
    RanAbility _ (isTarget a -> True) 2 windows _ -> do
      for_ [p | W.IdentityTakeDamage p _ <- windows] $ \p -> preventDamage p 4
      pure e
    RanAbility _ (isTarget a -> True) 3 windows _ -> do
      for_ [(source, scheme) | W.ThwartRemovedThreat source scheme _ _ <- windows] $ \(source,scheme) -> increaseCurrentThwart source scheme 4
      pure e
    _ -> TTEnvironment <$> runMessage msg a
   where
    add n = do
      count <- getPlayerCount
      pure $ TTEnvironment a {environmentCounters = min (fromIntegral count) (environmentCounters a + n)}
