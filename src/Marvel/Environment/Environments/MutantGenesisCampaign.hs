module Marvel.Environment.Environments.MutantGenesisCampaign where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Card.Id (CardId)
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Trait (Trait (Sentinel))
import Marvel.Window qualified as W

mgRescueCaptives :: EnvironmentCard MutantGenesisCampaignEnvironment
mgRescueCaptives = environment MutantGenesisCampaignEnvironment Cards.mgRescueCaptives

newtype MutantGenesisCampaignEnvironment = MutantGenesisCampaignEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MutantGenesisCampaignEnvironment where
  getAbilities a@(MutantGenesisCampaignEnvironment attrs) =
    [limitedWindowAbility a 1 (W.EnemyDefeated W.After (MinionEnemy <> EnemyWithTrait Sentinel) (DamageByPlayer You))
      Response NoCriteria (ResourceCost Nothing) (runAbility a 1) | notNull (environmentHeldCards attrs)]

instance RunMessage MutantGenesisCampaignEnvironment where
  runMessage msg e@(MutantGenesisCampaignEnvironment attrs) = runQueueT $ case msg of
    CardSpecific target "mgHoldCard" (fromJSON -> Success card) | target == toRef attrs ->
      pure $ MutantGenesisCampaignEnvironment attrs {environmentHeldCards = environmentHeldCards attrs <> [card]}
    CardSpecific target "mgClearHeld" _ | target == toRef attrs ->
      pure $ MutantGenesisCampaignEnvironment attrs {environmentHeldCards = []}
    CardSpecific target "mgRemoveHeld" (fromJSON -> Success cid) | target == toRef attrs ->
      pure $ MutantGenesisCampaignEnvironment attrs {environmentHeldCards = filter ((/= (cid :: CardId)) . toCardId) (environmentHeldCards attrs)}
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseOne you [targeting card (takeHeldCardIntoPlay you attrs card) | card <- environmentHeldCards attrs]
      pure e
    _ -> MutantGenesisCampaignEnvironment <$> runMessage msg attrs
