module Marvel.Support.Supports.MutantGenesisCampaign where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

mgMetroPD :: SupportCard MutantGenesisCampaignSupport
mgMetroPD = support MutantGenesisCampaignSupport Cards.mgMetroPD

newtype MutantGenesisCampaignSupport = MutantGenesisCampaignSupport (Attrs Support)
  deriving anyclass IsSupport
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor MutantGenesisCampaignSupport where
  getModifiersFor _ target (MutantGenesisCampaignSupport attrs)
    | target == toRef attrs = pure [ControlledByFirstPlayer]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MutantGenesisCampaignSupport where
  getAbilities a = [ability a 1 Action OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage MutantGenesisCampaignSupport where
  runMessage msg s@(MutantGenesisCampaignSupport attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList AnyEnemy
      schemes <- selectList (SchemeWithThreat $ GreaterThan $ Static 0)
      let damage = [labeled "$label.mutantMetroDamage" (chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage 1 FromAbility) | enemy <- enemies]) | notNull enemies]
          threat = [labeled "$label.mutantMetroThreat" (chooseOne you [targeting (schemeRef scheme) (removeThreat you attrs 1 (SchemeWithId scheme)) | scheme <- schemes]) | notNull schemes]
      unless (null $ damage <> threat) $ chooseOne you (damage <> threat)
      pure s
    _ -> MutantGenesisCampaignSupport <$> runMessage msg attrs
