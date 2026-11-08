module Marvel.Support.Supports.NextEvolutionNextEvolutionCampaign where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Card
import Marvel.Entity
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source ()
import Marvel.Query
import Marvel.Matchers
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype NextEvolutionCampaignSupport = NextEvolutionCampaignSupport (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40197 :: SupportCard NextEvolutionCampaignSupport
ne40197 = support NextEvolutionCampaignSupport Cards.ne40197

instance HasAbilities NextEvolutionCampaignSupport where
  getAbilities (NextEvolutionCampaignSupport a) =
    [limitedAbility a 1 (EachPlayer $ PerRound 1) AlterEgoAction ForAnyPlayer NoCost (runAbility a 1)]

instance RunMessage NextEvolutionCampaignSupport where
  runMessage msg e@(NextEvolutionCampaignSupport a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      canHeal <- selectAny (IdentityWithId you <> healableIdentity)
      chooseOne you $
        [labeled "$label.neSafehouseHeal" (heal you 2) | canHeal]
          <> [labeled "$label.neSafehouseDraw" (drawCards you 1)]
      pure e
    _ -> NextEvolutionCampaignSupport <$> runMessage msg a
