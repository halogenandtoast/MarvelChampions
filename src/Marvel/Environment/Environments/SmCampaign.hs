module Marvel.Environment.Environments.SmCampaign where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Window qualified as W

newtype SmCampaignEnvironment = SmCampaignEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smPublicOutcryA, smPublicOutcryB :: EnvironmentCard SmCampaignEnvironment
smPublicOutcryA = environment SmCampaignEnvironment Cards.smPublicOutcryA
smPublicOutcryB = environment SmCampaignEnvironment Cards.smPublicOutcryB

instance HasAbilities SmCampaignEnvironment where
  getAbilities a@(SmCampaignEnvironment attrs) =
    [ limitedWindowAbility a 1 (W.WindowOneOf [W.MinionDefeated W.After AnyMinion, W.SideSchemeDefeated W.After AnySideScheme])
        Response NoCriteria NoCost (runAbility a 1)
    | environmentCounters attrs > 0
    ]

instance RunMessage SmCampaignEnvironment where
  runMessage msg e@(SmCampaignEnvironment attrs) = runQueueT $ case msg of
    EnvironmentMessage eid EnvironmentEntered | eid == environmentId attrs -> do
      players <- getStartingPlayerCount
      placeCounters attrs (fromIntegral players * if toCardCode attrs == "27174b" then 3 else 2)
      pure e
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ removeCounters attrs 1
    RemoveCounters target n | target == toRef attrs -> do
      attrs' <- runMessage msg attrs
      when (n > 0 && environmentCounters attrs > 0 && environmentCounters attrs' == 0) $ addToVictoryDisplay attrs
      pure $ SmCampaignEnvironment attrs'
    _ -> SmCampaignEnvironment <$> runMessage msg attrs
