module Marvel.Support.Supports.AgeOfApocalypseBasicCampaign where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

newtype AoaBasicCampaignSupport = AoaBasicCampaignSupport (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

missionTeamFront :: SupportCard AoaBasicCampaignSupport
missionTeamFront = support AoaBasicCampaignSupport Cards.missionTeamFront

missionTeamFinished :: SupportCard AoaBasicCampaignSupport
missionTeamFinished = support AoaBasicCampaignSupport Cards.missionTeamFinished

instance HasModifiersFor AoaBasicCampaignSupport where
  getModifiersFor _ target (AoaBasicCampaignSupport a) | target == toRef a = pure [CannotBeDiscarded, ControlledByFirstPlayer]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AoaBasicCampaignSupport where
  getAbilities (AoaBasicCampaignSupport a) = [ability a 1 Action OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage AoaBasicCampaignSupport where
  runMessage msg e@(AoaBasicCampaignSupport a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      if toCardCode a == "45171a" then chooseOne you
        [labeled "$label.aoaMissionDiscount" (cardSpecific GameRef "aoaMissionDiscount" ())
        ,labeled "$label.aoaMissionAttempt" (cardSpecific GameRef "aoaMissionAttempt" you)]
      else do
        players <- getPlayers
        chooseOne you [targeting player (drawCards player 1) | player <- players]
      pure e
    CardSpecific target "aoaMissionTeamFlip" _ | target == toRef a -> pure $ AoaBasicCampaignSupport a {supportCardDef = Cards.missionTeamFinished}
    _ -> AoaBasicCampaignSupport <$> runMessage msg a
