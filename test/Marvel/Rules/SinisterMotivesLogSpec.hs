module Marvel.Rules.SinisterMotivesLogSpec (spec) where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Campaign
import Marvel.Prelude
import TestImport (testTail)
import Test.Hspec

campaign :: Campaign
campaign = newCampaignOf SinisterMotives StandardCampaign ["27001b", "27030a"]

-- | Remove a campaign section as a save made before its introduction would do.
withoutSection :: Text -> Value -> Value
withoutSection name = \case
  Object object -> Object $ KeyMap.mapWithKey (\key value -> if key == "campaignLog" then remove value else value) object
  other -> other
 where
  remove (Object object) = Object $ KeyMap.delete (Key.fromText name) object
  remove other = other

spec :: Spec
spec = describe "Sinister Motives campaign log" do
  it "starts the five scenarios in order and normalizes identity faces" do
    campaign.scenarios `shouldBe` ["27061", "27073", "27084", "27100", "27113"]
    currentScenario campaign `shouldBe` Just "27061"
    map (.identity) campaign.campaignLog.sinister.sinisterPlayers `shouldBe` ["27001a", "27030a"]
    campaign.campaignLog.sinister.reputation `shouldBe` 0

  it "round trips every campaign-log field including signed reputation" do
    let saved = foldl' (flip recordInLog) campaign
          [ RecordShieldTech "27001b" "27182a"
          , RecordAspectAdvantage "27030a" "01086" 3
          , RecordPlanningAhead "27001a" "27002"
          , RecordCommunityService ["27176", "27180"]
          , RecordOsbornTech "27147"
          , RecordWakingNightmare 4
          , RecordLastOnesStanding ["27094", "27099"]
          , RecordReputation (-1)
          , RecordFinalReputation
          ]
    fromJSON (toJSON saved) `shouldBe` Success saved
    saved.campaignLog.sinister.reputation `shouldBe` (-1)

  it "reads campaigns saved before the Sinister Motives section existed" do
    for_ [RiseOfRedSkull, GalaxysMostWanted, MadTitansShadow] \kind -> do
      let original = newCampaignOf kind StandardCampaign ["01001a"]
          old = withoutSection "sinister" (toJSON original)
      fromJSON old `shouldBe` Success original

  it "defaults missing fields in an empty or partial Sinister section" do
    fromJSON (object []) `shouldBe` Success noSinisterLog
    fromJSON (object ["reputation" .= (7 :: Int)]) `shouldBe` Success noSinisterLog {reputation = 7}

  it "reads a pre-campaign-kind save as Rise of Red Skull" do
    let original = newCampaign StandardCampaign ["01001a"]
        old = case withoutSection "sinister" (toJSON original) of
          Object object -> Object $ KeyMap.delete "kind" object
          other -> other
    fromJSON old `shouldBe` Success original

  it "does not add Sinister players or cards to another campaign" do
    let original = newCampaign StandardCampaign ["01001a"]
    original.campaignLog.sinister `shouldBe` noSinisterLog
    sinisterPlayer "01001a" original `shouldBe` Nothing
    campaignCardsOf "01001a" original `shouldBe` []

  it "crosses every connected node reached, without repeating prior nodes" do
    crossedReputationThresholds 0 13 `shouldBe` [1, 5, 9, 13]
    crossedReputationThresholds 13 25 `shouldBe` [17, 21, 25]
    crossedReputationThresholds 5 5 `shouldBe` []
    crossedReputationThresholds 25 40 `shouldBe` []

  it "does not round reputation to a connected node or cap the final score" do
    let scored = recordInLog RecordFinalReputation $ recordInLog (RecordReputation 31) campaign
    scored.campaignLog.sinister.reputation `shouldBe` 31
    scored.campaignLog.sinister.finalReputation `shouldBe` Just 31
    crossedReputationThresholds 0 31 `shouldBe` reputationThresholds

  it "adds earned reputation between victories" do
    let scored = recordInLog (RecordReputation 6) $ recordInLog (RecordReputation 4) campaign
    scored.campaignLog.sinister.reputation `shouldBe` 10

  it "persists exactly the granted copies and does not duplicate Planning Ahead" do
    let rewarded = recordInLog (RecordPlanningAhead "27001a" "27002")
          $ recordInLog (RecordAspectAdvantage "27001b" "01086" 3)
          $ recordInLog (RecordShieldTech "27001a" "27184a") campaign
    campaignCardsOf "27001b" rewarded `shouldBe` ["27184a", "01086", "01086", "01086"]
    campaignCardsOf "27030a" rewarded `shouldBe` []

  it "preserves a card's smaller maximum copy allowance" do
    let rewarded = recordInLog (RecordAspectAdvantage "27030a" "26033" 2) campaign
    campaignCardsOf "27030a" rewarded `shouldBe` ["26033", "26033"]

  it "removes only chosen physical Tech cards from the next player's random pool" do
    let rewarded = recordInLog (RecordShieldTech "27001a" "27182a") campaign
    shieldTechLeft rewarded `shouldBe` testTail shieldTechUpgrades
    recordInLog (RecordShieldTech "27030a" "27182a") rewarded `shouldBe` rewarded
    recordInLog (RecordShieldTech "27001a" "27182a") rewarded `shouldBe` rewarded

  it "ignores a Tech grant to an identity outside this campaign" do
    recordInLog (RecordShieldTech "01001a" "27182a") campaign `shouldBe` campaign
    recordInLog (RecordShieldTech "27001a" "04155") campaign `shouldBe` campaign

  it "records unique Osborn attachments and returns the remaining random pool" do
    let recorded = recordInLog (RecordOsbornTech "27147") $ recordInLog (RecordOsbornTech "27147") campaign
    recorded.campaignLog.sinister.osbornTech `shouldBe` ["27147"]
    osbornTechLeft recorded `shouldBe` testTail osbornTechAttachments
    recordInLog (RecordOsbornTech "27066") recorded `shouldBe` recorded

  it "excludes only completed Community Service from later random choices" do
    let recorded = recordInLog (RecordCommunityService ["27176", "27180", "27176", "27174a"]) campaign
    recorded.campaignLog.sinister.communityService `shouldBe` ["27176", "27180"]
    communityServiceLeft recorded `shouldBe` ["27177", "27178", "27179"]
    recordInLog (RecordCommunityService ["27180"]) recorded `shouldBe` recorded

  it "maps the surviving Six by name to their Sinister Assault minions" do
    let recorded = recordInLog (RecordLastOnesStanding ["27094", "27099", "27094", "27113"]) campaign
    recorded.campaignLog.sinister.lastOnesStanding `shouldBe` ["27094", "27099"]
    lastOnesStandingMinions recorded `shouldBe` ["27158", "27163"]

  it "records zero Illusions distinctly from an unplayed Mysterio scenario" do
    campaign.campaignLog.sinister.wakingNightmare `shouldBe` Nothing
    let recorded = recordInLog (RecordWakingNightmare 0) campaign
    recorded.campaignLog.sinister.wakingNightmare `shouldBe` Just 0
