module Marvel.Campaign.RiseOfRedSkullGuideSpec (spec) where

import Marvel.Campaign
import Data.Text qualified as Text
import TestImport

spec :: Spec
spec = describe "The Rise of Red Skull campaign guide" do
  for_ [(0,"crossbones"),(1,"absorbingMan"),(2,"taskmaster"),(3,"zola"),(4,"redSkull")] $ \(completed,chapter) ->
    for_ [StandardCampaign,ExpertCampaign] $ \mode ->
      it ("shows " <> Text.unpack chapter <> " and only its active campaign-mode instructions in " <> show mode) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let c = (newCampaign mode ["01001a"]) {completed}
              chapterKey section = "$story." <> chapter <> "." <> section
              expected = [chapterKey "intro"]
                <> (if completed == 0 then ["$story.campaignMode","$story.newKeywords"] else [])
                <> ["$story.expertCampaign" | completed == 0 && mode == ExpertCampaign]
                <> [chapterKey "rules"]
                <> (if completed `elem` [0,4] then [chapterKey "setup"] else ["$story.setup"])
                <> ["$story.expertSetup" | completed > 0 && mode == ExpertCampaign]
                <> [chapterKey "expertSetup" | completed == 4 && mode == ExpertCampaign]
          withGame_ $ campaignL ?~ c
          runNow [CampaignStep CampaignChoices]
          openQuestion >>= \case
            Just (_,Read sections _) -> sections `shouldBe` expected
            _ -> error "expected the Red Skull campaign briefing"
          (.completed) . fromMaybe (error "campaign missing") <$> getCampaign `shouldReturn` completed

  it "offers the printed conclusion after final victory before recording completion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let c = (newCampaign StandardCampaign ["01001a"]) {completed=4}
      withGame_ $ campaignL ?~ c
      runNow [CampaignStep CampaignVictory]
      openQuestion >>= \case
        Just (_,Read sections _) -> sections `shouldBe` ["$story.redSkull.victory","$story.redSkull.conclusion"]
        _ -> error "expected Red Skull victory and conclusion"
      (.completed) . fromMaybe (error "campaign missing") <$> getCampaign `shouldReturn` 4
      choose "read the conclusion and complete the campaign" (isLabel "$label.continue")
      (.completed) . fromMaybe (error "campaign missing") <$> getCampaign `shouldReturn` 5
