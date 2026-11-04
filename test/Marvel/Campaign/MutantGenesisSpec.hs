module Marvel.Campaign.MutantGenesisSpec (spec) where

import Marvel.Campaign
import TestImport

campaignNow :: GameAppT Campaign
campaignNow = getsGame (fromMaybe (error "missing campaign") . gameCampaign)

spec :: Spec
spec = describe "Mutant Genesis campaign guide" do
  for_ (zip [0..] ["sabretooth","projectWideawake","masterMold","mansionAttack","magneto"]) $ \(completed,chapter) ->
    for_ [StandardCampaign,ExpertCampaign] $ \mode ->
      it ("shows the introduction and relevant rules for " <> show chapter <> " in " <> show mode) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let c = (newCampaignOf MutantGenesis mode ["01001a"]) {completed}
              key section = "$story.mutantGenesis." <> chapter <> "." <> section
              expected = [key "intro"]
                <> ["$story.mutantGenesis." <> section | completed == 0, section <- ["campaignMode","roles","futurePast","keywords"]]
                <> ["$story.mutantGenesis.expertCampaign" | completed == 0 && mode == ExpertCampaign]
                <> [key "rules",key "setup"]
                <> ["$story.mutantGenesis.expertSetup" | completed > 0 && mode == ExpertCampaign]
          withGame_ $ campaignL ?~ c
          runNow [CampaignStep CampaignChoices]
          openQuestion >>= \case
            Just (_,Read sections _) -> sections `shouldBe` expected
            _ -> error "Mutant Genesis briefing not shown"
          (.campaignLog.mutantGenesis.mutantPlayers) <$> campaignNow `shouldReturn` [MutantPlayer "01001a" Nothing]

  it "explains roles before asking a player to choose one" do
    tg <- testGame ["spider-man"]
    inGame tg do
      withGame_ $ campaignL ?~ newCampaignOf MutantGenesis StandardCampaign ["01001a"]
      runNow [CampaignStep CampaignChoices]
      offers (isLabel "$label.mutantRoleBrawler") `shouldReturn` False
      choose "read the campaign briefing" (isLabel "$label.continue")
      offers (isLabel "$label.mutantRoleBrawler") `shouldReturn` True

  for_ [(0,"sabretooth"),(4,"magneto")] $ \(completed,chapter) ->
    for_ [StandardCampaign,ExpertCampaign] $ \mode ->
      it ("shows victory guidance before advancing " <> show chapter <> " in " <> show mode) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let c = (newCampaignOf MutantGenesis mode ["01001a"]) {completed}
              expected = ["$story.mutantGenesis." <> chapter <> ".victory"]
                <> ["$story.mutantGenesis.expertVictory" | completed < 4 && mode == ExpertCampaign]
                <> ["$story.mutantGenesis.conclusion" | completed == 4]
          withGame_ $ campaignL ?~ c
          runNow [CampaignStep CampaignVictory]
          openQuestion >>= \case
            Just (_,Read sections _) -> sections `shouldBe` expected
            _ -> error "Mutant Genesis victory instructions not shown"
          (.completed) <$> campaignNow `shouldReturn` completed
          choose "read the victory instructions" (isLabel "$label.continue")
          (.completed) <$> campaignNow `shouldReturn` completed + 1
