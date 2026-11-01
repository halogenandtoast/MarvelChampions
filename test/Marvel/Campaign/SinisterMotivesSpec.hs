module Marvel.Campaign.SinisterMotivesSpec (spec) where

import Data.HashSet qualified as HashSet
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Campaign
import Marvel.Campaign.SinisterMotives (reputationScore)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.SideScheme.Cards qualified as SideSchemes
import TestImport

attachCampaign :: TestGame -> CampaignMode -> Int -> [CampaignRecord] -> IO ()
attachCampaign tg mode completed records = inGame tg do
  let identities = map (toHeroCardCode . hero . starter) ["spider-man", "captain-marvel"]
      initial = (newCampaignOf SinisterMotives mode $ take (length tg.seats) identities) {completed}
      campaign = foldl' (flip recordInLog) initial records
  withGame_ $ campaignL ?~ campaign

campaignNow :: GameAppT Campaign
campaignNow = getsGame (fromMaybe (error "missing campaign") . gameCampaign)

logNow :: GameAppT SinisterLog
logNow = (.campaignLog.sinister) <$> campaignNow

cardChoice :: Choice -> Maybe CardCode
cardChoice = \case
  CardLabel code _ -> Just code
  _ -> Nothing

cardIs :: CardCode -> Choice -> Bool
cardIs code = (== Just code) . cardChoice

remainingHp :: IdentityId -> GameAppT Natural
remainingHp ident = do
  (remaining, _, _) <- getRemainingHitPoints ident
  pure remaining

encounters :: IdentityId -> GameAppT Int
encounters ident = length . (.playerIdentityEncounterCards) <$> playerAttrs ident

spec :: Spec
spec = describe "Sinister Motives campaign runtime" do
  it "briefs the campaign and its rules before any setup rewards" do
    tg <- testGame ["spider-man"]
    attachCampaign tg ExpertCampaign 0 []
    inGame tg do
      runNow [CampaignStep CampaignChoices]
      question <- openQuestion
      case question of
        Just (_, Read sections _) -> liftIO $ sections `shouldBe`
          [ "$story.sm.sandman.intro", "$story.sm.campaignMode", "$story.sm.expertCampaign"
          , "$story.sm.sandman.rules", "$story.sm.sandman.setup", "$story.sm.sandman.expertSetup"
          ]
        _ -> error "campaign briefing was not asked"

  it "scores every clean-board condition and counts all main schemes' threat" do
    tg <- testGame ["spider-man"]
    attachCampaign tg StandardCampaign 0 []
    inGame tg do
      mid <- mainSchemeId
      threat <- mainSchemeThreatNow
      resolve [MainSchemeMessage mid (MainSchemeThreatRemoved (toSource $ player1 tg) threat)]
      campaign <- campaignNow
      reputationScore campaign `shouldReturn` 5
      resolve [MainSchemeMessage mid (MainSchemePlaceThreat 1)]
      reputationScore campaign `shouldReturn` 4

  it "counts acceleration tokens, but not printed acceleration icons, for reputation" do
    tg <- testGame ["spider-man"]
    attachCampaign tg StandardCampaign 0 []
    inGame tg do
      mid <- mainSchemeId
      threat <- mainSchemeThreatNow
      resolve [MainSchemeMessage mid (MainSchemeThreatRemoved (toSource $ player1 tg) threat)]
      card <- encounterCard SideSchemes.bombScare
      resolve [RevealEncounterCard (player1 tg) card]
      campaign <- campaignNow
      -- Bomb Scare costs the no-side-scheme point, but its printed icon is not a token.
      reputationScore campaign `shouldReturn` 4
      resolve [PlaceAccelerationToken mid]
      reputationScore campaign `shouldReturn` 3

  it "deals three random Tech choices per player and returns the unchosen cards to the next pool" do
    tg <- testGame ["spider-man", "captain-marvel"]
    attachCampaign tg StandardCampaign 0 []
    inGame tg do
      runNow [CampaignStep $ CampaignSetupThen "sinister:reward1"]
      first <- maybe [] (mapMaybe cardChoice . snd) <$> asked
      liftIO $ length first `shouldBe` 3
      let chosen = fromMaybe (error "missing Tech offer") (headMay first)
      choose "choose first Tech" (cardIs chosen)
      second <- maybe [] (mapMaybe cardChoice . snd) <$> asked
      liftIO do
        length second `shouldBe` 3
        second `shouldSatisfy` notElem chosen
      choose "decline second Tech" (isLabel "$label.pass")
      log <- logNow
      liftIO $ map (.shieldTech) log.sinisterPlayers `shouldBe` [Just chosen, Nothing]
      -- A victory reward is recorded for the next game, not inserted into this game.
      cards <- getCardsOwnedBy (player1 tg)
      liftIO $ map cdCardCode cards `shouldSatisfy` notElem chosen

  it "resolves a deferred Tech reward before the next game's setup-card step" do
    tg <- testGame ["spider-man"]
    attachCampaign tg StandardCampaign 1 [RecordChoice "01001a" ChooseSinisterTech]
    inGame tg do
      runNow [CampaignStep CampaignChoices]
      choose "continue briefing" (isLabel "$label.continue")
      offered <- maybe [] (mapMaybe cardChoice . snd) <$> asked
      let chosen = fromMaybe (error "missing deferred Tech offer") (headMay offered)
      choose "choose deferred Tech" (cardIs chosen)
      campaign <- campaignNow
      liftIO $ maybe [] (.pending) (campaignPlayer "01001a" campaign) `shouldBe` []
      cards <- getCardsOwnedBy (player1 tg)
      liftIO $ map cdCardCode cards `shouldSatisfy` elem chosen

  it "offers Planning Ahead cards held under a support controlled by another player" do
    tg <- testGame ["spider-man", "captain-marvel"]
    attachCampaign tg StandardCampaign 2 []
    inGame tg do
      holder <- putIntoPlay (player1 tg) "01015"
      event <- putInHand (player1 tg) "01031"
      resolve
        [ RemoveFromGame (toRef event)
        , SupportMessage (supportIdOf holder) (AttachPlayableEvent event)
        , SupportMessage (supportIdOf holder) (SupportChangedController $ player2 tg)
        ]
      runNow [CampaignStep $ CampaignSetupThen "sinister:reward17"]
      choose "plan the event owned by the first player" (cardIs "01031")
      campaign <- campaignNow
      liftIO $ ((.planningAhead) =<< sinisterPlayer "01001a" campaign) `shouldBe` Just "01031"

  it "searches for each reputation minion after the preceding player's choice has removed it" do
    tg <- testGame ["spider-man", "captain-marvel"]
    attachCampaign tg StandardCampaign 1 [RecordReputation 9]
    inGame tg do
      -- Leave a non-minion after the fallback deal: an exhausted deck with no
      -- discard pile ends this synthetic game before its dealt-card message runs.
      (deck, _) <- setEncounterDeck [Minions.hydraMercenary, Treacheries.advance, Treacheries.imTough] []
      before <- encounters (player2 tg)
      runNow [CampaignStep $ CampaignSetupThen "sinister:reputationRight"]
      let minion = testHead deck
      choose "engage the only minion" (isTargetChoice minion)
      -- Finish its enter-play responses before the next player's search resolves.
      pass
      first <- playerAttrs (player1 tg)
      second <- playerAttrs (player2 tg)
      liftIO do
        HashSet.size first.playerIdentityMinions `shouldBe` 1
        HashSet.size second.playerIdentityMinions `shouldBe` 0
      encounters (player2 tg) `shouldReturn` before + 1

  it "restores expert hit points and charges exactly two encounters to heal before the Six" do
    tg <- testGame ["spider-man"]
    attachCampaign tg ExpertCampaign 3 [RecordHitPoints "01001a" 2]
    inGame tg do
      before <- encounters (player1 tg)
      runNow [CampaignStep $ CampaignSetupThen "sinister:expert"]
      choose "accept expert healing" (isLabel "$label.sinisterTakeEncountersToHeal")
      remainingHp (player1 tg) `shouldReturn` 10
      encounters (player1 tg) `shouldReturn` before + 2

  it "lets a surviving player keep recorded damage without taking healing encounters" do
    tg <- testGame ["spider-man"]
    attachCampaign tg ExpertCampaign 3 [RecordHitPoints "01001a" 2]
    inGame tg do
      before <- encounters (player1 tg)
      runNow [CampaignStep $ CampaignSetupThen "sinister:expert"]
      choose "keep recorded hit points" (isLabel "$label.keepHitPoints")
      remainingHp (player1 tg) `shouldReturn` 2
      encounters (player1 tg) `shouldReturn` before

  it "rejoins an eliminated expert player with the encounter penalty and no optional heal question" do
    tg <- testGame ["spider-man"]
    attachCampaign tg ExpertCampaign 3 [RecordHitPoints "01001a" 0]
    inGame tg do
      before <- encounters (player1 tg)
      runNow [CampaignStep $ CampaignSetupThen "sinister:expert"]
      remainingHp (player1 tg) `shouldReturn` 10
      encounters (player1 tg) `shouldReturn` before + 2
      offers (isLabel "$label.keepHitPoints") `shouldReturn` False

  it "floors a negative total gain at zero and preserves previously marked reputation" do
    tg <- testGame ["spider-man", "captain-marvel"]
    attachCampaign tg StandardCampaign 0 [RecordReputation 7]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Minions.hydraMercenary
      side <- encounterCard SideSchemes.bombScare
      penalty <- encounterCard Attachments.smSnitchesGetStitches
      mid <- mainSchemeId
      resolve
        [ RevealedEncounterCard p side
        , MainSchemeMessage mid (MainSchemePlaceThreat 1)
        , PlaceAccelerationToken mid
        , PlaceAccelerationToken mid
        , IdentityMessage (player2 tg) IdentityDefeated
        , AddCardToVictoryDisplay penalty
        ]
      campaign <- campaignNow
      reputationScore campaign `shouldReturn` (-1)
      runNow [CampaignStep CampaignVictory]
      choose "continue victory instructions" (isLabel "$label.continue")
      (.reputation) <$> logNow `shouldReturn` 7
