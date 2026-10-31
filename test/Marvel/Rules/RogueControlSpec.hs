module Marvel.Rules.RogueControlSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Minion.Cards qualified as Minions
import TestImport

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == n
  _ -> False

playKarmaOn :: IdentityId -> MinionId -> GameAppT PlayerCard
playKarmaOn you mid = do
  karma <- putInHand you "38011"
  energy <- putInHand you "01088"
  genius <- putInHand you "01089"
  playWith karma [energy, genius]
  choose "Karma takes control" (cardAbility karma 1)
  choose "the minion" (isTargetChoice mid)
  pure karma

playMindControlOn :: IdentityId -> MinionId -> GameAppT PlayerCard
playMindControlOn you mid = do
  control <- putInHand you "34009"
  energy <- putInHand you "01088"
  genius <- putInHand you "01089"
  playOn control mid [energy, genius]
  pass
  pure control

controlSource :: MinionId -> GameAppT (Maybe Source)
controlSource mid = getsGame $ maybe Nothing (Ally.allyControlSource . toAttrs) . HashMap.lookup (AllyId $ unMinionId mid) . gameAllies

spec :: Spec
spec = describe "Rogue control source ownership" do
  it "keeps the newer Mind Control conversion when Karma leaves" do
    tg <- testGame ["rogue"]
    let p = player1 tg
    inGame tg do
      clearHand p
      -- Fallen Warrior makes a five-cost ally a minion before Karma is played.
      -- Its printed cost then lets Beguiled select it over four-cost Karma.
      [giant] <- stackDeck p ["12012"]
      _ <- revealNow p Attachments.fallenWarrior
      pass
      let mid = MinionId $ unCardId $ pcCardId giant
      karma <- playKarmaOn p mid
      controlSource mid `shouldReturn` Just (AllyRef $ allyIdOf karma)
      _ <- revealNow p Attachments.beguiled
      pass
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` True
      control <- playMindControlOn p mid
      controlSource mid `shouldReturn` Just (UpgradeRef $ upgradeIdOf control)
      resolve [ReturnToHand $ AllyRef $ allyIdOf karma]
      controlSource mid `shouldReturn` Just (UpgradeRef $ upgradeIdOf control)
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
      resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf control]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` True

  it "keeps the newer Karma conversion when an older Mind Control leaves" do
    tg <- testGame ["rogue"]
    let p = player1 tg
    inGame tg do
      clearHand p
      mid <- addMinion p Minions.hydraMercenary
      control <- playMindControlOn p mid
      controlSource mid `shouldReturn` Just (UpgradeRef $ upgradeIdOf control)
      _ <- revealNow p Attachments.beguiled
      pass
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` True
      karma <- playKarmaOn p mid
      controlSource mid `shouldReturn` Just (AllyRef $ allyIdOf karma)
      resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf control]
      controlSource mid `shouldReturn` Just (AllyRef $ allyIdOf karma)
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
      resolve [ReturnToHand $ AllyRef $ allyIdOf karma]
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` True

  it "loads old controlled allies without a control-source field" do
    tg <- testGame ["rogue"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01067"
      Just entity <- getsGame (HashMap.lookup (allyIdOf ally) . gameAllies)
      let saved = case toJSON (toAttrs entity) of
            Object o -> Object $ KeyMap.delete "allyControlSource" o
            _ -> error "ally attributes must be an object"
      case fromJSON saved of
        Success (loaded :: Attrs Ally) -> Ally.allyControlSource loaded `shouldBe` Nothing
        Error err -> error err
