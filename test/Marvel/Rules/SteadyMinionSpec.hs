module Marvel.Rules.SteadyMinionSpec (spec) where

import Data.Aeson (Result (..), Value (Object), fromJSON, toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Keyword (Keyword (Steady))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Runner qualified as Minion
import Marvel.Status (StatusCard (StunnedStatus))
import Marvel.Stats (Sch (..))
import TestImport

attrsOf :: MinionId -> GameAppT (Attrs Minion)
attrsOf mid = getsGame (toAttrs . fromMaybe (error "missing Steady minion") . HashMap.lookup mid . gameMinions)

steadyGame :: IO TestGame
steadyGame = do
  tg <- testGameAgainst "01113" ["spider-man-miles-morales"]
  inGame tg do
    initial <- getsGame (HashMap.keys . gameMinions)
    resolve [RemoveFromGame $ toRef mid | mid <- initial]
  pure tg

spec :: Spec
spec = describe "Steady minions" do
  it "retain one stun while attacking, cancel the attack with two, and remove both after cancellation" do
    tg <- steadyGame
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.smRhino
      resolve [MinionMessage mid (MinionStunned GameRef)]
      first <- attrsOf mid
      Minion.isStunned first `shouldReturn` False
      minionStunned first `shouldBe` True
      resolve [MinionMessage mid (MinionAttacks p)]
      damageOf p `shouldReturn` 3
      minionStunned <$> attrsOf mid `shouldReturn` True
      resolve [MinionMessage mid (MinionStunned GameRef)]
      second <- attrsOf mid
      Minion.isStunned second `shouldReturn` True
      minionAdditionalStun second `shouldBe` True
      resolve [MinionMessage mid (MinionAttacks p)]
      damageOf p `shouldReturn` 3
      after <- attrsOf mid
      (minionStunned after, minionAdditionalStun after) `shouldBe` (False,False)

  it "similarly requires two confuse cards and clears both when the scheme is cancelled" do
    tg <- steadyGame
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.smRhino
      withGame_ $ entitiesL . minionsL . ix mid %~ Minion.overMinion (\attrs -> attrs {minionScheme = Sch 2})
      resolve [MinionMessage mid (MinionConfused GameRef)]
      confused <- Minion.isConfused =<< attrsOf mid
      confused `shouldBe` False
      before <- mainSchemeThreatNow
      resolve [MinionMessage mid MinionSchemes]
      mainSchemeThreatNow `shouldReturn` before + 2
      minionConfused <$> attrsOf mid `shouldReturn` True
      resolve [MinionMessage mid (MinionConfused GameRef), MinionMessage mid MinionSchemes]
      mainSchemeThreatNow `shouldReturn` before + 2
      after <- attrsOf mid
      (minionConfused after, minionAdditionalConfuse after) `shouldBe` (False,False)

  it "removes one status card at a time when an effect removes a status" do
    tg <- steadyGame
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.smRhino
      resolve [MinionMessage mid (MinionStunned GameRef), MinionMessage mid (MinionStunned GameRef)]
      resolve [RemoveStatus (toRef mid) StunnedStatus]
      remaining <- attrsOf mid
      (minionStunned remaining, minionAdditionalStun remaining) `shouldBe` (True,False)
      Minion.isStunned remaining `shouldReturn` False
      resolve [RemoveStatus (toRef mid) StunnedStatus]
      minionStunned <$> attrsOf mid `shouldReturn` False

  it "keeps only one status when Steady is lost, and that status becomes effective" do
    tg <- steadyGame
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.smRhino
      resolve [MinionMessage mid (MinionStunned GameRef), MinionMessage mid (MinionStunned GameRef)]
      withGame_ $ entitiesL . minionsL . ix mid %~ Minion.overMinion (\attrs -> attrs {minionCardDef = (minionCardDef attrs) {cdKeywords = HashSet.delete Steady $ cdKeywords $ minionCardDef attrs}})
      resolve [MinionMessage mid MinionCheckHitPoints]
      after <- attrsOf mid
      minionAdditionalStun after `shouldBe` False
      Minion.isStunned after `shouldReturn` True

  it "counts a single stunned status for Arachnobatics even when the Steady minion is not stunned" do
    tg <- steadyGame
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      clearHand p
      mid <- addMinion p Minions.smRhino
      resolve [MinionMessage mid (MinionStunned GameRef)]
      event <- putInHand p "27031"
      mental <- putInHand p "27031"
      playWith event [mental]
      choose "attack Rhino" (isTargetChoice mid)
      pass
      minionDamageOf mid `shouldReturn` Just 5

  it "loads old minion saves without extra status flags and round-trips new counts" do
    tg <- steadyGame
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.smRhino
      resolve [MinionMessage mid (MinionStunned GameRef), MinionMessage mid (MinionStunned GameRef)]
      attrs <- attrsOf mid
      fromJSON (toJSON attrs) `shouldBe` Success attrs
      case toJSON attrs of
        Object object -> case fromJSON (Object $ KeyMap.delete "minionAdditionalStun" $ KeyMap.delete "minionAdditionalConfuse" object) of
          Success (old :: Attrs Minion) -> (minionAdditionalStun old, minionAdditionalConfuse old) `shouldBe` (False,False)
          Error err -> error err
        _ -> error "expected minion object"
