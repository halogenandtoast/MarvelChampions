module Marvel.Rules.PsylockeTargetSpec (spec) where

import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import TestImport

-- Decline optional reactions while letting forced effects finish before the next
-- independently chosen target of Flurry of Blades.
advanceTo :: (Choice -> Bool) -> GameAppT ()
advanceTo wanted = go (100 :: Int)
 where
  go 0 = error "Psylocke target fixture did not reach the next target"
  go n = do
    reached <- offers wanted
    unless reached $ openQuestion >>= \case
      Just (you, question) -> do
        let passing choice = isNoResponse choice || isNoDefender choice || isLabel "$label.declineToDefend" choice
        answerQuestion you (fromMaybe 0 $ headMay [i | (i, choice) <- zip [0..] (choicesOf question), passing choice])
        go (n - 1)
      Nothing -> error "Psylocke target fixture ended before the next target"

asHero :: IdentityId -> GameAppT ()
asHero you = clearHand you >> changeForm you >> pass

playFlurry :: IdentityId -> GameAppT ()
playFlurry you = do
  card <- putInHand you "41004"
  double <- putInHand you "01089"
  single <- putInHand you "01091"
  playWith card [double, single]

spec :: Spec
spec = describe "Psylocke target interrupts" do
  it "Speed Demon counterattacks when a later Flurry target chooses him only for confusion" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let you = player1 tg
      asHero you
      demon <- addMinion you Minions.hood24046
      villain <- villainIdNow
      playFlurry you
      choose "initial damage to villain" (isTargetChoice villain)
      choose "confuse Speed Demon" (isTargetChoice demon)
      advanceTo (isTargetChoice villain)
      damageOf you `shouldReturn` 2
      minionDamageOf demon `shouldReturn` Just 0
      choose "second confusion to villain" (isTargetChoice villain)
      pass

  it "Titanium Exoskeleton resets its damage cap when its host is a later target of a new attack" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let you = player1 tg
      asHero you
      knife <- testHead <$> upgradesWith "41002a"
      L.runQueueT (L.capture $ L.flipPsiEnergy $ upgradeId knife) >>= resolve
      host <- addMinion you Minions.tombstone
      _ <- revealNow you Attachments.ne40091
      pass
      villain <- villainIdNow
      playFlurry you
      choose "first attack initial villain target" (isTargetChoice villain)
      choose "first attack confusion to armored minion" (isTargetChoice host)
      choose "first attack damage to armored minion" (isTargetChoice host)
      pass
      minionDamageOf host `shouldReturn` Just 2
      playFlurry you
      choose "new attack initial villain target" (isTargetChoice villain)
      choose "new attack confusion to armored minion" (isTargetChoice host)
      choose "new attack damage to armored minion" (isTargetChoice host)
      pass
      minionDamageOf host `shouldReturn` Just 4
