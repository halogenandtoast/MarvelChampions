module Marvel.Rules.PsylockeRedirectSpec (spec) where

import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Message.Lifted qualified as L
import TestImport

-- Follow the forced attachment interrupt without declining the optional attack
-- increases that should remain available after the new target is selected.
advanceTo :: (Choice -> Bool) -> GameAppT ()
advanceTo wanted = go (100 :: Int)
 where
  go 0 = error "Psylocke redirect fixture did not reach its forced choice"
  go n = do
    reached <- offers wanted
    unless reached $ openQuestion >>= \case
      Just (you, question) -> do
        let passing choice = isNoResponse choice || isNoDefender choice || isLabel "$label.declineToDefend" choice
        answerQuestion you (fromMaybe 0 $ headMay [i | (i, choice) <- zip [0..] (choicesOf question), passing choice])
        go (n - 1)
      Nothing -> error "Psylocke redirect fixture ended before its forced choice"

oneKatanaAndIllusion :: IdentityId -> GameAppT ()
oneKatanaAndIllusion you = do
  clearHand you
  changeForm you
  pass
  knife <- head <$> upgradesWith "41002a"
  L.runQueueT (L.capture $ L.flipPsiEnergy $ upgradeId knife) >>= resolve
  card <- encounterCard Attachments.psionicIllusion
  resolve [RevealEncounterCard you card]
  _ <- stackDeck you ["41020"]
  pure ()

redirectBasicAttackToSelf :: IdentityId -> GameAppT ()
redirectBasicAttackToSelf you = do
  choose "make a basic attack" (isHeroAttack you)
  pass
  chooseVillain
  advanceTo (isLabel "$label.mental")
  choose "name mental and discard a wild" (isLabel "$label.mental")
  choose "redirect the attack to Psylocke" (isTargetChoice you)

spec :: Spec
spec = describe "Psylocke redirected attack interrupts" do
  it "does not offer Float Like a Butterfly after the confused enemy stops being the target" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let you = player1 tg
      oneKatanaAndIllusion you
      butterfly <- putIntoPlay you "41017"
      villain <- villainIdNow
      resolve [VillainMessage villain (VillainConfused GameRef)]
      redirectBasicAttackToSelf you
      offers (isAbilityOfCard butterfly) `shouldReturn` False
      pass
      villainDamageNow `shouldReturn` 0
      damageOf you `shouldReturn` 2

  it "allows Warrior Skill to increase damage to the replacement friendly target" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let you = player1 tg
      oneKatanaAndIllusion you
      skill <- putIntoPlay you "35016"
      redirectBasicAttackToSelf you
      offers (isAbilityOfCard skill) `shouldReturn` True
      choose "Warrior Skill boosts the redirected attack" (isAbilityOfCard skill)
      pass
      villainDamageNow `shouldReturn` 0
      damageOf you `shouldReturn` 3
      counters <- upgradesWith "35016"
      map upgradeUses counters `shouldBe` [2]

  it "allows Directed Force when the redirected basic attack retains Psi-Katana's piercing" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let you = player1 tg
      oneKatanaAndIllusion you
      force <- putInHand you "41019"
      redirectBasicAttackToSelf you
      offers (isPlay force) `shouldReturn` True
      playWith force []
      pass
      villainDamageNow `shouldReturn` 0
      damageOf you `shouldReturn` 4

  it "does not reopen Warrior Skill when Flurry chooses another enemy after its first target was redirected" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let you = player1 tg
      oneKatanaAndIllusion you
      skill <- putIntoPlay you "35016"
      flurry <- putInHand you "41004"
      double <- putInHand you "01089"
      single <- putInHand you "01091"
      playWith flurry [double, single]
      chooseVillain
      advanceTo (isLabel "$label.mental")
      choose "name mental and discard a wild" (isLabel "$label.mental")
      choose "redirect Flurry's first target to Psylocke" (isTargetChoice you)
      offers (isAbilityOfCard skill) `shouldReturn` True
      pass
      chooseVillain
      offers (isAbilityOfCard skill) `shouldReturn` False
      chooseVillain
      pass
      damageOf you `shouldReturn` 2
      villainDamageNow `shouldReturn` 2
      map upgradeUses <$> upgradesWith "35016" `shouldReturn` [3]

  it "allows Sharpshooter to increase a ranged event's damage after its target becomes friendly" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let you = player1 tg
      oneKatanaAndIllusion you
      sharp <- putIntoPlay you "40064"
      attack <- putInHand you "41030"
      payment <- putInHand you "01089"
      _ <- stackDeck you ["41020", "01088"]
      playWith attack [payment]
      chooseVillain
      advanceTo (isLabel "$label.mental")
      choose "name mental and discard a wild" (isLabel "$label.mental")
      choose "redirect the ranged attack to Psylocke" (isTargetChoice you)
      offers (isAbilityOfCard sharp) `shouldReturn` True
      choose "Sharpshooter boosts the redirected attack" (isAbilityOfCard sharp)
      choose "discard a card with two printed resources" (isLabel $ countLabel "discardTopOfDeck" 1)
      pass
      villainDamageNow `shouldReturn` 0
      damageOf you `shouldReturn` 6
