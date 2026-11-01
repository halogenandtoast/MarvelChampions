module Marvel.Encounter.SinisterAttackRestrictionsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Matchers
import Marvel.Query
import Marvel.Status
import TestImport

attachments :: CardCode -> GameAppT [AttachmentId]
attachments code = getsGame $ map fst . filter ((== code) . toCardCode . getCardDef . snd) . HashMap.toList . gameAttachments

spec :: Spec
spec = describe "Sinister Motives attack restrictions" do
  it "Webbed Up replaces a basic hero attack, discards itself, and stuns the hero" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- revealNow you Attachments.smWebbedUp
      pass
      choose "make the basic attack" (isHeroAttack you)
      pass
      attachments "27166" `shouldReturn` []
      selectAny (IdentityWithId you <> StunnedIdentity) `shouldReturn` True
      villainDamageNow `shouldReturn` 0
      exhausted <- (.playerIdentityExhausted) <$> playerAttrs you
      liftIO $ exhausted `shouldBe` True
  it "an existing stun replaces the attack before Webbed Up is discarded" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- revealNow you Attachments.smWebbedUp
      pass
      resolve [GiveStatus (IdentityRef you) StunnedStatus]
      choose "make the basic attack" (isHeroAttack you)
      pass
      remaining <- attachments "27166"
      liftIO $ length remaining `shouldBe` 1
      selectAny (IdentityWithId you <> StunnedIdentity) `shouldReturn` False
  it "Tracking Display blocks stale hero and ally defense declarations" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      cat <- putIntoPlay you "01002"
      _ <- revealNow you Attachments.smTrackingDisplay
      pass
      vid <- villainIdNow
      -- Open the attack before its defense question, then submit stale declarations
      -- directly; neither defender is exhausted or marks the attack defended.
      runNow [VillainMessage vid $ VillainAttacks you]
      pass
      resolve [IdentityMessage you $ SideMessage $ Defended $ EnemyVillainId vid, AllyMessage (allyIdOf cat) $ AllyDefended $ EnemyVillainId vid]
      exhausted <- (.playerIdentityExhausted) <$> playerAttrs you
      liftIO $ exhausted `shouldBe` False
  it "Webbed Up replaces an attack event after its resources have been paid" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- revealNow you Attachments.smWebbedUp
      pass
      kick <- putInHand you "01005"
      energy <- putInHand you "01088"
      genius <- putInHand you "01089"
      playWith kick [energy, genius]
      pass
      attachments "27166" `shouldReturn` []
      selectAny (IdentityWithId you <> StunnedIdentity) `shouldReturn` True
      villainDamageNow `shouldReturn` 0
      hand <- handOf you
      liftIO $ hand `shouldNotContain` [kick, energy, genius]
