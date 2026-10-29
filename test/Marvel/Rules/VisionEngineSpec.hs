module Marvel.Rules.VisionEngineSpec (spec) where

import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Matchers
import Marvel.Query
import TestImport

spec :: Spec
spec = describe "Vision engine rules" do
  it "matches attack damage to a friendly identity but not ability damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      let matcher = W.CharacterWouldTakeAttackDamage (CharacterControlledBy $ IdentityWithId p)
      W.windowMatches matcher (W.Window W.When $ W.IdentityTakeDamage p $ toDamage 2 FromAttack) GameRef `shouldReturn` True
      W.windowMatches matcher (W.Window W.When $ W.IdentityTakeDamage p $ toDamage 2 FromAbility) GameRef `shouldReturn` False
      W.windowMatches matcher (W.Window W.When $ W.IdentityTakeDamage p $ toDamage 0 FromAttack) GameRef `shouldReturn` False

  it "matches attack damage to a friendly ally but excludes another player's ally" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
    inGame tg do
      own <- putIntoPlay p "01068"
      other <- putIntoPlay (player2 tg) "01083"
      let matcher = W.CharacterWouldTakeAttackDamage (CharacterControlledBy $ IdentityWithId p)
      W.windowMatches matcher (W.Window W.When $ W.AllyTakeDamage (allyIdOf own) $ toDamage 2 FromAttack) GameRef `shouldReturn` True
      W.windowMatches matcher (W.Window W.When $ W.AllyTakeDamage (allyIdOf other) $ toDamage 2 FromAttack) GameRef `shouldReturn` False

  it "finds an enemy attachment with a printed Hero Action" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      _ <- revealNow (player1 tg) Attachments.enhancedIvoryHorn
      pass
      selectList (AttachmentOnEnemyWithHeroAbility $ EnemyVillainId vid) >>= (`shouldSatisfy` not . null)
