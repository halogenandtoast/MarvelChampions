module Marvel.Rules.DefenseSpec (spec) where

import Marvel.Card.Side (Side (A))
import TestImport

spec :: Spec
spec = describe "defending" do
  it "another player's hero may defend, taking the attack" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      runNow [IdentityMessage p (ChangedToForm A), IdentityMessage other (ChangedToForm A)]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      pass' p
      choose "ask Captain Marvel to defend" (isTargetChoice other)
      -- her player decides
      fmap fst <$> asked `shouldReturn` Just other
      choose "Captain Marvel defends" (isTargetChoice other)
      pass
      damageOf p `shouldReturn` 0
      -- Rhino's 2 ATK and any boost, less Captain Marvel's 1 DEF, is hers
      damageOf other >>= (`shouldSatisfy` (> 0))

  it "another player may decline to defend, and is not asked again" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      runNow [IdentityMessage p (ChangedToForm A), IdentityMessage other (ChangedToForm A)]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      pass' p
      choose "ask Captain Marvel to defend" (isTargetChoice other)
      choose "she declines" (isLabel "$label.declineToDefend")
      fmap fst <$> asked `shouldReturn` Just p
      offers (isTargetChoice other) `shouldReturn` False

  it "when the attacked player does not defend, the other player decides whether they do" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      runNow [IdentityMessage p (ChangedToForm A), IdentityMessage other (ChangedToForm A)]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      pass' p
      choose "Spider-Man does not defend" isNoDefender
      fmap fst <$> asked `shouldReturn` Just other
      choose "Captain Marvel defends" (isTargetChoice other)
      pass
      damageOf p `shouldReturn` 0
      damageOf other >>= (`shouldSatisfy` (> 0))

  it "no one defends when every player declines" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      runNow [IdentityMessage p (ChangedToForm A), IdentityMessage other (ChangedToForm A)]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      pass' p
      choose "Spider-Man does not defend" isNoDefender
      choose "Captain Marvel declines" (isLabel "$label.declineToDefend")
      pass
      damageOf other `shouldReturn` 0
      damageOf p >>= (`shouldSatisfy` (> 0))
 where
  -- answer what comes before the defender is declared, such as Spider-Sense
  pass' ident =
    asked >>= \case
      Just (_, cs) | not (any isNoDefender cs), any isNoResponse cs -> choose "no response" isNoResponse >> pass' ident
      _ -> pure ()
