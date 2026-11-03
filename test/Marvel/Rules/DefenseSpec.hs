module Marvel.Rules.DefenseSpec (spec) where

import TestImport

spec :: Spec
spec = describe "defending" do
  it "resolves a teammate defender's retaliate once under that defender's window limit" do
    tg <- testGame ["spider-man","black-panther"]
    inGame tg do
      let attacked = player1 tg
          defender = player2 tg
      runNow [IdentityMessage attacked (ChangedToForm A),IdentityMessage defender (ChangedToForm A)]
      vid <- villainIdNow
      before <- villainDamageNow
      runNow [VillainMessage vid $ VillainAttacksWithoutBoost attacked]
      pass' attacked
      choose "ask Black Panther to defend" (isTargetChoice defender)
      choose "Black Panther defends for Spider-Man" (isTargetChoice defender)
      pass
      damageOf attacked `shouldReturn` 0
      damageOf defender `shouldReturn` 0
      villainDamageNow `shouldReturn` (before + 1)
      getEnemyAttack (EnemyVillainId vid) `shouldReturn` Nothing

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
