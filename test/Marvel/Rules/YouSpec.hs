module Marvel.Rules.YouSpec (spec) where

import Data.List (findIndex)
import Marvel.Card.Side (Side (A))
import TestImport

-- | "You" on a player card is the player who would play or trigger it (RR "You"), not
-- the active player.
spec :: Spec
spec = describe "\"you\"" do
  it "a player who is not the active player is offered their own Backflip when attacked" do
    tg <- testGame ["captain-america", "spider-man"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      heroes [p, other]
      clearHand other
      backflip <- putInHand other "01003"
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks other)]
      passUntil (isPlay backflip)
      fmap fst <$> asked `shouldReturn` Just other
      choose "play Backflip" (isPlay backflip)
      pass
      damageOf other `shouldReturn` 0
      discardOf other `shouldReturn` [backflip]

  it "the active player is not offered their Backflip when another player is attacked" do
    tg <- testGame ["spider-man", "captain-america"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      heroes [p, other]
      clearHand p
      backflip <- putInHand p "01003"
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks other)]
      neverOffered (isPlay backflip)
      damageOf other >>= (`shouldSatisfy` (> 0))
      handOf p `shouldReturn` [backflip]

  it "a player who is not the active player blocks with their own shield" do
    tg <- testGame ["spider-man", "captain-america"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      heroes [p, other]
      clearHand other
      shield <- putIntoPlay other "03009"
      block <- putInHand other "03005"
      runNow (damageIdentity other GameRef (toDamage 5 FromAbility))
      fmap fst <$> asked `shouldReturn` Just other
      choose "block" (isPlay block)
      pass
      damageOf other `shouldReturn` 0
      upgradeIsExhausted (upgradeIdOf shield) `shouldReturn` True

  it "the active player does not block another player's damage with their shield" do
    tg <- testGame ["captain-america", "spider-man"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      heroes [p, other]
      clearHand p
      _ <- putIntoPlay p "03009"
      block <- putInHand p "03005"
      runNow (damageIdentity other GameRef (toDamage 2 FromAbility))
      neverOffered (isPlay block)
      damageOf other `shouldReturn` 2
 where
  heroes = runNow . map (\ident -> IdentityMessage ident (ChangedToForm A))
  passable c = isNoResponse c || isNoDefender c
  -- decline what is asked until a matching choice is offered
  passUntil p =
    asked >>= \case
      Just (ident, cs)
        | not (any p cs)
        , Just n <- findIndex passable cs ->
            answerQuestion ident n >> passUntil p
      _ -> pure ()
  -- decline everything asked, failing if a matching choice is ever offered
  neverOffered p =
    asked >>= \case
      Just (ident, cs) | Just n <- findIndex passable cs -> do
        any p cs `shouldBe` False
        answerQuestion ident n
        neverOffered p
      _ -> pure ()
