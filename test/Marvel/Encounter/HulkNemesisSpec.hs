module Marvel.Encounter.HulkNemesisSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

hulk :: IO TestGame
hulk = testGame ["hulk"]

toHulk :: IdentityId -> GameAppT ()
toHulk p = changeForm p >> pass

spec :: Spec
spec = describe "Hulk nemesis" do
  describe "Abomination (10026)" do
    it "after attacking you, discards your top card: a [physical] resource deals you 2 damage" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        toHulk p
        abomination <- addMinion p Minions.abomination
        [strength] <- stackDeck p ["01090"]
        runNow [MinionMessage abomination (MinionAttacks p)]
        choose "no defender" isNoDefender
        choose "Abomination's forced response" isAnyAbility
        pass
        discardOf p >>= (`shouldSatisfy` elem strength)
        damageOf p `shouldReturn` 5

    it "deals nothing more when the card has no [physical] resource" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        toHulk p
        abomination <- addMinion p Minions.abomination
        [genius] <- stackDeck p ["01089"]
        runNow [MinionMessage abomination (MinionAttacks p)]
        choose "no defender" isNoDefender
        choose "Abomination's forced response" isAnyAbility
        pass
        discardOf p >>= (`shouldSatisfy` elem genius)
        damageOf p `shouldReturn` 3

    it "\"you\" is the player whose hero defended the attack (RR Defend)" do
      tg <- testGame ["hulk", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        runNow [IdentityMessage p (ChangedToForm A), IdentityMessage other (ChangedToForm A)]
        abomination <- addMinion p Minions.abomination
        _ <- stackDeck p ["01089"]
        [strength] <- stackDeck other ["01090"]
        runNow [MinionMessage abomination (MinionAttacks p)]
        choose "ask Captain Marvel to defend" (isTargetChoice other)
        choose "Captain Marvel defends" (isTargetChoice other)
        choose "Abomination's forced response" isAnyAbility
        pass
        discardOf other >>= (`shouldSatisfy` elem strength)
        discardOf p `shouldReturn` []
        damageOf p `shouldReturn` 0
        -- 3 ATK less her 1 DEF, then 2 for the [physical] resource
        damageOf other `shouldReturn` 4

  describe "Total Destruction (10027)" do
    it "enters with 2 threat per player, which cannot be removed while Abomination is in play" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.totalDestruction
        pass
        sideSchemeThreat scheme `shouldReturn` 2
        abomination <- addMinion p Minions.abomination
        resolve [SideSchemeMessage scheme (SideSchemeThwarted GameRef 1)]
        sideSchemeThreat scheme `shouldReturn` 2
        -- not offered to thwart
        toHulk p
        clearHand p
        rescue <- putInHand p "10019"
        _ <- putInHand p "01090"
        choose "play To the Rescue!" (isPlay rescue)
        chooseIf isFinishPayment
        offers (isTargetChoice scheme) `shouldReturn` False
        pass
        -- once he is gone it can be
        resolve [MinionMessage abomination MinionDefeated]
        resolve [SideSchemeMessage scheme (SideSchemeThwarted GameRef 1)]
        sideSchemeThreat scheme `shouldReturn` 1

  describe "Clash of the Titans (10028)" do
    it "the enemy with the highest ATK attacks the hero with the highest ATK" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        toHulk p
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.clashOfTheTitans
        choose "no defender" isNoDefender
        pass
        -- Rhino's 2 ATK and the mercenary's boost icon
        damageOf p `shouldReturn` 3

    it "attacks the ally with the highest ATK, who takes the undefended damage" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "10011"
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.clashOfTheTitans
        choose "no defender" isNoDefender
        pass
        (.allyDamage) <$> allyIn "10011" `shouldReturn` 3
        damageOf p `shouldReturn` 0

    it "gains surge when no attack is made" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.clashOfTheTitans
        pass
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)

    it "gains surge when the enemy is stunned" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        toHulk p
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainStunned GameRef)]
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.clashOfTheTitans
        pass
        damageOf p `shouldReturn` 0
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)
