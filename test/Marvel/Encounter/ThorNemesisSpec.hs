module Marvel.Encounter.ThorNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

thor :: IO TestGame
thor = testGame ["thor"]

isToughMinion :: MinionId -> GameAppT (Maybe Bool)
isToughMinion m = getsGame (fmap ((.minionTough) . toAttrs) . HashMap.lookup m . gameMinions)

spec :: Spec
spec = describe "Thor's nemesis set" do
  describe "Family Feud (06027)" do
    it "places 1 more threat for each Asgard card in play" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        -- Odinson and Asgard
        _ <- putIntoPlay p "06007"
        feud <- revealNow p SideSchemes.familyFeud
        pass
        sideSchemeThreat (sideSchemeOf feud) `shouldReturn` 4

    it "counts every type of Asgard card: identity, ally, support, upgrade and minion" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        -- Thor, Lady Sif, Asgard, Mjolnir and Loki
        _ <- putIntoPlay p "06002"
        _ <- putIntoPlay p "06007"
        _ <- putIntoPlay p "06009"
        _ <- addMinion p Minions.loki
        pass
        feud <- revealNow p SideSchemes.familyFeud
        pass
        sideSchemeThreat (sideSchemeOf feud) `shouldReturn` 7

  describe "Loki (06028)" do
    it "heals all his damage instead of being defeated when a treachery is discarded" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        loki <- addMinion p Minions.loki
        pass
        _ <- stackEncounterDeck [Treacheries.advance]
        resolve [MinionMessage loki (MinionDamaged GameRef (toDamage 5 FromAbility))]
        minionDamageOf loki `shouldReturn` Just 0
        map toCardCode <$> encounterDiscardNow >>= (`shouldSatisfy` elem (toCardCode Treacheries.advance))

    it "is defeated when the card discarded is not a treachery" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        loki <- addMinion p Minions.loki
        pass
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        resolve [MinionMessage loki (MinionDamaged GameRef (toDamage 5 FromAbility))]
        minionDamageOf loki `shouldReturn` Nothing

    it "saved from Hammer Throw, takes no overkill to the villain (RR, Overkill)" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        mjolnir <- putIntoPlay p "06009"
        loki <- addMinion p Minions.loki
        chooseIf (isNamedAbility "\"Have at thee!\"")
        pass
        _ <- stackEncounterDeck [Treacheries.advance]
        throw <- putInHand p "06005"
        payment <- traverse (putInHand p) ["01088", "01089", "01090"]
        playWith throw payment
        choose "throw at Loki" (isTargetChoice loki)
        choose "Loki's interrupt" (isAbilityOf loki)
        pass
        minionDamageOf loki `shouldReturn` Just 0
        villainDamageNow `shouldReturn` 0
        codes <- map toCardCode <$> handOf p
        codes `shouldSatisfy` elem (toCardCode mjolnir)

    it "defeated by Hammer Throw, the damage past his hit points goes to the villain" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        _ <- putIntoPlay p "06009"
        loki <- addMinion p Minions.loki
        chooseIf (isNamedAbility "\"Have at thee!\"")
        pass
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        throw <- putInHand p "06005"
        payment <- traverse (putInHand p) ["01088", "01089", "01090"]
        playWith throw payment
        choose "throw at Loki" (isTargetChoice loki)
        choose "Loki's interrupt" (isAbilityOf loki)
        pass
        minionDamageOf loki `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 4

  describe "Frost Giant (06029)" do
    it "enters play with a tough status card" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        giant <- addMinion p Minions.frostGiant
        isToughMinion giant `shouldReturn` Just True

    it "as a boost, stuns the character the villain's attack damages" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- stackEncounterDeck [Minions.frostGiant]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        pass
        damageOf p `shouldReturn` 3
        isStunned p `shouldReturn` True
        getsGame (HashMap.null . gameMinions) `shouldReturn` True

    it "as a boost, does not stun when the attack deals no damage" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        resolve [IdentityMessage p IdentityBecomeTough]
        _ <- stackEncounterDeck [Minions.frostGiant]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        pass
        damageOf p `shouldReturn` 0
        isStunned p `shouldReturn` False

    it "as a boost for a scheme, stuns no one" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- stackEncounterDeck [Minions.frostGiant]
        vid <- villainIdNow
        resolve [VillainMessage vid VillainSchemes]
        isStunned p `shouldReturn` False

  describe "Trickster (06030)" do
    it "discards the top 3 cards of the deck: 1 threat for each card type among them" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        _ <- stackDeck p ["06002", "06003", "06021"]
        before <- mainSchemeThreat
        _ <- revealNow p Treacheries.trickster
        pass
        mainSchemeThreat `shouldReturn` (before + 2)
        take 3 . map toCardCode <$> discardOf p >>= (`shouldSatisfy` (\cs -> all (`elem` cs) ["06002", "06003", "06021"]))
