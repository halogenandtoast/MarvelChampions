module Marvel.Encounter.RocketRaccoonNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

rocket :: IO TestGame
rocket = testGame ["rocket-raccoon"]

asHero :: IdentityId -> GameAppT ()
asHero p = resolve [IdentityMessage p (ChangedToForm A)]

minionAttackOf :: MinionId -> GameAppT Natural
minionAttackOf m = Minion.getModifiedAttack . toAttrs =<< getsGame (fromMaybe (error "no such minion") . HashMap.lookup m . gameMinions)

villainAttackNow :: GameAppT Natural
villainAttackNow = Villain.getModifiedAttack . toAttrs =<< getVillain

isToughMinion :: MinionId -> GameAppT (Maybe Bool)
isToughMinion m = getsGame (fmap ((.minionTough) . toAttrs) . HashMap.lookup m . gameMinions)

minionCodes :: GameAppT [CardCode]
minionCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMinions)

sideSchemeCodes :: GameAppT [CardCode]
sideSchemeCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes)

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

spec :: Spec
spec = describe "Rocket Raccoon nemesis" do
  describe "Shadow of the Past" do
    it "Blackjack's quickstrike attack happens before Vendetta enters play (FAQ)" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        -- the boost card for his villainous attack: Hail Hydra! has no boost icons
        _ <- stackEncounterDeck [Treacheries.hailHydra]
        _ <- revealNow p Treacheries.shadowOfThePast
        -- asked to defend against Blackjack: Vendetta is not yet in play
        minionCodes `shouldReturn` ["16055"]
        sideSchemeCodes `shouldReturn` []
        pass
        damageOf p `shouldReturn` 1
        sideSchemeCodes `shouldReturn` ["16054"]
        sort . filter (`elem` ["16056", "16057"]) . map toCardCode <$> scenarioEncounterDeckNow
          `shouldReturn` ["16056", "16057", "16057"]

  describe "Vendetta (16054)" do
    it "enters play with 2 threat per player" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        scheme <- revealNow p SideSchemes.vendetta
        pass
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 2

    it "4 with two players" do
      tg <- testGame ["rocket-raccoon", "spider-man"]
      let p = player1 tg
      inGame tg do
        scheme <- revealNow p SideSchemes.vendetta
        pass
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4

  describe "Blackjack O'Hare (16055)" do
    it "attacks the hero as he engages (quickstrike), given a boost card (villainous)" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        asHero p
        -- Vulture has 2 boost icons
        [boost] <- stackEncounterDeck [Minions.vulture]
        _ <- revealNow p Minions.blackjackOHare
        pass
        damageOf p `shouldReturn` 3
        map ecCardId <$> encounterDiscardNow >>= (`shouldSatisfy` elem (ecCardId boost))

    it "does not attack an alter-ego as he engages" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        [top] <- stackEncounterDeck [Minions.vulture]
        _ <- revealNow p Minions.blackjackOHare
        pass
        damageOf p `shouldReturn` 0
        map ecCardId . take 1 <$> scenarioEncounterDeckNow `shouldReturn` [ecCardId top]

  describe "Blackjack's Bazooka (16056)" do
    it "attaches to Blackjack O'Hare for +2 ATK; spending [mental][mental][mental] discards it" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        m <- addMinion p Minions.blackjackOHare
        pass
        villain <- villainAttackNow
        card <- revealNow p Attachments.blackjacksBazooka
        pass
        minionAttackOf m `shouldReturn` 3
        villainAttackNow `shouldReturn` villain
        asHero p
        clearHand p
        g1 <- putInHand p "01089"
        g2 <- putInHand p "01089"
        choose "Blackjack's Bazooka" (isAbilityOf (attachmentOf card))
        payWith [g1, g2]
        pass
        getsGame (HashMap.member (attachmentOf card) . gameAttachments) `shouldReturn` False
        minionAttackOf m `shouldReturn` 1

    it "attaches to the villain when Blackjack is not in play" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        villain <- villainAttackNow
        _ <- revealNow p Attachments.blackjacksBazooka
        pass
        villainAttackNow `shouldReturn` (villain + 2)

  describe "Planetary Invasion (16057)" do
    it "discards to a minion, reveals it and makes it tough" do
      tg <- rocket
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [Treacheries.hailHydra, Minions.hydraMercenary]
        _ <- revealNow p Treacheries.planetaryInvasion
        pass
        encounterDiscardCodes >>= (`shouldSatisfy` elem "03030")
        [m] <- getsGame (HashMap.keys . gameMinions)
        minionCodes `shouldReturn` ["01101"]
        isToughMinion m `shouldReturn` Just True
