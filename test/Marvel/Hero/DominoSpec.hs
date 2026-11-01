module Marvel.Hero.DominoSpec (spec) where
import Data.HashMap.Strict qualified as HashMap
import Marvel.Deck
import Marvel.Discard
import Marvel.Domino
import Marvel.Identity (currentIdentity, PlayerIdentitySide(..))
import Marvel.Hero.Runner qualified as Hero
import Marvel.Support.Types
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card idx = \case
  UseAbility ab -> isAbilityOfCard card (UseAbility ab) && abilityIndex ab == idx
  _ -> False
identityAction :: IdentityId -> Choice -> Bool
identityAction p = \case
  UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab `elem` [1,2]
  _ -> False
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p
payDiscard :: GameAppT ()
payDiscard = choose "discard top card" (isLabel $ countLabel "discardTopOfDeck" 1) >> pass
spec :: Spec
spec = describe "Domino" do
  it "starts with nine hit points, six-card alter ego hand size and changes to printed 1/2/3 powers" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 9
      handSizeOf p `shouldReturn` 6
      asHero p
      handSizeOf p `shouldReturn` 5
      currentIdentity <$> getPlayer p >>= \case
        HeroSide hero -> do
          Hero.getModifiedThwart (toAttrs hero) `shouldReturn` 1
          Hero.getModifiedAttack (toAttrs hero) `shouldReturn` 2
          Hero.getModifiedDefense (toAttrs hero) `shouldReturn` 3
        _ -> error "expected Domino"
  it "swaps a hand card with the deck top once a round without discarding or drawing it" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "40043"
      [top] <- stackDeck p ["40060"]
      choose "Domino's action" (identityAction p)
      choose "swap Jackpot" (isTargetChoice card)
      handOf p `shouldReturn` [top]
      head . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` card
      discardOf p >>= (`shouldSatisfy` notElem top)
      offers (identityAction p) `shouldReturn` False
  it "does not offer the hero swap without a card in hand" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      offers (identityAction p) `shouldReturn` False
  it "Neena swaps only with the top discard and keeps a separate round limit from Domino" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      top <- putInHand p "40043"
      older <- putInHand p "01088"
      resolve [DiscardedCard $ PlayerCard older, DiscardedCard $ PlayerCard top]
      hand <- putInHand p "40041"
      choose "Neena action" (identityAction p)
      choose "swap hand" (isTargetChoice hand)
      handOf p `shouldReturn` [top]
      head <$> discardOf p `shouldReturn` hand
      offers (identityAction p) `shouldReturn` False
      changeForm p
      pass
      offers (identityAction p) `shouldReturn` True
  it "counts Wild twice only in hero form and counts Jackpot's three printed icons once each" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      wild <- putInHand p "40040"
      jackpot <- putInHand p "40043"
      discardedResourceCount p [PlayerCard wild] `shouldReturn` 1
      asHero p
      discardedResourceCount p [PlayerCard wild] `shouldReturn` 2
      discardedResourceCount p [PlayerCard jackpot] `shouldReturn` 3
  it "Pip puts a Domino or Posse card from discard on top and is alter ego only" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      pip <- putIntoPlay p "40044"
      card <- putInHand p "40043"
      resolve [DiscardedCard $ PlayerCard card]
      choose "Pip" (cardAbility pip 1)
      choose "Jackpot" (isTargetChoice card)
      head . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` card
      discardOf p >>= (`shouldSatisfy` notElem card)
      changeForm p
      pass
      offers (cardAbility pip 1) `shouldReturn` False
  it "The Painted Lady stores a top-deck discard facedown, then returns exactly one in alter ego" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      lady <- putIntoPlay p "40045"
      [card] <- stackDeck p ["40040"]
      runNow [IdentityMessage p (DiscardFrom FromDeck 1 Nothing)]
      choose "Painted Lady response" (cardAbility lady 1)
      choose "store card" (isTargetChoice card)
      pass
      discardOf p >>= (`shouldSatisfy` notElem card)
      held <- getsGame $ supportHeldCards . toAttrs . (HashMap.! (supportIdOf lady)) . gameSupports
      held `shouldBe` [card]
      choose "retrieve from Painted Lady" (cardAbility lady 2)
      choose "held card" (isTargetChoice card)
      handOf p >>= (`shouldSatisfy` elem card)
  it "Domino's Pistol exhausts and deals two ranged damage for one printed wild" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      pistol <- putIntoPlay p "40046"
      _ <- stackDeck p ["40040"]
      choose "fire pistol" (cardAbility pistol 1)
      chooseVillain
      payDiscard
      villainDamageNow `shouldReturn` 2
      offers (cardAbility pistol 1) `shouldReturn` False
  it "Probability Field increases a basic attack using discarded icons only for that attack" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      field <- putIntoPlay p "40049"
      _ <- stackDeck p ["40040"]
      choose "basic attack" (\case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 301; _ -> False)
      choose "Probability Field" (cardAbility field 1)
      payDiscard
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4
  it "Probability Field also increases alter ego recovery without doubling wild icons" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      setDamage p 6
      field <- putIntoPlay p "40049"
      _ <- stackDeck p ["40040"]
      choose "recover" (\case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 200; _ -> False)
      choose "Probability Field recovery" (cardAbility field 1)
      payDiscard
      damageOf p `shouldReturn` 2
  it "Lucky Break cancels an entire encounter card and reveals a replacement" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      lucky <- putIntoPlay p "40048"
      _ <- stackEncounterDeck [Treacheries.advance, Treacheries.imTough]
      before <- mainSchemeThreat
      runNow [DrawAndRevealEncounterCard p]
      choose "Lucky Break" (cardAbility lucky 1)
      pass
      upgradeCodes >>= (`shouldSatisfy` notElem "40048")
      mainSchemeThreat `shouldReturn` before
  it "Lucky and Good exhausts to cancel a boost card and replace it for this attack" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      good <- putIntoPlay p "40047"
      _ <- stackEncounterDeck [Treacheries.advance, Treacheries.imTough]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "do not defend" (\case Label l _ -> l `elem` ["$label.noDefender", "$label.declineToDefend"]; _ -> False)
      choose "Lucky and Good" (cardAbility good 1)
      pass
      damageOf p `shouldReturn` 2
      offers (cardAbility good 1) `shouldReturn` False
  it "Lucky and Good is not offered while the villain schemes" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      good <- putIntoPlay p "40047"
      _ <- stackEncounterDeck [Treacheries.advance]
      vid <- villainIdNow
      runNow [VillainMessage vid VillainSchemes]
      offers (cardAbility good 1) `shouldReturn` False
  it "The Painted Lady holds at most three cards and leaves further discards in the discard pile" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      lady <- putIntoPlay p "40045"
      for_ ["40040", "40041", "40042"] $ \code -> do
        [card] <- stackDeck p [code]
        runNow [IdentityMessage p (DiscardFrom FromDeck 1 Nothing)]
        choose "Painted Lady" (cardAbility lady 1)
        choose "hold the discarded card" (isTargetChoice card)
        pass
      [fourth] <- stackDeck p ["40046"]
      runNow [IdentityMessage p (DiscardFrom FromDeck 1 Nothing)]
      offers (cardAbility lady 1) `shouldReturn` False
      pass
      discardOf p >>= (`shouldSatisfy` elem fourth)
      held <- getsGame $ supportHeldCards . toAttrs . (HashMap.! (supportIdOf lady)) . gameSupports
      length held `shouldBe` 3
  it "Probability Field can improve the basic defense declared against an attack" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      field <- putIntoPlay p "40049"
      _ <- stackDeck p ["40040"]
      _ <- stackEncounterDeck [Treacheries.advance]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "defend" isDefend
      choose "Probability Field defense" (cardAbility field 1)
      payDiscard
      damageOf p `shouldReturn` 0
  it "Lucky Break and Lucky and Good are unavailable in alter ego" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      lucky <- putIntoPlay p "40048"
      good <- putIntoPlay p "40047"
      _ <- stackEncounterDeck [Treacheries.advance]
      runNow [DrawAndRevealEncounterCard p]
      offers (cardAbility lucky 1) `shouldReturn` False
      offers (cardAbility good 1) `shouldReturn` False

  it "Probability Field enhances ATK when a basic thwart uses ATK against an Assault scheme" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      field <- putIntoPlay p "40049"
      scheme <- sideSchemeOf <$> revealNow p Schemes.ne40087
      pass
      resolve [SideSchemeMessage scheme (SideSchemePlaceThreat 4)]
      before <- sideSchemeThreat scheme
      _ <- stackDeck p ["40040"]
      choose "basic thwart" (\case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 300; _ -> False)
      choose "Probability Field" (cardAbility field 1)
      payDiscard
      choose "Assault side scheme" (isTargetChoice scheme)
      pass
      -- ATK 2 plus two icons from the wild, rather than THW 1 plus two.
      sideSchemeThreat scheme `shouldReturn` (before - 4)
