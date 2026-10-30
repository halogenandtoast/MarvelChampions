module Marvel.Rules.EncounterPlayerDeckSpec (spec) where

import Marvel.EncounterCard (genEncounterCard)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

playerDeck :: IdentityId -> [PlayerCard] -> GameAppT ()
playerDeck you cards = withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity attrs) ->
  PlayerIdentity attrs {playerIdentityDeck = Deck cards, playerIdentityHand = Hand [], playerIdentityDiscard = Discard []}

dealt :: IdentityId -> GameAppT [EncounterCard]
dealt you = playerIdentityEncounterCards . toAttrs <$> getPlayer you

spec :: Spec
spec = describe "encounter cards in player decks" do
  it "preserves the original encounter card through a saved player-zone representation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      card <- genEncounterCard Treacheries.advance
      let proxy = encounterCardInPlayerDeck (player1 tg) card
      playerZoneEncounterCard proxy `shouldBe` Just card
      (fromJSON (toJSON proxy) :: Result PlayerCard) `shouldBe` Success proxy

  it "completes the original draw batch, deals encountered cards facedown, and replaces them" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      a <- putInHand p "01088"
      b <- putInHand p "01089"
      c <- putInHand p "01090"
      remaining <- putInHand p "01093"
      encounter <- genEncounterCard Treacheries.advance
      playerDeck p [a, encounterCardInPlayerDeck p encounter, b, c, remaining]
      resolve [IdentityMessage p (DrawCards FromDeck 3)]
      handOf p `shouldReturn` [a, b, c]
      dealt p `shouldReturn` [encounter]
      discardOf p `shouldReturn` []

  it "replaces encounter cards discarded from the deck without putting them in the player discard" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      a <- putInHand p "01088"
      b <- putInHand p "01089"
      replacement <- putInHand p "01090"
      remaining <- putInHand p "01093"
      encounter <- genEncounterCard Treacheries.advance
      playerDeck p [a, encounterCardInPlayerDeck p encounter, b, replacement, remaining]
      resolve [IdentityMessage p (DiscardFrom FromDeck 3 Nothing)]
      handOf p `shouldReturn` [replacement]
      discardOf p `shouldReturn` [a, b]
      dealt p `shouldReturn` [encounter]

  it "continues replacement draws through consecutive encounter cards" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- putInHand p "01088"
      remaining <- putInHand p "01089"
      one <- genEncounterCard Treacheries.advance
      two <- genEncounterCard Minions.hydraMercenary
      playerDeck p [encounterCardInPlayerDeck p one, encounterCardInPlayerDeck p two, card, remaining]
      resolve [IdentityMessage p (DrawCards FromDeck 1)]
      handOf p `shouldReturn` [card]
      dealt p `shouldReturn` [one, two]

  it "does not treat ordinary player-deck obligations as encounter cards" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      obligation <- putInHand p "21185"
      playerZoneEncounterCard obligation `shouldBe` Nothing
