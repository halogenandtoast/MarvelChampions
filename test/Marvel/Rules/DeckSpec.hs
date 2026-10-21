module Marvel.Rules.DeckSpec (spec) where

import TestImport

setDeck :: IdentityId -> [PlayerCard] -> GameAppT ()
setDeck ident cards = withGame_ $ entitiesL . playersL . ix ident %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentityDeck = Deck cards}

dealtCards :: IdentityId -> GameAppT Int
dealtCards ident = length . (.playerIdentityEncounterCards) . toAttrs <$> getPlayer ident

spec :: Spec
spec = describe "a player deck that empties" do
  it "is made again from the discard pile at once, and its player is dealt an encounter card" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      discard <- discardOf p
      deck <- unDeck . (.playerIdentityDeck) . toAttrs <$> getPlayer p
      -- two cards left, the rest in the discard pile
      setDeck p (take 2 deck)
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
        PlayerIdentity a {playerIdentityDiscard = Discard (drop 2 deck <> discard)}
      dealt <- dealtCards p
      runNow [IdentityMessage p (DrawCards FromDeck 2)]
      length . unDeck . (.playerIdentityDeck) . toAttrs <$> getPlayer p >>= (`shouldSatisfy` (> 0))
      discardOf p `shouldReturn` []
      dealtCards p `shouldReturn` dealt + 1

  it "keeps drawing from the new deck" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      deck <- unDeck . (.playerIdentityDeck) . toAttrs <$> getPlayer p
      setDeck p (take 1 deck)
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
        PlayerIdentity a {playerIdentityDiscard = Discard (drop 1 deck)}
      before <- length <$> handOf p
      runNow [IdentityMessage p (DrawCards FromDeck 3)]
      length <$> handOf p `shouldReturn` before + 3
