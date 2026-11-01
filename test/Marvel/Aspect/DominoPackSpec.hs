module Marvel.Aspect.DominoPackSpec (spec) where
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types
import Marvel.Deck
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Minion.Cards qualified as Minions
import TestImport

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card idx = \case
  UseAbility ab -> isAbilityOfCard card (UseAbility ab) && abilityIndex ab == idx
  _ -> False
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p
payDiscard :: GameAppT ()
payDiscard = choose "discard top card" (isLabel $ countLabel "discardTopOfDeck" 1) >> pass
thwartWith :: PlayerCard -> GameAppT ()
thwartWith card = choose "ally thwarts" (cardAbility card 300) >> chooseMainScheme
spec :: Spec
spec = describe "Domino pack" do
  it "Feral discards after thwarting and damages the villain for doubled wild icons" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- mainSchemeIdNow
      resolve [MainSchemeMessage sid (MainSchemePlaceThreat 5)]
      feral <- putIntoPlay p "40050"
      _ <- stackDeck p ["40040"]
      thwartWith feral
      choose "Feral response" (cardAbility feral 1)
      pass
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2
      mainSchemeThreat `shouldReturn` 3
  it "Wolfsbane names a card type and may retrieve the matching discarded card" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- mainSchemeIdNow
      resolve [MainSchemeMessage sid (MainSchemePlaceThreat 5)]
      wolf <- putIntoPlay p "40051"
      [event] <- stackDeck p ["40040"]
      thwartWith wolf
      choose "Wolfsbane" (cardAbility wolf 1)
      choose "name event" (isLabel "$label.nameEventType")
      choose "retrieve event" (isTargetChoice event)
      pass
      handOf p >>= (`shouldSatisfy` elem event)
  it "Wolfsbane leaves a mismatching card discarded" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- mainSchemeIdNow
      resolve [MainSchemeMessage sid (MainSchemePlaceThreat 5)]
      wolf <- putIntoPlay p "40051"
      [card] <- stackDeck p ["40040"]
      thwartWith wolf
      choose "Wolfsbane" (cardAbility wolf 1)
      choose "name upgrade" (isLabel "$label.nameUpgradeType")
      pass
      discardOf p >>= (`shouldSatisfy` elem card)
      handOf p >>= (`shouldSatisfy` notElem card)
  it "Atlas Bear looks at another player's top card and may damage herself to give its owner an event" do
    tg <- testGame ["domino", "spider-man"]
    inGame tg do
      let p = player1 tg; other = player2 tg
      clearHand p
      bear <- putIntoPlay p "40056"
      [card] <- stackDeck other ["01005"]
      choose "Atlas Bear" (cardAbility bear 1)
      choose "other player's deck" (isTargetChoice other)
      choose "take the event" (isLabel "$label.dominoTakeEvent")
      handOf other >>= (`shouldSatisfy` elem card)
      allyDamage <$> allyIn "40056" `shouldReturn` 1
  it "Atlas Bear cannot take a non-event" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      bear <- putIntoPlay p "40056"
      [card] <- stackDeck p ["01088"]
      choose "Atlas Bear" (cardAbility bear 1)
      choose "own deck" (isTargetChoice p)
      offers (isLabel "$label.dominoTakeEvent") `shouldReturn` False
      choose "done looking" (isLabel "$label.done")
      handOf p >>= (`shouldSatisfy` notElem card)
      allyDamage <$> allyIn "40056" `shouldReturn` 0
  it "White Fox can enter play when discarded from the top of a deck" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- stackDeck p ["40057"]
      runNow [IdentityMessage p (DiscardFrom FromDeck 1 Nothing)]
      choose "White Fox" (isNamedAbility "$label.dominoWhiteFox")
      pass
      alliesWith "40057" >>= (`shouldSatisfy` ((==1) . length))
  it "White Fox's response is optional and does not trigger when discarded from hand" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      fox <- putInHand p "40057"
      runNow [DiscardedCard $ PlayerCard fox]
      offers (isNamedAbility "$label.dominoWhiteFox") `shouldReturn` False
      alliesWith "40057" `shouldReturn` []
  it "Digging Deep retrieves its exact discarded copy and can be declined" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      [card] <- stackDeck p ["40060"]
      runNow [IdentityMessage p (DiscardFrom FromDeck 1 Nothing)]
      choose "Digging Deep" (isNamedAbility "$label.dominoDiggingDeep")
      pass
      handOf p `shouldReturn` [card]
      [other] <- stackDeck p ["40060"]
      runNow [IdentityMessage p (DiscardFrom FromDeck 1 Nothing)]
      pass
      discardOf p >>= (`shouldSatisfy` elem other)
  it "The Posse requires three controlled Posse characters and heals and readies all Posse characters" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      posse <- putInHand p "40058"
      pay <- putInHand p "01088"
      offers (isPlay posse) `shouldReturn` False
      diamond <- putIntoPlay p "40038"
      _ <- putIntoPlay p "40056"
      setDamage p 2
      resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf diamond) ExhaustedAlly]
      playWith posse [pay]
      pass
      damageOf p `shouldReturn` 1
      exhaustedNow p `shouldReturn` False
      allyExhausted <$> allyIn "40038" `shouldReturn` False
  it "Sharpshooter adds resource damage to a ranged pistol attack and does not trigger on ordinary attacks" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      pistol <- putIntoPlay p "40046"
      sharp <- putIntoPlay p "40064"
      _ <- stackDeck p ["40040", "01088"]
      choose "Pistol" (cardAbility pistol 1)
      chooseVillain
      choose "discard for pistol" (isLabel $ countLabel "discardTopOfDeck" 1)
      choose "Sharpshooter" (cardAbility sharp 1)
      payDiscard
      villainDamageNow `shouldReturn` 4
      choose "basic attack" (\case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 301; _ -> False)
      offers (cardAbility sharp 1) `shouldReturn` False
  it "Team Investigation removes three threat per player from one side scheme" do
    tg <- testGame ["domino", "spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      scheme <- sideSchemeOf <$> revealNow p Schemes.crowdControl
      pass
      resolve [SideSchemeMessage scheme (SideSchemePlaceThreat 8)]
      before <- sideSchemeThreat scheme
      card <- putInHand p "40053"
      pay <- putInHand p "01089"
      playWith card [pay]
      choose "side scheme" (isTargetChoice scheme)
      pass
      sideSchemeThreat scheme `shouldReturn` (before - 6)
  it "Take Out the Guards lets each player discard a non-Elite minion anywhere in play" do
    tg <- testGame ["domino", "spider-man"]
    inGame tg do
      let p = player1 tg; other = player2 tg
      asHero p
      scheme <- putIntoPlay p "40054"
      first <- addMinion other Minions.hydraMercenary
      second <- addMinion p Minions.hydraSoldier
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid (SideSchemeThwarted (toSource p) 8)]
      choose "first discards other's minion" (isTargetChoice first)
      choose "second discards first player's minion" (isTargetChoice second)
      pass
      minionDamageOf first `shouldReturn` Nothing
      minionDamageOf second `shouldReturn` Nothing
  it "Superpower Training puts a signature upgrade into play free from each player's deck or discard" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      scheme <- putIntoPlay p "40059"
      pistol <- putInHand p "40046"
      resolve [DiscardedCard $ PlayerCard pistol]
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid (SideSchemeThwarted (toSource p) 3)]
      choose "free signature upgrade" (isTargetChoice pistol)
      pass
      upgradeCodes >>= (`shouldSatisfy` elem "40046")
      discardOf p >>= (`shouldSatisfy` notElem pistol)
  it "Digging Deep retrieves its exact copy after being the last card and immediately reshuffling" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      [card] <- stackDeck p ["40060"]
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck [card]}
      runNow [IdentityMessage p (DiscardFrom FromDeck 1 Nothing)]
      choose "Digging Deep after reset" (isNamedAbility "$label.dominoDiggingDeep")
      pass
      handOf p `shouldReturn` [card]
      unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` notElem card)
  it "Take Out the Guards never offers an Elite minion" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      scheme <- putIntoPlay p "40054"
      elite <- addMinion p Minions.sandman
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid (SideSchemeThwarted (toSource p) 8)]
      offers (isTargetChoice elite) `shouldReturn` False
      choose "decline" (isLabel "$label.noResponse")
      pass
      minionDamageOf elite `shouldReturn` Just 0
