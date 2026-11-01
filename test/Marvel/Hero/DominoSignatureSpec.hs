module Marvel.Hero.DominoSignatureSpec (spec) where
import Data.Text qualified as Text
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Hp
import Marvel.Ally.Types
import Marvel.Matchers
import TestImport

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card idx = \case
  UseAbility ab -> isAbilityOfCard card (UseAbility ab) && abilityIndex ab == idx
  _ -> False
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p
payDiscard :: GameAppT ()
payDiscard = choose "discard top card" (isLabel $ countLabel "discardTopOfDeck" 1) >> pass
spec :: Spec
spec = describe "Domino signature" do
  it "Diamondback exhausts, takes one damage and hits every enemy for the discarded printed icons" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      diamond <- putIntoPlay p "40038"
      mid <- addMinion p Minions.hydraMercenary
      _ <- stackDeck p ["40040"]
      choose "Diamondback" (cardAbility diamond 1)
      payDiscard
      villainDamageNow `shouldReturn` 2
      minionDamageOf mid `shouldReturn` Just 2
      allyDamage <$> allyIn "40038" `shouldReturn` 1
  it "Diamondback's effect finishes when her self-damage defeats her" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      diamond <- putIntoPlay p "40038"
      resolve [AllyMessage (allyIdOf diamond) (AllyDamaged (toSource p) $ toDamage 1 FromAbility)]
      _ <- stackDeck p ["40040"]
      choose "Diamondback at one hit point" (cardAbility diamond 1)
      payDiscard
      villainDamageNow `shouldReturn` 2
      alliesWith "40038" `shouldReturn` []
  it "Outlaw enters tough and adds resource icons only to her interrupted attack" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      outlaw <- putIntoPlay p "40039"
      allyTough <$> allyIn "40039" `shouldReturn` True
      _ <- stackDeck p ["40040"]
      choose "Outlaw basic attack" (cardAbility outlaw 301)
      chooseVillain
      choose "Outlaw's interrupt" (cardAbility outlaw 1)
      payDiscard
      pass
      villainDamageNow `shouldReturn` 3
  it "A Good Workout deals its complete resource-enhanced damage to the chosen enemy" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.shocker
      _ <- stackDeck p ["40041"]
      event <- putInHand p "40040"
      pay <- putInHand p "01088"
      playWith event [pay]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6
      minionDamageOf mid `shouldReturn` Just 0
  it "A Good Workout's entire combined damage is prevented by one tough card" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      vid <- villainIdNow
      resolve [GiveStatus (VillainRef vid) ToughStatus]
      _ <- stackDeck p ["40041"]
      event <- putInHand p "40040"
      pay <- putInHand p "01088"
      playWith event [pay]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 0
      characterMatches (CharacterWithStatus ToughStatus) (VillainCharacter vid) `shouldReturn` False
  it "A Good Workout applies per-attack damage reduction once to the complete amount" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.ne40182
      withGame_ $ entitiesL . minionsL . ix mid %~ Minion.overMinion (\a -> a {Minion.minionHitPoints = HP 12})
      _ <- stackDeck p ["40041"]
      event <- putInHand p "40040"
      pay <- putInHand p "01088"
      playWith event [pay]
      choose "chosen enemy" (isTargetChoice mid)
      pass
      minionDamageOf mid `shouldReturn` Just 5
  it "Luck Be a Lady resolves a wild icon twice as two independent choices" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      setDamage p 4
      _ <- stackDeck p ["40040"]
      event <- putInHand p "40041"
      pay <- putInHand p "01093"
      playWith event [pay]
      pass
      choose "heal for first wild" (isLabel "$label.dominoHeal")
      choose "heal Domino" (isTargetChoice p)
      choose "damage for second wild" (isLabel "$label.dominoDamage")
      chooseVillain
      pass
      damageOf p `shouldReturn` 2
      villainDamageNow `shouldReturn` 3
  it "Luck Be a Lady resolves Jackpot's energy, mental and physical independently" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      setDamage p 4
      mid <- mainSchemeIdNow
      resolve [MainSchemeMessage mid (MainSchemePlaceThreat 5)]
      _ <- stackDeck p ["40043"]
      event <- putInHand p "40041"
      pay <- putInHand p "01093"
      playWith event [pay]
      pass
      choose "energy heals" (isTargetChoice p)
      chooseVillain
      chooseMainScheme
      pass
      damageOf p `shouldReturn` 2
      mainSchemeThreat `shouldReturn` 3
      villainDamageNow `shouldReturn` 3
  it "Right Place Right Time adds discarded-icon threat removal to the original scheme" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- mainSchemeIdNow
      resolve [MainSchemeMessage mid (MainSchemePlaceThreat 6)]
      _ <- stackDeck p ["40040"]
      event <- putInHand p "40042"
      pay <- putInHand p "01089"
      playWith event [pay]
      chooseMainScheme
      pass
      mainSchemeThreat `shouldReturn` 1
  it "Jackpot's response shuffles it back instead of adding it to hand" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      [jackpot] <- stackDeck p ["40043"]
      runNow [IdentityMessage p (DiscardFrom FromDeck 1 Nothing)]
      choose "Jackpot" (isNamedAbility "$label.dominoJackpot")
      pass
      discardOf p >>= (`shouldSatisfy` notElem jackpot)
      handOf p >>= (`shouldSatisfy` notElem jackpot)
  it "Jackpot remains optional and may be left for Neena to retrieve" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      [jackpot] <- stackDeck p ["40043"]
      runNow [IdentityMessage p (DiscardFrom FromDeck 1 Nothing)]
      pass
      discardOf p >>= (`shouldSatisfy` elem jackpot)
  it "a last-card Jackpot is already in the reset deck before its response window" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      [card] <- stackDeck p ["40043"]
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck [card]}
      runNow [IdentityMessage p (DiscardFrom FromDeck 1 Nothing)]
      unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` elem card)
      choose "Jackpot after reset" (isNamedAbility "$label.dominoJackpot")
      pass
      unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` elem card)

  it "Right Place Right Time resolves a single combined removal on its chosen scheme" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- mainSchemeIdNow
      resolve [MainSchemeMessage mid (MainSchemePlaceThreat 6)]
      _ <- stackDeck p ["40040"]
      event <- putInHand p "40042"
      pay <- putInHand p "01089"
      liftIO $ writeIORef (ran tg) []
      playWith event [pay]
      chooseMainScheme
      pass
      mainSchemeThreat `shouldReturn` 1
    messages <- readIORef (ran tg)
    let removals = filter (\m -> Text.isPrefixOf "MainSchemeMessage " m && Text.isInfixOf "MainSchemeThwarted " m) messages
    length removals `shouldBe` 1
