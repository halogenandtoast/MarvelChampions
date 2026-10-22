module Marvel.Hero.BlackPantherSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Card.Def (CardDef (cdTraits), cdCardType)
import Marvel.Card.Types (CardType (UpgradeType))
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types qualified as Minion
import Marvel.Obligation.Cards qualified as Cards
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types (getSideSchemeThreat)
import Marvel.Trait (Trait (BlackPanther))
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Villain.Types (villainIsTough)
import TestImport

isBlackPantherUpgrade :: PlayerCard -> Bool
isBlackPantherUpgrade c = cdCardType (getCardDef c) == UpgradeType && BlackPanther `elem` cdTraits (getCardDef c)

isMinionChoice :: Choice -> Bool
isMinionChoice = \case
  TargetLabel (MinionRef _) _ -> True
  TargetLabel (EnemyRef (EnemyMinionId _)) _ -> True
  _ -> False

isVillainChoice :: Choice -> Bool
isVillainChoice = \case
  TargetLabel (VillainRef _) _ -> True
  TargetLabel (EnemyRef (EnemyVillainId _)) _ -> True
  _ -> False

-- | Play Wakanda Forever! with an Energy, resolving the upgrades' specials in this order.
wakandaForever :: IdentityId -> CardCode -> [PlayerCard] -> GameAppT ()
wakandaForever p code order = do
  wf <- putInHand p code
  energy <- putInHand p "01088"
  playWith wf [energy]
  for_ order \u -> choose "resolve the special" (isTargetChoice (upgradeIdOf u))

-- | Answer when asked; a choice of one may be made for the player.
maybeChoose :: Text -> (Choice -> Bool) -> GameAppT ()
maybeChoose what p = whenM (offers p) (choose what p)

spec :: Spec
spec = describe "Black Panther" do
  it "T'Challa's Foresight adds a Black Panther upgrade to the starting hand" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      hand <- handOf p
      length hand `shouldBe` 7
      any isBlackPantherUpgrade hand `shouldBe` True

  it "Black Panther retaliates 1 when attacked" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      changeForm p
      villain <- getVillain
      start [VillainMessage (toId villain) (VillainAttacks p)]
      pass
      villainDamageNow `shouldReturn` 1

  it "Shuri searches the deck for an upgrade" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      shuri <- putInHand p "01041"
      energy <- putInHand p "01088"
      [_, claws] <- stackDeck p ["01044", "01047"]
      deckBefore <- length . unDeck . (.playerIdentityDeck) . toAttrs <$> getPlayer p
      playWith shuri [energy]
      choose "use Shuri" (isAbilityOf (allyIdOf shuri))
      choose "take the claws" (isTargetChoice claws)
      handOf p `shouldReturn` [claws]
      deckAfter <- length . unDeck . (.playerIdentityDeck) . toAttrs <$> getPlayer p
      deckAfter `shouldBe` deckBefore - 1

  it "Shuri finding no upgrade leaves the deck as it was" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck []}
      _ <- stackDeck p ["01044", "01088", "01042"]
      shuri <- putInHand p "01041"
      energy <- putInHand p "01088"
      playWith shuri [energy]
      choose "use Shuri" (isAbilityOf (allyIdOf shuri))
      pass
      whenM (offers (isLabel "$label.noMatchingCardsFound")) $ choose "nothing found" (isLabel "$label.noMatchingCardsFound")
      length . unDeck . (.playerIdentityDeck) . toAttrs <$> getPlayer p `shouldReturn` 3

  it "Ancestral Knowledge shuffles up to 3 different cards from the discard pile into the deck" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      -- the Wakanda Forever!s share a title, so are the same card
      [a, b, c, d] <- stackDiscard p ["01043a", "01043b", "01045", "01046"]
      deckBefore <- length . unDeck . (.playerIdentityDeck) . toAttrs <$> getPlayer p
      knowledge <- putInHand p "01042"
      genius <- putInHand p "01089"
      playWith knowledge [genius]
      choose "Wakanda Forever!" (isTargetChoice a)
      -- a second Vibranium is the same card
      offers (isTargetChoice b) `shouldReturn` False
      choose "The Golden City" (isTargetChoice c)
      choose "Energy Daggers" (isTargetChoice d)
      deckAfter <- length . unDeck . (.playerIdentityDeck) . toAttrs <$> getPlayer p
      deckAfter `shouldBe` deckBefore + 3
      map toCardCode <$> discardOf p `shouldReturn` ["01042", "01089", "01043b"]

  it "Ancestral Knowledge is an alter-ego action" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- stackDiscard p ["01044"]
      knowledge <- putInHand p "01042"
      _ <- putInHand p "01089"
      changeForm p
      offers (isPlay knowledge) `shouldReturn` False

  for_ ["01043a", "01043b", "01043c", "01043d"] \code ->
    it ("Wakanda Forever! (" <> show code <> ") is a hero action") do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "01047"
        wf <- putInHand p code
        _ <- putInHand p "01088"
        offers (isPlay wf) `shouldReturn` False
        changeForm p
        offers (isPlay wf) `shouldReturn` True

  it "Wakanda Forever! resolves each special, the final step doubled" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      ms <- mainSchemeId
      resolve [MainSchemeMessage ms (MainSchemePlaceThreat 5)]
      threat <- mainSchemeThreatNow
      genius <- putIntoPlay p "01048"
      daggers <- putIntoPlay p "01046"
      claws <- putIntoPlay p "01047"
      wakandaForever p "01043a" [genius]
      whenM (offers \case TargetLabel (MainSchemeRef _) _ -> True; TargetLabel (SchemeRef _) _ -> True; _ -> False) $
        choose "the main scheme" \case TargetLabel {} -> True; _ -> False
      mainSchemeThreatNow `shouldReturn` (threat - 1)
      choose "daggers" (isTargetChoice (upgradeIdOf daggers))
      maybeChoose "you" (isTargetChoice p)
      villainDamageNow `shouldReturn` 1
      choose "claws" (isTargetChoice (upgradeIdOf claws))
      maybeChoose "Rhino" isVillainChoice
      villainDamageNow `shouldReturn` 5

  it "Wakanda Forever! with one Black Panther upgrade makes it the final step" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      claws <- putIntoPlay p "01047"
      wakandaForever p "01043b" [claws]
      maybeChoose "Rhino" isVillainChoice
      villainDamageNow `shouldReturn` 4

  it "Wakanda Forever! resolves only your own Black Panther upgrades" do
    tg <- testGame ["black-panther", "spider-man"]
    let (p, other) = case tg.seats of
          [a, b] -> (a, b)
          _ -> error "two players"
    inGame tg do
      clearHand p
      changeForm p
      claws <- putIntoPlay p "01047"
      _ <- putIntoPlay other "01046"
      wakandaForever p "01043c" [claws]
      maybeChoose "Rhino" isVillainChoice
      villainDamageNow `shouldReturn` 4

  it "Vibranium Suit moves damage from Black Panther to an enemy, 2 as the final step" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      setDamage p 3
      suit <- putIntoPlay p "01049"
      wakandaForever p "01043d" [suit]
      maybeChoose "Rhino" isVillainChoice
      villainDamageNow `shouldReturn` 2
      damageOf p `shouldReturn` 1

  it "Panther Claws' special is an attack: Whiplash retaliates" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      whiplash <- encounterCard Cards.whiplash
      resolve [RevealEncounterCard p whiplash]
      claws <- putIntoPlay p "01047"
      genius <- putIntoPlay p "01048"
      wakandaForever p "01043a" [claws]
      maybeChoose "Whiplash" isMinionChoice
      minionDamages `shouldReturn` [2]
      choose "genius" (isTargetChoice (upgradeIdOf genius))
      damageOf p `shouldReturn` 1

  it "Energy Daggers damage the villain and each enemy engaged with the player chosen" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      mercenary <- encounterCard Cards.hydraMercenary
      resolve [RevealEncounterCard p mercenary]
      daggers <- putIntoPlay p "01046"
      wakandaForever p "01043a" [daggers]
      maybeChoose "you" (isTargetChoice p)
      villainDamageNow `shouldReturn` 2
      minionDamages `shouldReturn` [2]

  it "Killmonger takes no damage from Black Panther upgrades" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      killmonger <- encounterCard Cards.killmonger
      resolve [RevealEncounterCard p killmonger]
      daggers <- putIntoPlay p "01046"
      claws <- putIntoPlay p "01047"
      suit <- putIntoPlay p "01049"
      setDamage p 2
      wakandaForever p "01043a" [daggers]
      maybeChoose "you" (isTargetChoice p)
      villainDamageNow `shouldReturn` 1
      minionDamages `shouldReturn` [0]
      choose "claws" (isTargetChoice (upgradeIdOf claws))
      offers isMinionChoice `shouldReturn` False
      maybeChoose "Rhino" isVillainChoice
      choose "suit" (isTargetChoice (upgradeIdOf suit))
      offers isMinionChoice `shouldReturn` False
      maybeChoose "Rhino" isVillainChoice
      minionDamages `shouldReturn` [0]

  it "Tactical Genius removes threat, 2 as the final step" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      ms <- mainSchemeId
      resolve [MainSchemeMessage ms (MainSchemePlaceThreat 5)]
      threat <- mainSchemeThreatNow
      genius <- putIntoPlay p "01048"
      wakandaForever p "01043a" [genius]
      whenM (offers \case TargetLabel {} -> True; _ -> False) $ choose "the main scheme" \case TargetLabel {} -> True; _ -> False
      mainSchemeThreatNow `shouldReturn` (threat - 2)

  it "Vibranium pays 2 resources" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      claws <- putInHand p "01047"
      vibranium <- putInHand p "01044"
      playWith claws [vibranium]
      upgradesOf p `shouldReturn` [upgradeIdOf claws]

  it "The Golden City draws 2 cards, in alter-ego form" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      city <- putIntoPlay p "01045"
      choose "use The Golden City" (isAbilityOf (supportIdOf city))
      length <$> handOf p `shouldReturn` 2
      offers (isAbilityOf (supportIdOf city)) `shouldReturn` False

  it "The Golden City is an alter-ego action" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      city <- putIntoPlay p "01045"
      changeForm p
      offers (isAbilityOf (supportIdOf city)) `shouldReturn` False

  it "Affairs of State: exhaust T'Challa to remove it from the game" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      affairs <- encounterCard Cards.affairsOfState
      resolve [RevealEncounterCard p affairs]
      isExhaustedPlayer p `shouldReturn` True
      encounterDiscardCodes `shouldReturn` []

  it "Affairs of State: otherwise discard a Black Panther upgrade" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      changeForm p
      claws <- putIntoPlay p "01047"
      suit <- putIntoPlay p "01049"
      affairs <- encounterCard Cards.affairsOfState
      start [RevealEncounterCard p affairs]
      choose "stay Black Panther" (isLabel "$label.doNotFlipToAlterEgo")
      choose "discard the suit" (isTargetChoice (upgradeIdOf suit))
      upgradesOf p `shouldReturn` [upgradeIdOf claws]
      encounterDiscardCodes `shouldReturn` ["01155"]

  it "Usurp the Throne enters with 3 threat per player and adds a hazard" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      usurp <- encounterCard Cards.usurpTheThrone
      resolve [RevealEncounterCard p usurp]
      getsGame (map getSideSchemeThreat . HashMap.elems . gameSideSchemes) `shouldReturn` [3]
      getHazardCount `shouldReturn` 1

  it "Heart-Shaped Herb toughens the villain and your minions, and surges" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      mercenary <- encounterCard Cards.hydraMercenary
      resolve [RevealEncounterCard p mercenary]
      [next] <- stackEncounterDeck [Cards.imminentOverload]
      herb <- encounterCard Cards.heartShapedHerb
      resolve [RevealEncounterCard p herb]
      getsGame (map villainIsTough . HashMap.elems . gameVillains) `shouldReturn` [True]
      getsGame (map ((.minionTough) . toAttrs) . HashMap.elems . gameMinions) `shouldReturn` [True]
      -- the surge dealt the side scheme on top facedown
      dealtTo p `shouldReturn` [next]

  it "Heart-Shaped Herb's boost toughens the villain" do
    tg <- testGame ["black-panther"]
    inGame tg do
      villain <- getVillain
      herb <- encounterCard Cards.heartShapedHerb
      resolve [RevealBoostCard herb (EnemyVillainId (toId villain)), ClearBoosts]
      getsGame (map villainIsTough . HashMap.elems . gameVillains) `shouldReturn` [True]

  it "Ritual Combat deals 1 more damage than the boost icons discarded, a star not counting" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      _ <- stackEncounterDeck [Cards.heartShapedHerb]
      combat <- encounterCard Cards.ritualCombat
      resolve [RevealEncounterCard p combat]
      damageOf p `shouldReturn` 2

  it "Ritual Combat may place the threat on the main scheme instead" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      threat <- mainSchemeThreatNow
      _ <- stackEncounterDeck [Cards.imminentOverload]
      combat <- encounterCard Cards.ritualCombat
      start [RevealEncounterCard p combat]
      choose "place threat" (isLabelStarting "Place")
      mainSchemeThreatNow `shouldReturn` (threat + 4)
      damageOf p `shouldReturn` 0
