module Marvel.Hero.StormSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query (selectAny)
import Marvel.Trait
import Marvel.Villain.Types qualified as Villain
import Marvel.Ally.Types qualified as Ally
import Marvel.Keyword
import TestImport

weatherCode :: IdentityId -> GameAppT CardCode
weatherCode p = getsGame $ toCardCode . supportCardDef . toAttrs . testHead . filter (\s -> supportController (toAttrs s) == p && Weather `elem` cdTraits (supportCardDef $ toAttrs s)) . HashMap.elems . gameSupports

weatherId :: IdentityId -> GameAppT SupportId
weatherId p = getsGame $ toId . testHead . filter (\s -> supportController (toAttrs s) == p && Weather `elem` cdTraits (supportCardDef $ toAttrs s)) . HashMap.elems . gameSupports

setWeather :: IdentityId -> CardCode -> GameAppT ()
setWeather p code = do
  current <- weatherCode p
  when (current /= code) do
    cards <- fromMaybe [] . playerIdentityWeatherDeck <$> playerAttrs p
    let card = testHead [c | c <- cards, toCardCode c == code]
    runNow [IdentityMessage p (ChooseWeatherSupport card False)]
    pass

isWeather :: CardCode -> Choice -> Bool
isWeather code = \case
  CardLabel other _ -> other == code
  _ -> False

heroStat :: IdentityId -> (Attrs Hero.Hero -> GameAppT Natural) -> GameAppT Natural
heroStat p f = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> f (toAttrs h)
  _ -> pure 0

special :: IdentityId -> GameAppT ()
special p = do
  sid <- weatherId p
  msgs <- Lifted.runQueueT $ Lifted.capture $ Lifted.resolveWeatherSpecial p sid
  runNow msgs

spec :: Spec
spec = describe "Storm" do
  it "waits for a click on Thunderstorm before offering its Special's damage target" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      setWeather p "36004"
      sid <- weatherId p
      waiting <- openQuestion
      withGame_ $ questionL .~ mempty
      msgs <- Lifted.runQueueT $ Lifted.capture $ Lifted.resolveWeatherSpecial p sid
      pushAll $ msgs <> [Ask ident q | Just (ident, q) <- [waiting]]
      runGameMessages
      villainDamageNow `shouldReturn` 0
      openQuestion >>= \case
        Just (ident, QuestionLabel "$label.resolveSpecial" (Just target) q) -> do
          liftIO $ target `shouldBe` SupportRef sid
          liftIO $ choicesOf q `shouldSatisfy` any (isTargetChoice $ SupportRef sid)
          answerQuestion ident 0
        _ -> error "expected Thunderstorm's Special prompt"
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2

  it "begins with one weather in play, three in a separate deck, and does not resolve its Special" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      cards <- fromMaybe [] . playerIdentityWeatherDeck <$> playerAttrs p
      length cards `shouldBe` 3
      current <- weatherCode p
      (current : map toCardCode cards) `shouldMatchList` ["36002", "36003", "36004", "36005"]
      length <$> handOf p `shouldReturn` 6

  it "Weather Control swaps a different support, resolves its Special, and is once per round" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      setWeather p "36004"
      offers (isNamedAbility "$label.weatherControl") `shouldReturn` False
      changeForm p
      pass
      before <- length <$> handOf p
      choose "Weather Control" (isNamedAbility "$label.weatherControl")
      offers (isWeather "36004") `shouldReturn` False
      choose "Clear Skies" (isWeather "36002")
      pass
      weatherCode p `shouldReturn` "36002"
      length <$> handOf p `shouldReturn` before + 1
      offers (isNamedAbility "$label.weatherControl") `shouldReturn` False
      cards <- fromMaybe [] . playerIdentityWeatherDeck <$> playerAttrs p
      map toCardCode cards `shouldSatisfy` elem "36004"

  it "Clear Skies makes friendly and enemy characters stalwart and removes existing statuses" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      setWeather p "36004"
      vid <- villainIdNow
      resolve [IdentityMessage p IdentityStunned, VillainMessage vid (VillainConfused GameRef)]
      setWeather p "36002"
      playerIdentityStunned <$> playerAttrs p `shouldReturn` False
      Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` False
      resolve [IdentityMessage p IdentityStunned, VillainMessage vid (VillainConfused GameRef)]
      playerIdentityStunned <$> playerAttrs p `shouldReturn` False
      Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` False

  it "Hurricane gives each character retaliate and removes two threat as a support ability" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      setWeather p "36003"
      vid <- villainIdNow
      getModifiers (IdentityRef p) >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))
      placeMainThreat 5
      before <- mainSchemeThreatNow
      special p
      mid <- mainSchemeIdNow
      choose "Hurricane target" (isTargetChoice $ MainSchemeRef mid)
      pass
      mainSchemeThreatNow `shouldReturn` before - 2

  it "Thunderstorm raises every character's ATK and its Special deals two damage without an attack" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      setWeather p "36004"
      changeForm p
      pass
      heroStat p Hero.getModifiedAttack `shouldReturn` 3
      ally <- putIntoPlay p "01067"
      a <- getsGame $ toAttrs . fromMaybe (error "Storm test ally not in play") . HashMap.lookup (allyIdOf ally) . gameAllies
      -- Maria Hill's printed ATK is 1.
      Ally.getModifiedAttack a `shouldReturn` 2
      special p
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2

  it "Blizzard reduces friendly and enemy ATK, blanks a non-Elite through the round, and preserves traits" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      setWeather p "36005"
      changeForm p
      pass
      heroStat p Hero.getModifiedAttack `shouldReturn` 1
      mid <- addMinion p Minions.hydraMercenary
      special p
      choose "blank the non-Elite" (isTargetChoice $ MinionRef mid)
      pass
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` elem Blank)
      selectAny (MinionWithId mid <> MinionWithTrait Hydra) `shouldReturn` True
      resolve [EndRound]
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` notElem Blank)

  it "Storm's Crown adds THW and generates each active weather's printed resource only in hero form" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "36006"
      clearHand p
      getAvailableResourcesFor p Nothing `shouldReturn` []
      changeForm p
      pass
      heroStat p Hero.getModifiedThwart `shouldReturn` 2
      for_ [("36002", Wild), ("36003", Physical), ("36004", Energy), ("36005", Mental)] $ \(code, resource) -> do
        setWeather p code
        getAvailableResourcesFor p Nothing `shouldReturn` [resource]

  it "Storm's Cape adds DEF and Aerial only in hero form, and readies her after a resolved Special" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      cape <- putIntoPlay p "36007"
      identityMatches (IdentityWithTrait Aerial) p `shouldReturn` False
      changeForm p
      pass
      heroStat p Hero.getModifiedDefense `shouldReturn` 2
      identityMatches (IdentityWithTrait Aerial) p `shouldReturn` True
      setWeather p "36002"
      resolve [IdentityMessage p ExhaustedIdentity]
      special p
      choose "exhaust Cape to ready Storm" (isAbilityOfCard cape)
      pass
      exhaustedNow p `shouldReturn` False
      upgradeIsExhausted (upgradeOf cape) `shouldReturn` True

  it "Blizzard with no legal target does not offer Storm's Cape" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      cape <- putIntoPlay p "36007"
      changeForm p
      pass
      setWeather p "36005"
      resolve [IdentityMessage p ExhaustedIdentity]
      special p
      offers (isAbilityOfCard cape) `shouldReturn` False
      exhaustedNow p `shouldReturn` True
      upgradeIsExhausted (upgradeOf cape) `shouldReturn` False

  it "Ororo's Garden exhausts to heal two only in alter-ego form" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      garden <- putIntoPlay p "36008"
      setDamage p 4
      choose "use the Garden" (isAbilityOfCard garden)
      pass
      damageOf p `shouldReturn` 2
      getsGame (maybe False isExhausted . HashMap.lookup (supportOf garden) . gameSupports) `shouldReturn` True
      offers (isAbilityOfCard garden) `shouldReturn` False
      changeForm p
      pass
      resolve [SupportMessage (supportOf garden) ReadiedSupport]
      offers (isAbilityOfCard garden) `shouldReturn` False

  it "Storm's Crown exhausts to pay with Hurricane's physical resource" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      crown <- putIntoPlay p "36006"
      setWeather p "36003"
      changeForm p
      pass
      clearHand p
      garden <- putInHand p "36008"
      choose "play Garden" (isPlay garden)
      choose "pay with Crown" (isAbilityOfCard crown)
      chooseIf isFinishPayment
      pass
      upgradeIsExhausted (upgradeOf crown) `shouldReturn` True
      getAvailableResourcesFor p Nothing `shouldReturn` []

  it "resolving Blizzard again on an already blank non-Elite still offers Storm's Cape" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      cape <- putIntoPlay p "36007"
      changeForm p
      pass
      setWeather p "36005"
      mid <- addMinion p Minions.hydraMercenary
      special p
      choose "first Blizzard" (isTargetChoice $ MinionRef mid)
      pass
      resolve [IdentityMessage p ExhaustedIdentity]
      special p
      choose "Blizzard again" (isTargetChoice $ MinionRef mid)
      choose "Cape after a repeated blank" (isAbilityOfCard cape)
      pass
      exhaustedNow p `shouldReturn` False

  it "Blizzard cannot target an Elite minion or ready Storm when only Elites are present" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      cape <- putIntoPlay p "36007"
      changeForm p
      pass
      setWeather p "36005"
      _ <- addMinion p Minions.sandman
      resolve [IdentityMessage p ExhaustedIdentity]
      special p
      offers (isAbilityOfCard cape) `shouldReturn` False
      exhaustedNow p `shouldReturn` True

  it "swapping Weather discards cards held under the departing support and returns only that support to its deck" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      setWeather p "36002"
      old <- weatherId p
      event <- putInHand p "36009"
      messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.attachPlayableEventToSupport old event
      resolve messages
      setWeather p "36003"
      discardOf p >>= (`shouldSatisfy` elem event)
      handOf p >>= (`shouldSatisfy` notElem event)
      cards <- fromMaybe [] . playerIdentityWeatherDeck <$> playerAttrs p
      map toCardCode cards `shouldSatisfy` elem "36002"
      map toCardCode cards `shouldSatisfy` notElem "36009"
      getsGame (HashMap.member old . gameSupports) `shouldReturn` False
      getsGame (HashMap.member old . entitiesSupports . gameRemovedEntities) `shouldReturn` True

  it "a returned Weather support enters ready without held cards and resolves its Special once" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      setWeather p "36002"
      sid <- weatherId p
      resolve [SupportMessage sid ExhaustedSupport]
      setWeather p "36003"
      setWeather p "36002"
      getsGame (maybe True isExhausted . HashMap.lookup sid . gameSupports) `shouldReturn` False
      before <- length <$> handOf p
      special p
      pass
      length <$> handOf p `shouldReturn` before + 1

  it "a stale Special message for a Weather support that left play has no effect or Cape response" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      setWeather p "36002"
      old <- weatherId p
      cape <- putIntoPlay p "36007"
      changeForm p
      pass
      setWeather p "36005"
      resolve [IdentityMessage p ExhaustedIdentity]
      before <- length <$> handOf p
      runNow [ResolveWeatherSpecial p old]
      offers (isAbilityOfCard cape) `shouldReturn` False
      length <$> handOf p `shouldReturn` before
      exhaustedNow p `shouldReturn` True

  it "retains the separate Weather deck through a saved-game reload and can swap from it" do
    tg <- testGame ["storm"]
    let p = player1 tg
    inGame tg do
      setWeather p "36004"
      changeForm p
      pass
      cards <- playerIdentityWeatherDeck <$> playerAttrs p
      saved <- toJSON <$> getGame
      case fromJSON saved of
        Success restored -> withGame_ (const restored)
        Error message -> liftIO $ expectationFailure message
      playerIdentityWeatherDeck <$> playerAttrs p `shouldReturn` cards
      weatherCode p `shouldReturn` "36004"
      choose "Weather Control after reload" (isNamedAbility "$label.weatherControl")
      choose "Clear Skies after reload" (isWeather "36002")
      pass
      weatherCode p `shouldReturn` "36002"
