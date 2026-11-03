module Marvel.CivilWar.AtlanteansSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Keyword qualified as K
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.SideScheme.Types qualified as S
import Marvel.Treachery.Cards qualified as T
import TestImport

setPlayerDeck :: IdentityId -> [PlayerCard] -> GameAppT ()
setPlayerDeck you cards = withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck cards}

sideTokens :: SideSchemeId -> GameAppT Natural
sideTokens sid = getsGame $ maybe 0 (S.sideSchemeAccelerationTokens . toAttrs) . lookup sid . gameSideSchemes

spec :: Spec
spec = describe "Civil War Atlanteans" do
  it "Atlantean Guard has guard and discards exactly the revealing player's top three cards" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      cards <- stackDeck p ["01088","01086","01008","01002"]
      other <- unDeck . playerIdentityDeck <$> playerAttrs q
      mid <- addMinion p C.cw56165
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem K.Guard)
      take 3 <$> discardOf p `shouldReturn` take 3 cards
      headMay . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` Just (cards !! 3)
      unDeck . playerIdentityDeck <$> playerAttrs q `shouldReturn` other

  for_ [(False,["01088","01089","01090"],1),(False,["01088","01086","01008"],3),(True,["01088","01086","01008"],3)] $ \(hero,codes,count) ->
    it ("Atlanteans counts distinct discarded types: hero " <> show hero <> ", count " <> show count) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        when hero $ changeForm you
        cards <- stackDeck you (codes <> ["01002"])
        card <- encounterCard C.cw56166
        resolve [RevealEncounterCard you card]
        damageOf you `shouldReturn` if hero then count else 0
        mainSchemeThreat `shouldReturn` if hero then 0 else count
        take 3 <$> discardOf you `shouldReturn` take 3 cards

  it "Atlanteans' indirect damage can be split between the hero and an ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      cat <- putInHand you "01002"
      put <- L.runQueueT $ L.capture $ L.putIntoPlay you cat
      resolve put
      changeForm you
      _ <- stackDeck you ["01088","01086","01089","01008"]
      card <- encounterCard C.cw56166
      start [RevealEncounterCard you card]
      let decline = whenM (offers isNoResponse) $ choose "decline a response" isNoResponse >> decline
      decline
      choose "assign one damage to Black Cat" (isTargetChoice $ allyIdOf cat)
      choose "assign one damage to the hero" (isTargetChoice you)
      pass
      damageOf you `shouldReturn` 1
      allyDamage <$> allyIn "01002" `shouldReturn` 1

  it "Atlanteans deals one combined indirect damage instance for tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      resolve [IdentityMessage you IdentityBecomeTough]
      _ <- stackDeck you ["01088","01086","01008","01002"]
      card <- encounterCard C.cw56166
      resolve [RevealEncounterCard you card]
      damageOf you `shouldReturn` 0
      playerIdentityTough <$> playerAttrs you `shouldReturn` False

  it "Atlanteans counts only cards actually discarded from a short deck" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      cards <- stackDeck you ["01088","01086"]
      setPlayerDeck you cards
      card <- encounterCard C.cw56166
      resolve [RevealEncounterCard you card]
      mainSchemeThreat `shouldReturn` 2

  it "discarded Shapeshifter encounter cards count even though they go to the encounter discard" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      infiltration <- encounterCard T.mgInfiltration
      cards <- stackDeck you ["01088","01086","01008"]
      setPlayerDeck you (encounterCardInPlayerDeck you infiltration : cards)
      card <- encounterCard C.cw56166
      resolve [RevealEncounterCard you card]
      mainSchemeThreat `shouldReturn` 3
      fmap (any ((== ecCardId infiltration) . ecCardId)) getEncounterDiscard `shouldReturn` True
      fmap (any ((== ecCardId infiltration) . pcCardId)) (discardOf you) `shouldReturn` False

  it "a Mysterio-style replacement does not count as a discarded card type" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      encounter <- encounterCard M.hydraMercenary
      cards <- stackDeck you ["01088","01086","01008","01002"]
      setPlayerDeck you (encounterCardInPlayerDeck you encounter : cards)
      card <- encounterCard C.cw56166
      resolve [RevealEncounterCard you card]
      mainSchemeThreat `shouldReturn` 2
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [encounter]

  for_ [["01088","01089"],["01088","01086"]] $ \codes ->
    it ("Atlantis Attacks places one acceleration token per distinct type in " <> show codes) do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        cards <- stackDeck you (codes <> ["01002"])
        side <- encounterCard C.cw56167
        resolve [RevealEncounterCard you side]
        let sid = SideSchemeId $ unCardId $ ecCardId side
        sideSchemeThreat sid `shouldReturn` 5
        sideTokens sid `shouldReturn` if codes == ["01088","01089"] then 1 else 2
        take 2 <$> discardOf you `shouldReturn` take 2 cards
        before <- mainSchemeThreat
        resolve [AccelerateMainScheme]
        withTokens <- mainSchemeThreat
        resolve [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef you) 5]
        getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` False
        after <- mainSchemeThreat
        resolve [AccelerateMainScheme]
        withoutTokens <- mainSchemeThreat
        (withTokens - before) `shouldBe` (withoutTokens - after + if codes == ["01088","01089"] then 1 else 2)

  it "Atlantis Attacks uses its own two-player team size in a four-seat competition" do
    tg <- competitive ["spider-man","captain-marvel","black-panther","she-hulk"]
    inGame tg do
      _ <- addSecondBoard tg
      let you = last (seats tg)
      cards <- stackDeck you ["01088","01086","01008"]
      side <- encounterCard C.cw56167
      resolve [RevealEncounterCard you side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      sideSchemeThreat sid `shouldReturn` 5
      sideTokens sid `shouldReturn` 2
      take 2 <$> discardOf you `shouldReturn` take 2 cards
      headMay . unDeck . playerIdentityDeck <$> playerAttrs you `shouldReturn` Just (cards !! 2)


  it "older side-scheme saves default the revealing player to unknown" do
    tg <- testGame ["spider-man"]
    inGame tg do
      side <- encounterCard C.cw56167
      resolve [RevealEncounterCard (player1 tg) side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      attrs <- getsGame $ toAttrs . fromMaybe (error "missing Atlantis Attacks") . lookup sid . gameSideSchemes
      S.sideSchemeRevealedBy attrs `shouldBe` Just (player1 tg)
      case toJSON attrs of
        Object fields -> fromJSON (Object $ KeyMap.delete "sideSchemeRevealedBy" fields)
          `shouldBe` Success (attrs {S.sideSchemeRevealedBy = Nothing})
        _ -> error "side-scheme attrs were not an object"

  it "revealing Atlantis Attacks again updates the player without creating another scheme" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      first <- stackDeck p ["01088","01089","01008"]
      second <- stackDeck q ["01086","01008","01002"]
      side <- encounterCard C.cw56167
      resolve [RevealEncounterCard p side,RevealEncounterCard q side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      sideTokens sid `shouldReturn` 3
      sideSchemeThreat sid `shouldReturn` 5
      take 2 <$> discardOf p `shouldReturn` take 2 first
      take 2 <$> discardOf q `shouldReturn` take 2 second
