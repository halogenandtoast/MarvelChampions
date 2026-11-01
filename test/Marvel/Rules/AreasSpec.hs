module Marvel.Rules.AreasSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Area
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Game.Public (publicGame)
import Marvel.MainScheme (lookupMainScheme)
import Marvel.Modifier (Modifier (RedirectAccelerationTokens))
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Scenario.Types (Attrs (scenarioDecks))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

-- ponytail: no scenario has several villains or areas yet, so these set them up by hand:
-- Klaw beside Rhino, a second area for the second player.

-- | Add the villain to the area of the player given, returning him.
addVillainFor :: IdentityId -> CardCode -> GameAppT VillainId
addVillainFor ident code = newIn (HashMap.keys . gameVillains) do
  active <- getActivePlayerId
  resolve [SetActivePlayer ident, AddVillain code, SetActivePlayer active]

addMainSchemeFor :: IdentityId -> CardCode -> GameAppT MainSchemeId
addMainSchemeFor ident code = newIn (HashMap.keys . gameMainSchemes) do
  active <- getActivePlayerId
  resolve [SetActivePlayer ident, AddMainScheme code, SetActivePlayer active]

newIn :: (Eq a) => (Game -> [a]) -> GameAppT () -> GameAppT a
newIn keys body = do
  before <- getsGame keys
  body
  getsGame (filter (`notElem` before) . keys) >>= \case
    [x] -> pure x
    _ -> error "nothing new entered play"

-- | A test-only card's effect: whatever modifiers it is given.
newtype RedirectTest = RedirectTest (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage RedirectTest where
  runMessage _ = pure

testEffectAttrs :: EffectId -> Source -> EntityMatcher -> Attrs Effect
testEffectAttrs eid source matcher = EffectAttrs eid "01092" source matcher [] Nothing

-- | A second area made for the second player.
splitOff :: IdentityId -> GameAppT AreaId
splitOff ident = do
  resolve [CreateArea (AreaId 7) [ident]]
  pure (AreaId 7)

areasNow :: GameAppT [Area]
areasNow = getsGame gameAreas

activeVillainFor :: IdentityId -> GameAppT VillainId
activeVillainFor ident = asYou ident (selectJust ActiveVillain)

pileNow :: DeckKey -> GameAppT Pile
pileNow = getPile

ranMessage :: TestGame -> Message -> GameAppT Bool
ranMessage tg msg = elem (tshow msg) <$> liftIO (readIORef tg.ran)

spec :: Spec
spec = describe "game areas" do
  it "a game is one area holding every player, whose villain is the villain" do
    tg <- testGame ["spider-man", "thor"]
    inGame tg do
      [area] <- areasNow
      ps <- players
      areaPlayers area `shouldSatisfy` \xs -> all (`elem` xs) ps && length xs == length ps
      rhino <- villainIdNow
      activeVillainFor (player1 tg) `shouldReturn` rhino

  it "a new area takes the player given; there the villain and the main scheme are its own" do
    tg <- testGame ["spider-man", "thor"]
    inGame tg do
      [p1, p2] <- players
      rhino <- villainIdNow
      rhinoScheme <- mainSchemeIdNow
      aid <- splitOff p2
      areaId <$> getAreaOf p2 `shouldReturn` aid
      areaId <$> getAreaOf p1 `shouldReturn` defaultAreaId
      klaw <- addVillainFor p2 "01113"
      klawScheme <- addMainSchemeFor p2 "01116"
      activeVillainFor p1 `shouldReturn` rhino
      activeVillainFor p2 `shouldReturn` klaw
      asYou p1 (selectList MainScheme) `shouldReturn` [SchemeMainSchemeId rhinoScheme]
      asYou p2 (selectList MainScheme) `shouldReturn` [SchemeMainSchemeId klawScheme]

  it "players cannot target what is in another area" do
    tg <- testGame ["spider-man", "thor"]
    inGame tg do
      [p1, p2] <- players
      _ <- splitOff p2
      klaw <- addVillainFor p2 "01113"
      asYou p1 (selectList AnyEnemy) >>= (`shouldSatisfy` notElem (EnemyVillainId klaw))
      asYou p2 (selectList AnyEnemy) >>= (`shouldSatisfy` elem (EnemyVillainId klaw))
      asYou p1 (selectList AnyIdentity) `shouldReturn` [p1]

  it "a player moved to another area plays against its villain" do
    tg <- testGame ["spider-man", "thor"]
    inGame tg do
      [_, p2] <- players
      rhino <- villainIdNow
      aid <- splitOff p2
      klaw <- addVillainFor p2 "01113"
      resolve [MovePlayerToArea p2 defaultAreaId]
      areaId <$> getAreaOf p2 `shouldReturn` defaultAreaId
      activeVillainFor p2 `shouldReturn` rhino
      getVillainsInArea aid `shouldReturn` [klaw]

  it "combining areas puts everything in one, which keeps its active villain" do
    tg <- testGame ["spider-man", "thor"]
    inGame tg do
      [p1, p2] <- players
      rhino <- villainIdNow
      aid <- splitOff p2
      klaw <- addVillainFor p2 "01113"
      resolve [CombineAreas aid defaultAreaId]
      length <$> areasNow `shouldReturn` 1
      getVillainsInArea defaultAreaId `shouldReturn` [rhino, klaw]
      activeVillainFor p2 `shouldReturn` rhino
      asYou p1 (selectList AnyEnemy) >>= (`shouldSatisfy` elem (EnemyVillainId klaw))

  it "the active villain is the one that activates in his area" do
    tg <- testGame ["spider-man", "thor"]
    let p = player1 tg
    inGame tg do
      klaw <- addVillainFor p "01113"
      resolve [SetActiveVillain klaw]
      activeVillainFor p `shouldReturn` klaw
      runNow [IdentityMessage p VillainAndMinionsActivate]
      ranMessage tg (VillainMessage klaw VillainBeginScheme) `shouldReturn` True

  it "the villain phase activates each area's villain against its own players" do
    tg <- testGame ["spider-man", "thor"]
    inGame tg do
      [_, p2] <- players
      _ <- splitOff p2
      klaw <- addVillainFor p2 "01113"
      runNow [IdentityMessage p2 VillainAndMinionsActivate]
      ranMessage tg (VillainMessage klaw VillainBeginScheme) `shouldReturn` True

  it "a villain defeated with another left leaves play, his deck and side scheme with him" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      rhino <- villainIdNow
      klaw <- addVillainFor p "01113"
      resolve [SetActiveVillain klaw]
      own <- sideSchemeOf <$> revealNow p SideSchemes.crowdControl
      deck <- traverse encounterCard [Treacheries.advance]
      resolve [SetSignatureScheme klaw own, CreateDeck (VillainDeckKey klaw) deck]
      resolve [ResolveVillainDefeat klaw]
      getsGame (HashMap.member klaw . gameVillains) `shouldReturn` False
      getsGame (HashMap.member own . gameSideSchemes) `shouldReturn` False
      getsGame (HashMap.member (VillainDeckKey klaw) . scenarioDecksOf) `shouldReturn` False
      activeVillainFor p `shouldReturn` rhino
      getsGame gameState `shouldReturn` InProgress
      resolve [ResolveVillainDefeat rhino]
      getsGame gameState `shouldReturn` Finished Won

  it "a villain schemes onto his own side scheme, which no threat does not discard" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      rhino <- villainIdNow
      own <- sideSchemeOf <$> revealNow p SideSchemes.crowdControl
      resolve [SetSignatureScheme rhino own]
      before <- mainSchemeThreatNow
      threat <- sideSchemeThreat own
      runNow [VillainMessage rhino VillainSchemes]
      mainSchemeThreatNow `shouldReturn` before
      sideSchemeThreat own >>= (`shouldSatisfy` (> threat))
      after <- sideSchemeThreat own
      resolve [SideSchemeMessage own (SideSchemeThwarted GameRef after), RemoveFromPlay (SideSchemeRef own)]
      getsGame (HashMap.member own . gameSideSchemes) `shouldReturn` True

  it "the encounter deck is the active villain's own, when he has one" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      klaw <- addVillainFor p "01113"
      resolve [SetActiveVillain klaw]
      deck <- traverse encounterCard [Treacheries.advance, Treacheries.gangUp]
      resolve [CreateDeck (VillainDeckKey klaw) deck]
      getDeckKeyFor p `shouldReturn` VillainDeckKey klaw
      encounterBefore <- encounterDeckSize
      dealtBefore <- dealtTo p
      runNow [DealEncounterCard p]
      length <$> dealtTo p `shouldReturn` length dealtBefore + 1
      length . pileDeck <$> pileNow (VillainDeckKey klaw) `shouldReturn` 1
      encounterDeckSize `shouldReturn` encounterBefore

  it "a villain's deck that empties is made again from its own discard pile, with an acceleration token" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      klaw <- addVillainFor p "01113"
      resolve [SetActiveVillain klaw]
      deck@[card] <- traverse encounterCard [Treacheries.advance]
      resolve [CreateDeck (VillainDeckKey klaw) deck]
      before <- accelerationTokensNow
      encounterDiscardBefore <- length <$> encounterDiscard
      resolve [DiscardedCard (EncounterCard card)]
      pileNow (VillainDeckKey klaw) `shouldReturn` Pile [card] []
      accelerationTokensNow `shouldReturn` before + 1
      length <$> encounterDiscard `shouldReturn` encounterDiscardBefore

  it "a named deck's card is the encounter deck's once in play, and the deck is not made again" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      let weapons = NamedDeckKey "experimentalWeapons"
      deck@[card] <- traverse encounterCard [Minions.hydraMercenary]
      resolve [CreateDeck weapons deck]
      before <- accelerationTokensNow
      runNow [UsingDeck weapons (DrawAndRevealEncounterCard p)]
      getsGame (HashMap.member (minionOf card) . gameMinions) `shouldReturn` True
      resolve [RemoveFromPlay (MinionRef $ minionOf card)]
      (toCardCode card `elem`) <$> encounterDiscardCodes `shouldReturn` True
      pileNow weapons `shouldReturn` Pile [] []
      runNow [UsingDeck weapons (DrawAndRevealEncounterCard p)]
      accelerationTokensNow `shouldReturn` before

  it "a single area's acceleration tokens are on its main scheme, and accelerate it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      ms <- mainSchemeIdNow
      resolve [AccelerateMainScheme]
      before <- mainSchemeThreatNow
      resolve [AccelerateMainScheme]
      once <- mainSchemeThreatNow
      resolve [PlaceAccelerationToken ms]
      accelerationTokensNow `shouldReturn` 1
      resolve [AccelerateMainScheme]
      mainSchemeThreatNow `shouldReturn` once + (once - before) + 1

  it "a card may redirect an acceleration token placed on a scheme to another scheme" do
    tg <- testGame ["spider-man", "thor"]
    let (p, q) = (player1 tg, player2 tg)
    inGame tg do
      ms <- mainSchemeIdNow
      there <- splitOff q
      other <- addMainSchemeFor q "01097"
      eid <- getRandom
      let redirect = RedirectTest (testEffectAttrs eid (toSource p) (MainSchemeEntity ms)) {effectModifiers = [RedirectAccelerationTokens other]}
      withGame_ $ entitiesL . effectsL %~ insertMap eid (Effect redirect)
      before <- getAccelerationCountIn there
      resolve [PlaceAccelerationToken ms]
      getAccelerationCountIn there `shouldReturn` before + 1
      getsGame (map areaAccelerationTokens . gameAreas) `shouldReturn` [0, 1]

  it "The Master of Time redirects acceleration tokens only once flipped to 2B" do
    tg <- testGame ["spider-man", "thor"]
    let q = player2 tg
    inGame tg do
      _ <- splitOff q
      other <- addMainSchemeFor q "01097"
      -- 2A, its When Revealed not yet resolved, in the first area
      mid <- getRandom
      let master = fromMaybe (error "no Master of Time") (lookupMainScheme "11008" mid)
      withGame_ $ entitiesL . mainSchemesL %~ insertMap mid master
      resolve [PlaceAccelerationToken other]
      getsGame (map areaAccelerationTokens . gameAreas) `shouldReturn` [0, 1]
      resolve [CardSpecific (MainSchemeRef mid) "flipToB" Null]
      resolve [PlaceAccelerationToken other]
      getsGame (map areaAccelerationTokens . gameAreas) `shouldReturn` [1, 1]

  it "the side-scheme deck is made again from its discard pile without penalty" do
    tg <- testGame ["spider-man"]
    inGame tg do
      deck@[card] <- traverse encounterCard [SideSchemes.crowdControl]
      resolve [CreateDeck SideSchemeDeckKey deck]
      before <- accelerationTokensNow
      resolve [UsingDeck SideSchemeDeckKey (DiscardTopOfEncounterDeck 1 Nothing)]
      pileNow SideSchemeDeckKey `shouldReturn` Pile [card] []
      accelerationTokensNow `shouldReturn` before

  it "the last player of an area defeated, with players left elsewhere, defeats the area" do
    tg <- testGame ["spider-man", "thor"]
    inGame tg do
      [p1, p2] <- players
      aid <- splitOff p2
      resolve [RemoveFromPlay (IdentityRef p2)]
      ranMessage tg (AreaDefeated aid) `shouldReturn` True
      getsGame gameState `shouldReturn` InProgress
      areaPlayers <$> getAreaOf p1 `shouldReturn` [p1]

  it "a defeated villain's kept deck is shown in the area he was in" do
    tg <- testGame ["spider-man", "thor"]
    inGame tg do
      [_, p2] <- players
      aid <- splitOff p2
      klaw <- addVillainFor p2 "01113"
      deck@[card] <- traverse encounterCard [Treacheries.advance]
      resolve [CreateDeck (VillainDeckKey klaw) deck]
      -- kept for his cards still in play, as Breakout keeps it
      resolve [RemoveFromPlay (VillainRef klaw), CreateDeck (VillainDeckKey klaw) [card]]
      view <- liftIO . publicGame =<< getGame
      let decksOf = \case
            Object a | Just (Array ds) <- KeyMap.lookup "decks" a -> [d | Object d <- toList ds, KeyMap.lookup "kind" d == Just (String "villain")]
            _ -> []
          areas = case view of
            Object o | Just (Array as) <- KeyMap.lookup "areas" o -> [(KeyMap.lookup "id" a, decksOf (Object a)) | Object a <- toList as]
            _ -> []
      [(i, length ds) | (i, ds) <- areas] `shouldBe` [(Just (toJSON defaultAreaId), 0), (Just (toJSON aid), 1)]
 where
  scenarioDecksOf = scenarioDecks . toAttrs . gameScenario
