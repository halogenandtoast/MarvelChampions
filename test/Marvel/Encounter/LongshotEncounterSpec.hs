module Marvel.Encounter.LongshotEncounterSpec (spec) where

import Data.Aeson (Result (..), fromJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Ally.Cards qualified as Allies
import Marvel.Ally.Types qualified as Ally
import Marvel.EncounterSet qualified as Set
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

spec :: Spec
spec = describe "Mojo Mania Longshot" do
  it "forms an optional encounter set with exactly one encounter ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      cards <- gatherEncounterSet Set.LongshotSet
      liftIO $ map toCardCode cards `shouldBe` ["39071"]

  it "enters under the revealing player's control, gains piercing and ignores the ally limit" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      ([card, _], _) <- setEncounterDeck [Allies.mojo39071, Treacheries.advance] []
      resolve =<< Lifted.runQueueT (Lifted.capture $ Lifted.revealEncounterCard you card)
      a <- allyIn "39071"
      liftIO $ Ally.allyController a `shouldBe` you
      mods <- getModifiers (AllyRef $ Ally.allyId a)
      liftIO $ mods `shouldContain` [NotCountedAgainstAllyLimit, KeywordModifier Piercing]

  it "returns to the encounter discard pile when defeated" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      cards <- gatherEncounterSet Set.LongshotSet
      for_ cards $ \card -> resolve =<< Lifted.runQueueT (Lifted.capture $ Lifted.revealEncounterCard you card)
      a <- allyIn "39071"
      resolve =<< Lifted.runQueueT (Lifted.capture $ Lifted.defeatAlly $ Ally.allyId a)
      map toCardCode <$> encounterDiscard >>= (`shouldSatisfy` elem "39071")

  it "preserves character threat when saving and loading a game" do
    tg <- testGame ["spider-man"]
    inGame tg $ resolve [PlaceCharacterThreat (IdentityCharacter $ player1 tg) 4]
    saved <- inGame tg getGame
    fromJSON (toJSON saved) `shouldBe` Success saved

  it "loads games saved before characters could carry threat" do
    tg <- testGame ["spider-man"]
    saved <- inGame tg getGame
    let legacy = case toJSON saved of
          Object o -> Object $ KeyMap.delete "characterThreat" o
          value -> value
    fromJSON legacy `shouldBe` Success saved {gameCharacterThreat = []}

  it "moves threat before eliminating its player and opening placement interrupts" do
    tg <- testGameAgainst "39022" ["spider-man", "she-hulk"]
    inGame tg do
      let you = player1 tg
      before <- mainSchemeThreatNow
      runNow [PlaceCharacterThreat (IdentityCharacter you) 3, RemoveFromPlay $ IdentityRef you]
      pass
      mainSchemeThreatNow `shouldReturn` (before + 3)
      getCharacterThreat (IdentityCharacter you) `shouldReturn` 0
      length <$> getPlayers `shouldReturn` 1
