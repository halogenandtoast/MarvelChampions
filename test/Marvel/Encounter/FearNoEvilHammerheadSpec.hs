module Marvel.Encounter.FearNoEvilHammerheadSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Types qualified as Minion
import Marvel.Query
import Marvel.Villain.Types qualified as Villain
import TestImport

spec :: Spec
spec = describe "Fear No Evil Hammerhead behaviors" do
  for_ [("60087",15,1),("60088",16,2)] $ \(code,hp,retaliate) ->
    it ("stage " <> show code <> " has its hit points, toughness, and retaliation") do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        resolve [ReplaceVillain vid code Advanced]
        getVillainHitPoints vid `shouldReturn` hp
        getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1
        villain <- getVillain
        Villain.getModifiedKeywords (toAttrs villain) >>= \keywords -> liftIO $ keywords `shouldContain` [Retaliate retaliate]

  it "Chameleon adds the highest current THW to its SCH and schemes when revealed" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "01084"
      before <- mainSchemeThreatNow
      _ <- revealNow you Cards.street60091
      pass
      mid <- selectJust $ MinionIs Cards.street60091
      attrs <- getsGame $ toAttrs . (HashMap.! mid) . gameMinions
      Minion.getModifiedScheme attrs `shouldReturn` 3
      mainSchemeThreatNow `shouldReturn` (before + 3)

  it "Maggia Enforcers counts itself, another Enforcers, and the Maggia villain" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60086" Advanced]
      mid <- addMinion you Cards.street60092
      other <- addMinion you Cards.street60092
      attrs <- getsGame $ toAttrs . (HashMap.! mid) . gameMinions
      Minion.getModifiedScheme attrs `shouldReturn` 3
      minionMatches (MinionWithKeyword Guard) mid `shouldReturn` True
      resolve =<< L.runQueueT (L.capture $ L.defeatMinion other)
      Minion.getModifiedScheme attrs `shouldReturn` 2

  it "Consolidate Power gives the defeated minion's physical card to the villain as a boost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Cards.street60094
      pass
      mid <- addMinion you Cards.street60093
      resolve =<< L.runQueueT (L.capture $ L.defeatMinion mid)
      pass
      boosts <- Villain.villainBoostCards . toAttrs <$> getVillain
      map ecCardId boosts `shouldBe` [CardId $ unMinionId mid]
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldNotContain` map ecCardId boosts

  it "Power Struggle defeats the minion with the fewest remaining hit points before revealing its replacement" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      hurt <- addMinion you Cards.street60091
      healthy <- addMinion you Cards.street60092
      resolve [MinionMessage hurt $ MinionDamaged GameRef $ toDamage 4 FromAbility]
      _ <- setEncounterDeck [Cards.street60188,Cards.street60093,Cards.street60188] []
      _ <- revealNow you Cards.street60095
      choose "defeat the wounded Chameleon" (isTargetChoice hurt)
      pass
      selectAny (MinionWithId hurt) `shouldReturn` False
      selectAny (MinionWithId healthy) `shouldReturn` True
      selectAny (MinionIs Cards.street60093) `shouldReturn` True

