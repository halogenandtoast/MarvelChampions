module Marvel.CivilWar.MightyAvengersSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Villain.Types qualified as V
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait (Trait (Avenger))
import Marvel.Keyword qualified as K
import Marvel.Minion.Cards qualified as M
import Marvel.Message.Lifted qualified as L
import TestImport

spec :: Spec
spec = describe "Civil War Mighty Avengers minions" do
  it "Yellow Jacket enters tough and gives the enemy leader tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      mid <- addMinion (player1 tg) C.cw56073
      getsGame (maybe False (minionTough . toAttrs) . lookup mid . gameMinions) `shouldReturn` True
      getsGame (maybe False (V.villainTough . toAttrs) . lookup vid . gameVillains) `shouldReturn` True

  it "Wasp places one threat on the main scheme when revealed" do
    tg <- testGame ["spider-man"]
    inGame tg do
      before <- mainSchemeThreat
      _ <- addMinion (player1 tg) C.cw56074
      mainSchemeThreat `shouldReturn` (before + 1)

  it "U.S. Agent damages the revealing player and enters tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you C.cw56075
      damageOf you `shouldReturn` 1
      getsGame (maybe False (minionTough . toAttrs) . lookup mid . gameMinions) `shouldReturn` True

  it "Yellow Jacket's boost gives the enemy leader tough without putting the minion into play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      _ <- stackEncounterDeck [C.cw56073]
      resolve [VillainMessage vid VillainSchemes]
      getsGame (maybe False (V.villainTough . toAttrs) . lookup vid . gameVillains) `shouldReturn` True
      selectList (MinionIs C.cw56073) `shouldReturn` []

  it "Wasp's boost places additional threat during a scheme activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      before <- mainSchemeThreat
      _ <- stackEncounterDeck [C.cw56074]
      resolve [VillainMessage vid VillainSchemes]
      mainSchemeThreat `shouldReturn` (before + 2)

  it "U.S. Agent's boost damages the player during a scheme activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      _ <- stackEncounterDeck [C.cw56075]
      resolve [VillainMessage vid VillainSchemes]
      damageOf (player1 tg) `shouldReturn` 1


  it "Earth's Mightiest Heroes shares printed keywords and stops sharing when their source leaves" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      wasp <- addMinion you C.cw56074
      agent <- addMinion you C.cw56075
      side <- encounterCard C.cw56077
      resolve [RevealEncounterCard you side]
      sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId side) `shouldReturn` 4
      keywords <- getEnemyKeywords (EnemyMinionId wasp)
      keywords `shouldSatisfy` elem (K.Retaliate 1)
      keywords `shouldSatisfy` elem K.Toughness
      getEnemyKeywords (EnemyMinionId agent) >>= (`shouldSatisfy` elem (K.Teamwork Avenger))
      messages <- L.runQueueT $ L.capture $ L.removeFromPlay agent
      resolve messages
      getEnemyKeywords (EnemyMinionId wasp) >>= (`shouldSatisfy` notElem (K.Retaliate 1))

  for_ [C.cw56076,C.cw56081] $ \def ->
    it (show (toCardCode def) <> " repeats all printed minion reveal abilities on the resolving player") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        _ <- addMinion (player1 tg) C.cw56074
        _ <- addMinion (player2 tg) C.cw56075
        before <- mainSchemeThreat
        card <- encounterCard def
        resolve [RevealEncounterCard (player1 tg) card]
        mainSchemeThreat `shouldReturn` (before + 1)
        damageOf (player1 tg) `shouldReturn` 1
        damageOf (player2 tg) `shouldReturn` 1

  it "Mighty Avengers surges when no minion has a printed reveal ability" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- addMinion (player1 tg) M.hydraMercenary
      [next] <- stackEncounterDeck [M.hydraSoldier]
      card <- encounterCard C.cw56076
      resolve [RevealEncounterCard (player1 tg) card]
      playerIdentityEncounterCards <$> playerAttrs (player1 tg) `shouldReturn` [next]


  it "keeps shared keywords and repeated reveal effects on the resolving team's board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- addSecondBoard tg
      own <- addMinion (player1 tg) C.cw56074
      other <- addMinion (player2 tg) C.cw56075
      side <- encounterCard C.cw56077
      resolve [RevealEncounterCard (player1 tg) side]
      asYou (player1 tg) (getEnemyKeywords (EnemyMinionId own)) >>= (`shouldSatisfy` notElem (K.Retaliate 1))
      asYou (player2 tg) (getEnemyKeywords (EnemyMinionId other)) >>= (`shouldSatisfy` notElem (K.Teamwork Avenger))
      before <- mainSchemeThreat
      card <- encounterCard C.cw56076
      resolve [RevealEncounterCard (player1 tg) card]
      mainSchemeThreat `shouldReturn` (before + 1)
      damageOf (player1 tg) `shouldReturn` 0


  it "repeats Surge as a When Revealed ability under Rules Reference 1.8" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      runner <- encounterCard M.weaponsRunner
      resolve [PutEncounterCardIntoPlay you runner]
      [next] <- stackEncounterDeck [M.hydraSoldier]
      card <- encounterCard C.cw56076
      resolve [RevealEncounterCard you card]
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [next]
