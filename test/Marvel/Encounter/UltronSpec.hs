module Marvel.Encounter.UltronSpec (spec) where

import Control.Monad.Reader (runReaderT)
import Marvel.Villain (lookupVillain)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Damage
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types (getMinionFacedownCard, getModifiedAttack, getModifiedHitPoints)
import Marvel.Minion.Types qualified as Minion
import Marvel.Ref
import Marvel.Scenario.Types (scenarioDiscard)
import Marvel.Stats
import TestImport

ultron :: IO GameApp
ultron = do
  let setup = fromMaybe (error "no setup") (recommendedSetup "01134")
  (app, _) <- dealtGame setup [starter "spider-man"] 1
  runGameApp app toFirstTurn
  pure app

drones :: GameAppT [Minion]
drones = getsGame $ filter ((== Cards.drone) . getCardDef) . HashMap.elems . gameMinions

player :: GameAppT PlayerIdentity
player = players >>= \case
  [p] -> getIdentity p
  _ -> error "one player"

spec :: Spec
spec = describe "Ultron" do
  it "checks drone blanking without evaluating Ultron III's unrelated trait modifiers" do
    app <- ultron
    runGameApp app do
      g <- getGame
      vid <- villainIdNow
      let stageThree = fromMaybe (error "missing Ultron III") (lookupVillain "01136" vid)
          snapshot = g
            { gameEntities = (gameEntities g) {entitiesVillains = HashMap.singleton vid stageThree}
            , gameGainedTraits = error "blanking must not calculate unrelated traits"
            }
      ds <- drones
      length ds `shouldBe` 1
      for_ ds $ \drone -> liftIO $ runReaderT (isBlanked drone) snapshot `shouldReturn` False

  it "sets up the Ultron Drones environment and deals a facedown drone from the player's deck" do
    app <- ultron
    runGameApp app do
      environments <- getsGame (map getCardDef . HashMap.elems . gameEnvironments)
      liftIO $ environments `shouldBe` [Cards.ultronDrones]
      ds <- drones
      liftIO $ length ds `shouldBe` 1
      p <- player
      for_ ds \drone -> do
        let attrs = toAttrs drone
        liftIO $ Minion.minionEngagedIdentity attrs `shouldBe` toId p
        liftIO $ unSch (Minion.minionScheme attrs) `shouldBe` 1
        getModifiedAttack attrs >>= liftIO . (`shouldBe` 1)
        getModifiedHitPoints attrs >>= liftIO . (`shouldBe` 1)
        -- the card is no longer in the deck
        let card = fromMaybe (error "Ultron fixture: a drone must retain its physical facedown card") (getMinionFacedownCard drone)
        liftIO $ map pcCardId (unDeck $ playerIdentityDeck $ toAttrs p) `shouldNotContain` [pcCardId card]
      -- and the game, environment and drone included, is saved and loaded as it is
      g <- getGame
      liftIO $ (fromJSON (toJSON g) :: Result Game) `shouldBe` Success g

  it "a defeated facedown drone goes to its owner's discard pile, and Upgraded Drones makes them bigger" do
    app <- ultron
    runGameApp app do
      [you] <- players
      upgraded <- encounterCard Cards.upgradedDrones
      resolve [RevealEncounterCard you upgraded]
      ds <- drones
      for_ ds \drone -> do
        getModifiedAttack (toAttrs drone) >>= liftIO . (`shouldBe` 2)
        getModifiedHitPoints (toAttrs drone) >>= liftIO . (`shouldBe` 2)
      resolve [PutTopOfDeckIntoPlayAsDrone you]
      ds' <- drones
      liftIO $ length ds' `shouldBe` 2
      for_ ds' \drone ->
        resolve [MinionMessage (toId drone) $ MinionDamaged GameRef (toDamage 2 FromAbility)]
      drones >>= liftIO . (`shouldBe` 0) . length
      discard <- map pcCardId . unDiscard . playerIdentityDiscard . toAttrs <$> player
      liftIO $ mapMaybe (fmap pcCardId . getMinionFacedownCard) ds' `shouldSatisfy` all (`elem` discard)
      encounterDiscard <- getsGame (map getCardDef . scenarioDiscard . toAttrs . gameScenario)
      liftIO $ encounterDiscard `shouldNotContain` [Cards.drone]

  it "discarding Upgraded Drones defeats a drone with damage equal to its remaining hit points" do
    app <- ultron
    runGameApp app do
      [you] <- players
      upgraded <- encounterCard Cards.upgradedDrones
      resolve [RevealEncounterCard you upgraded]
      resolve [PutTopOfDeckIntoPlayAsDrone you]
      [hurt, fresh] <- drones
      resolve [MinionMessage (toId hurt) $ MinionDamaged GameRef (toDamage 1 FromAbility)]
      map toId <$> drones >>= liftIO . (`shouldBe` [toId hurt, toId fresh])
      resolve [RemoveFromPlay $ AttachmentRef $ AttachmentId $ unCardId $ ecCardId upgraded]
      -- the undamaged drone survives at 1 hit point
      map toId <$> drones >>= liftIO . (`shouldBe` [toId fresh])
      discard <- map pcCardId . unDiscard . playerIdentityDiscard . toAttrs <$> player
      liftIO $ mapMaybe (fmap pcCardId . getMinionFacedownCard) [hurt] `shouldSatisfy` all (`elem` discard)
