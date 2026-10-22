-- | The Green Goblin's scenarios: Risky Business, its villain flipped by Criminal
-- Enterprise, and Mutagen Formula.
module Marvel.Encounter.GreenGoblinSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Marvel.Damage
import Marvel.Environment.Types (Field (EnvironmentCounters))
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

-- | The environment's face and counters.
environmentNow :: GameAppT (CardCode, Natural)
environmentNow =
  getsGame (HashMap.elems . gameEnvironments) >>= \case
    [e] -> pure (toCardCode (getCardDef e), field EnvironmentCounters e)
    es -> error $ "one environment, not " <> show (length es)

villainCodeNow :: GameAppT CardCode
villainCodeNow = toCardCode . getCardDef <$> getVillain

damageVillain :: IdentityId -> Natural -> GameAppT ()
damageVillain you n = do
  vid <- villainIdNow
  resolve =<< Lifted.runQueueT (Lifted.capture $ Lifted.dealDamage vid you (toDamage n FromAbility))

spec :: Spec
spec = describe "The Green Goblin" do
  describe "Risky Business" do
    it "sets up Criminal Enterprise with 2 infamy a player; Norman Osborn's damage removes infamy instead" do
      tg <- testGameAgainst "02001a" ["spider-man"]
      inGame tg do
        environmentNow `shouldReturn` ("02006a", 2)
        damageVillain (player1 tg) 1
        villainDamageNow `shouldReturn` 0
        environmentNow `shouldReturn` ("02006a", 1)

    it "with no infamy left, Norman and Criminal Enterprise flip: Green Goblin's When Revealed, State of Madness with 2 madness a player" do
      tg <- testGameAgainst "02001a" ["spider-man"]
      let you = player1 tg
      inGame tg do
        changeForm you
        before <- damageOf you
        damageVillain you 5
        villainCodeNow `shouldReturn` "02001b"
        environmentNow `shouldReturn` ("02006b", 2)
        -- 3 indirect damage, all of it to the hero, who has no allies
        damageOf you `shouldReturn` (before + 3)
        -- Green Goblin removes madness in place of scheming; with none left both flip back
        vid <- villainIdNow
        resolve [VillainMessage vid VillainSchemes]
        environmentNow `shouldReturn` ("02006b", 1)
        resolve [VillainMessage vid VillainSchemes]
        villainCodeNow `shouldReturn` "02001a"
        environmentNow `shouldReturn` ("02006a", 2)

    it "Norman Osborn places infamy in place of attacking" do
      tg <- testGameAgainst "02001a" ["spider-man"]
      let you = player1 tg
      inGame tg do
        changeForm you
        before <- damageOf you
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainAttacks you)]
        environmentNow `shouldReturn` ("02006a", 3)
        damageOf you `shouldReturn` before

  describe "Mutagen Formula" do
    it "sets up a Goblin Thrall with each player; Mutagen Cloud accelerates by the Goblin enemies in play" do
      tg <- testGameAgainst "02014" ["spider-man"]
      inGame tg do
        thralls <- getsGame (filter ((== Minions.goblinThrall) . getCardDef) . HashMap.elems . gameMinions)
        liftIO $ length thralls `shouldBe` 1
        resolve [NextMainScheme]
        before <- mainSchemeThreatNow
        tokens <- accelerationTokensNow
        resolve [AccelerateMainScheme]
        -- Green Goblin and the Thrall
        mainSchemeThreatNow `shouldReturn` (before + 2 + tokens)

    it "Wicked Ambitions: each Goblin minion discarded, the player chooses 3 damage or to put it into play" do
      tg <- testGameAgainst "02014" ["spider-man"]
      let you = player1 tg
      inGame tg do
        [soldier, _] <- stackEncounterDeck [Minions.goblinSoldier, Minions.hiredGun]
        _ <- revealNow you Treacheries.wickedAmbitions
        choose "put it into play" (isTargetChoice soldier)
        engaged <- getsGame (filter ((== you) . Minion.minionEngagedIdentity . toAttrs) . filter ((== Minions.goblinSoldier) . getCardDef) . HashMap.elems . gameMinions)
        liftIO $ length engaged `shouldBe` 1

  it "a minion its boost card puts into play as the villain attacks activates in the same step" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      _ <- stackEncounterDeck [Minions.goblinThrall]
      resolve [IdentityMessage you VillainAndMinionsActivate]
    ran <- readIORef tg.ran
    liftIO $ any ("MinionAttacks" `T.isInfixOf`) ran `shouldBe` True
