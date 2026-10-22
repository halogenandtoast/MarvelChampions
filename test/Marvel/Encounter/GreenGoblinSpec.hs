-- | The Green Goblin's scenarios: Risky Business, its villain flipped by Criminal
-- Enterprise, and Mutagen Formula.
module Marvel.Encounter.GreenGoblinSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Marvel.Damage
import Marvel.Environment.Types (Field (EnvironmentCounters))
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Cards qualified as SideSchemes
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

    it "the log tells a replaced attack or scheme as what replaces it" do
      tg <- testGameAgainst "02001a" ["spider-man"]
      let you = player1 tg
      inGame tg do
        changeForm you
        vid <- villainIdNow
        liftIO $ writeIORef tg.logged []
        resolve [VillainMessage vid (VillainAttacks you)]
        damageVillain you 3
        resolve [VillainMessage vid VillainSchemes]
      text <- T.concat . map entryText <$> readIORef tg.logged
      text `shouldSatisfy` T.isInfixOf "$log.countersPlaced"
      text `shouldSatisfy` T.isInfixOf "$log.countersRemoved"
      text `shouldSatisfy` (not . T.isInfixOf "$log.attacks")
      text `shouldSatisfy` (not . T.isInfixOf "$log.schemes")

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

  it "Overrun: a player whose discards empty the encounter deck stops; the next discards from the deck made anew" do
    tg <- testGameAgainst "02014" ["spider-man", "she-hulk"]
    let (you, them) = (player1 tg, player2 tg)
    inGame tg do
      overrun <- revealNow you SideSchemes.overrun
      resolve []
      -- one card left: the first player discards the Soldier and the deck is empty; the
      -- second discards from the discard pile shuffled anew, the Thrall among it
      _ <- setEncounterDeck [Minions.goblinSoldier] [Minions.goblinThrall]
      resolve =<< Lifted.runQueueT (Lifted.capture $ Lifted.defeatSideScheme (sideSchemeOf overrun))
      let engagedWith p def = getsGame (length . filter (\m -> Minion.minionEngagedIdentity (toAttrs m) == p && getCardDef m == def) . HashMap.elems . gameMinions)
      engagedWith you Minions.goblinSoldier `shouldReturn` 1
      -- the setup's Thrall and the one discarded
      engagedWith them Minions.goblinThrall `shouldReturn` 2

  for_ [(True, False), (False, True)] \(tough, surges) ->
    it ("Overconfidence counts the damage its attack deals: an ally's tough card preventing it, none (" <> show tough <> ")") do
      tg <- testGameAgainst "02014" ["spider-man"]
      let you = player1 tg
      inGame tg do
        changeForm you
        hellcat <- allyIdOf <$> putIntoPlay you "01020"
        when tough $ resolve [GiveStatus (AllyRef hellcat) ToughStatus]
        -- Green Goblin's 2 ATK and the boost card's 3 icons
        _ <- stackEncounterDeck [Attachments.goblinGlider]
        card <- encounterCard Treacheries.overconfidence
        start [RevealEncounterCard you card]
        let defends = \case
              AllyDefend a _ -> a == hellcat
              _ -> False
            toDefense = offers defends >>= \d -> unless d $ choose "no response" (isLabel "$label.noResponse") >> toDefense
        toDefense
        choose "Hellcat defends" defends
        pass
      ran <- readIORef tg.ran
      liftIO $ any ("Surge " `T.isPrefixOf`) ran `shouldBe` surges

  it "a minion its boost card puts into play as the villain attacks activates in the same step" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      _ <- stackEncounterDeck [Minions.goblinThrall]
      resolve [IdentityMessage you VillainAndMinionsActivate]
    ran <- readIORef tg.ran
    liftIO $ any ("MinionAttacks" `T.isInfixOf`) ran `shouldBe` True
