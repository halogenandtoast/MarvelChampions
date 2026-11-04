module Marvel.Encounter.FearNoEvilPurpleManSpec (spec) where

import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import Marvel.Trait qualified as Trait
import TestImport

spec :: Spec
spec = describe "Fear No Evil Purple Man behaviors" do
  it "Judge places three threat when defeated, while discarding it through vulnerable avoids When Defeated" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you Cards.street60102
      before <- mainSchemeThreatNow
      resolve =<< L.runQueueT (L.capture $ L.defeatMinion mid)
      mainSchemeThreatNow `shouldReturn` (before + 3)
      second <- addMinion you Cards.street60102
      resolve [GiveStatus (MinionRef second) ConfusedStatus]
      selectAny (MinionWithId second) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` (before + 3)

  it "Security Guard discards the ally with the highest ATK across all players" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      weak <- putIntoPlay (player1 tg) "01002"
      strong <- putIntoPlay (player2 tg) "01050"
      mid <- addMinion (player1 tg) Cards.street60103
      resolve =<< L.runQueueT (L.capture $ L.defeatMinion mid)
      selectAny (AllyWithId $ allyIdOf weak) `shouldReturn` True
      selectAny (AllyWithId $ allyIdOf strong) `shouldReturn` False
      discardOf (player2 tg) >>= \cards -> liftIO $ map pcCardId cards `shouldContain` [pcCardId strong]

  it "Skater Punk damages the player whose attack defeated it rather than the engaged player" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let attacker = player1 tg
          engaged = player2 tg
      mid <- addMinion engaged Cards.street60104
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef attacker) $ toDamage 2 $ FromPlayerAttack attacker]
      damageOf attacker `shouldReturn` 3
      damageOf engaged `shouldReturn` 0

  it "Serve exhausts once per round and leaves play when its third command counter is spent" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- revealNow you Cards.street60107
      pass
      let oid = ObligationId $ unCardId $ ecCardId card
      before <- mainSchemeThreatNow
      for_ [1..3 :: Int] $ \turn -> do
        choose "obey Serve" (isAbilityOf $ ObligationRef oid)
        pass
        mainSchemeThreatNow `shouldReturn` (before + fromIntegral (2 * turn))
        offers (isAbilityOf $ ObligationRef oid) `shouldReturn` False
        when (turn < 3) $ resolve [IdentityMessage you ReadyCards]
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [ecCardId card]

  it "Fight can damage an ally and cannot be used again until its owner readies cards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      ally <- putIntoPlay you "01050"
      card <- revealNow you Cards.street60105
      pass
      let oid = ObligationId $ unCardId $ ecCardId card
      choose "obey Fight" (isAbilityOf $ ObligationRef oid)
      choose "damage Hulk" (isTargetChoice $ allyIdOf ally)
      pass
      allyDamage <$> allyIn "01050" `shouldReturn` 2
      offers (isAbilityOf $ ObligationRef oid) `shouldReturn` False

  it "Protect becomes unavailable while the villain already has tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      card <- revealNow you Cards.street60106
      pass
      let oid = ObligationId $ unCardId $ ecCardId card
      choose "obey Protect" (isAbilityOf $ ObligationRef oid)
      pass
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1
      resolve [IdentityMessage you ReadyCards]
      offers (isAbilityOf $ ObligationRef oid) `shouldReturn` False

  it "Power of Suggestion adds its per-player hinder and grants Influenced until it leaves play" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60092
      _ <- revealNow (player1 tg) Cards.street60108
      pass
      sid <- selectJust $ SideSchemeIs Cards.street60108
      getSchemeThreatAmount (SchemeSideSchemeId sid) `shouldReturn` 8
      minionMatches (MinionWithTrait Trait.Influenced) mid `shouldReturn` True
      resolve [RemoveFromPlay $ SideSchemeRef sid]
      minionMatches (MinionWithTrait Trait.Influenced) mid `shouldReturn` False

  it "Sway the Masses reveals the chosen Influenced minion from the discard pile" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      (_,[judge,_]) <- setEncounterDeck [Cards.street60188] [Cards.street60102,Cards.street60092]
      _ <- revealNow you Cards.street60109
      choose "reveal Judge" (\case CardLabel code _ -> code == "60102"; _ -> False)
      pass
      selectAny (MinionWithId $ MinionId $ unCardId $ ecCardId judge) `shouldReturn` True
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldNotContain` [ecCardId judge]
