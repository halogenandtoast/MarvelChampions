module Marvel.Encounter.NextEvolutionCampaignSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Environment.Cards qualified as Environments
import Marvel.Keyword
import Marvel.Modifier
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Upgrade.Types qualified as Upgrade
import TestImport

isEnvironmentAbility :: EncounterCard -> Choice -> Bool
isEnvironmentAbility card = \case
  UseAbility a -> abilitySource a == EnvironmentRef (EnvironmentId $ unCardId $ toCardId card)
  _ -> False

addEnvironment :: IdentityId -> CardDef -> GameAppT EncounterCard
addEnvironment you def = do
  card <- encounterCard def
  resolve [PutEncounterCardIntoPlay you card]
  pure card

putInDeck :: IdentityId -> CardCode -> GameAppT PlayerCard
putInDeck you code = do
  card <- putInHand you code
  withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity a) -> PlayerIdentity a
    {playerIdentityDeck = Deck (card : unDeck a.playerIdentityDeck)
    ,playerIdentityHand = Hand (filter (/= card) $ unHand a.playerIdentityHand)}
  pure card

spec :: Spec
spec = describe "Next Evolution campaign cards" do
  for_ [("40190a","40190b"),("40191a","40191b"),("40192a","40192b"),("40193a","40193b"),("40194a","40194b"),("40195a","40195b")] \(front,back) ->
    it ("flips its defeated campaign front to the reward environment: " <> show front) do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- putIntoPlay (player1 tg) front
        let sid = SideSchemeId $ unCardId $ toCardId card
        sideSchemeThreat sid `shouldReturn` 4
        resolve [SideSchemeMessage sid (SideSchemeThwarted (toSource $ player1 tg) 4)]
        getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` False
        envs <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameEnvironments)
        liftIO $ envs `shouldSatisfy` elem back
        victory <- getVictoryDisplay
        liftIO $ map toCardCode victory `shouldSatisfy` notElem front

  it "Team Assembled spends one counter and puts a low-cost ally into play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      ally <- putInDeck you "01083"
      env <- addEnvironment you Environments.ne40190b
      choose "assemble" (isEnvironmentAbility env)
      choose "choose Mockingbird" (isTargetChoice ally)
      pass
      alliesWith "01083" >>= (\xs -> liftIO $ length xs `shouldBe` 1)
      getCountersOn (EnvironmentRef $ EnvironmentId $ unCardId $ toCardId env) `shouldReturn` 0
      offers (isEnvironmentAbility env) `shouldReturn` False

  it "Team Assembled does not offer an ally above its printed cost limit" do
    tg <- testGame ["spider-man"]
    inGame tg do
      expensive <- putInDeck (player1 tg) "01084"
      env <- addEnvironment (player1 tg) Environments.ne40190b
      choose "assemble" (isEnvironmentAbility env)
      offers (isTargetChoice expensive) `shouldReturn` False

  it "Mission Prepped puts a cost-two upgrade into play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      upgrade <- putInDeck (player1 tg) "01065"
      env <- addEnvironment (player1 tg) Environments.ne40193b
      choose "mission prep" (isEnvironmentAbility env)
      choose "choose upgrade" (isTargetChoice upgrade)
      upgradesWith "01065" >>= (\xs -> liftIO $ length xs `shouldBe` 1)

  it "Mission Prepped asks which legal ally receives a searched Sidearm" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      blackCat <- putIntoPlay you "01002"
      _ <- putIntoPlay you "01083"
      sidearm <- putInDeck you "23035"
      env <- addEnvironment you Environments.ne40193b
      choose "mission prep" (isEnvironmentAbility env)
      choose "choose Sidearm" (isTargetChoice sidearm)
      choose "attach to Black Cat" (isTargetChoice $ allyIdOf blackCat)
      upgrades <- upgradesWith "23035"
      liftIO $ map Upgrade.upgradeAttachedAlly upgrades `shouldBe` [Just $ allyIdOf blackCat]

  it "Mission Prepped excludes an attachment with no legal target" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      sidearm <- putInDeck you "23035"
      env <- addEnvironment you Environments.ne40193b
      choose "mission prep" (isEnvironmentAbility env)
      offers (isTargetChoice sidearm) `shouldReturn` False
      upgradesWith "23035" >>= (\cards -> liftIO $ length cards `shouldBe` 0)

  it "Geared Up adds one double-wild Pouches to each player's current deck only once" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      env <- addEnvironment (player1 tg) Environments.ne40192b
      choose "gear up" (isEnvironmentAbility env)
      for_ [player1 tg, player2 tg] \you -> do
        cards <- getCardsOwnedBy you
        liftIO $ length (filter ((== "40196") . cdCardCode) cards) `shouldBe` 1
      offers (isEnvironmentAbility env) `shouldReturn` False
      liftIO $ map snd (cdResources $ lookupPlayerCard "40196") `shouldBe` [Wild,Wild]

  it "Safehouse Established creates a support controlled by the first player" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      env <- addEnvironment (player2 tg) Environments.ne40191b
      choose "establish safehouse" (isEnvironmentAbility env)
      supports <- supportsWith "40197"
      liftIO $ map supportController supports `shouldBe` [player1 tg]

  it "Prepared Defenses applies DEF and retaliate to heroes but not alter-egos" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _env <- addEnvironment (player1 tg) Environments.ne40195b
      mods <- getModifiers (player1 tg)
      liftIO $ DefenseModifier 1 `elem` mods `shouldBe` False
      changeForm (player1 tg)
      pass
      mods' <- getModifiers (player1 tg)
      liftIO do
        mods' `shouldSatisfy` elem (DefenseModifier 1)
        mods' `shouldSatisfy` elem (KeywordModifier $ Retaliate 1)

  it "Safehouse heals the activating identity and observes a separate per-player limit" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      house <- putIntoPlay (player1 tg) "40197"
      setDamage (player1 tg) 3
      choose "Safehouse" (isAbilityOfCard house)
      choose "heal" (isLabel "$label.neSafehouseHeal")
      damageOf (player1 tg) `shouldReturn` 1
      offers (isAbilityOfCard house) `shouldReturn` False
      endTurn
      offers (isAbilityOfCard house) `shouldReturn` True
      before <- length <$> handOf (player2 tg)
      choose "Safehouse for second player" (isAbilityOfCard house)
      choose "draw" (isLabel "$label.neSafehouseDraw")
      (length <$> handOf (player2 tg)) `shouldReturn` (before + 1)

  it "Lady Mastermind deals damage from the highest-cost event in hand" do
    tg <- testGame ["spider-man"]
    inGame tg do
      clearHand (player1 tg)
      _ <- putInHand (player1 tg) "01005"
      _ <- putInHand (player1 tg) "01088"
      resolve . pure . RevealEncounterCard (player1 tg) =<< encounterCard Minions.ne40198
      damageOf (player1 tg) `shouldReturn` 3

  it "Scrambler discards an upgrade controlled by the revealing player" do
    tg <- testGame ["spider-man"]
    inGame tg do
      upgrade <- putIntoPlay (player1 tg) "01065"
      _card <- revealNow (player1 tg) Minions.ne40200
      choose "discard upgrade" (isTargetChoice $ UpgradeRef $ upgradeIdOf upgrade)
      getsGame (HashMap.member (upgradeIdOf upgrade) . gameUpgrades) `shouldReturn` False

  it "Vanisher returns the revealing player's highest-cost support" do
    tg <- testGame ["spider-man"]
    inGame tg do
      house <- putIntoPlay (player1 tg) "40197"
      _card <- revealNow (player1 tg) Minions.ne40201
      choose "return Safehouse" (isTargetChoice $ SupportRef $ supportIdOf house)
      (map toCardCode <$> handOf (player1 tg)) >>= (`shouldSatisfy` elem "40197")

  it "Overburdened lets the player discard a resource instead of taking damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      resource <- putInHand (player1 tg) "01088"
      _card <- revealNow (player1 tg) Treacheries.ne40203
      choose "discard resource" (isTargetChoice resource)
      damageOf (player1 tg) `shouldReturn` 0
      (map toCardId <$> handOf (player1 tg)) >>= (`shouldSatisfy` notElem (toCardId resource))

  it "Overburdened deals two damage if there is no resource in hand" do
    tg <- testGame ["spider-man"]
    inGame tg do
      clearHand (player1 tg)
      _card <- revealNow (player1 tg) Treacheries.ne40203
      choose "take damage" (isLabel "$label.neTakeTwoDamage")
      damageOf (player1 tg) `shouldReturn` 2

  it "Practiced Maneuvers reduces a printed-cost-three event by one" do
    tg <- testGame ["spider-man"]
    inGame tg do
      changeForm (player1 tg)
      pass
      clearHand (player1 tg)
      _env <- addEnvironment (player1 tg) Environments.ne40194b
      kick <- putInHand (player1 tg) "01005"
      energy <- putInHand (player1 tg) "01088"
      playWith kick [energy]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8

  it "Practiced Maneuvers does not reduce X-cost, absent-cost, or low-cost events" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- addEnvironment you Environments.ne40194b
      x <- putInHand you "22010"
      low <- putInHand you "01003"
      let missing = low {pcCardDef = (getCardDef low) {cdCost = Nothing}}
      mods <- getModifiers (toRef you)
      for_ [x,missing,low] $ \card ->
        liftIO $ sum [n | ResourceCostReductionFor matcher n <- mods, cardMatch matcher card] `shouldBe` 0

  it "Safehouse cannot be used in hero form" do
    tg <- testGame ["spider-man"]
    inGame tg do
      house <- putIntoPlay (player1 tg) "40197"
      changeForm (player1 tg)
      pass
      offers (isAbilityOfCard house) `shouldReturn` False
