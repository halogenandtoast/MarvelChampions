module Marvel.Encounter.MojoSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (nub)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.EncounterSet qualified as Set
import Marvel.Environment.Cards qualified as Environments
import Marvel.Matchers
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Phase
import Marvel.Query
import Marvel.Scenario.Types (Attrs (scenarioSetAsideCards))
import Marvel.Scenario.Mojo (mojoGenres)
import Marvel.SideScheme.Cards qualified as Sides
import Marvel.SideScheme.Types qualified as Side
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Window qualified as W
import TestImport

mojo :: IO TestGame
mojo = do
  let setup = ScenarioSetup "39022" [Set.Fantasy, Set.Sitcom] Normal
  (app, ran, logged) <- dealtGameLogged setup [starter "spider-man"] 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

settle :: GameAppT ()
settle = go (100 :: Int)
 where
  go 0 = error "Mojo card fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, _) -> answerQuestion you 0 >> go (n - 1)
    Nothing -> pure ()

asideGenres :: GameAppT [Set.EncounterSet]
asideGenres = getsGame $ filter (`elem` mojoGenres) . nub . catMaybes . map (cdEncounterSet . getCardDef) . scenarioSetAsideCards . toAttrs . gameScenario

spec :: Spec
spec = describe "Mojo standalone" do
  it "starts with Mojo I, 10 threat per player, the spinning wheel and one remaining genre" do
    tg <- mojo
    inGame tg do
      toCardCode . getCardDef <$> getVillain `shouldReturn` "39022"
      mainSchemeThreatNow `shouldReturn` 10
      selectAny (EnvironmentIs Environments.mojo39026a) `shouldReturn` True
      length <$> asideGenres `shouldReturn` 1
      environments <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameEnvironments)
      length environments `shouldBe` 2

  it "sets aside whole genres without putting their cards into the initial deck" do
    tg <- mojo
    inGame tg do
      sets <- asideGenres
      deck <- scenarioEncounterDeckNow
      liftIO $ all (\c -> cdEncounterSet (getCardDef c) `notElem` map Just sets) deck `shouldBe` True

  it "turn-end rating pressure counts cards outside the Mojo set" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      void $ setEncounterDeck [Treacheries.advance, Treacheries.mojo39032, Treacheries.assault, Treacheries.mojo39033] []
      vid <- villainIdNow
      resolve [RanAbility p (VillainRef vid) 1 [W.TurnEnded p] NoPayment]
      getCharacterThreat (IdentityCharacter p) `shouldReturn` 2
      map toCardCode <$> encounterDiscard >>= (`shouldMatchList` ["01186", "39032", "01187"])

  it "transfers a hero's threat to the main scheme when the hero changes form" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      resolve [PlaceCharacterThreat (IdentityCharacter p) 3]
      before <- mainSchemeThreatNow
      changeForm p
      getCharacterThreat (IdentityCharacter p) `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` before + 3

  it "Supporting Actor stores two threat after its activation" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Minions.mojo39029
      resolve [RanAbility p (MinionRef mid) 1 [] NoPayment]
      getCharacterThreat (MinionCharacter mid) `shouldReturn` 2

  it "Supporting Actor transfers its threat when defeated" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Minions.mojo39029
      resolve [PlaceCharacterThreat (MinionCharacter mid) 2]
      before <- mainSchemeThreatNow
      resolve [MinionMessage mid (MinionDamaged (toSource p) $ toDamage 6 FromAbility)]
      mainSchemeThreatNow `shouldReturn` before + 2

  it "Top Billing heals controlled characters and places two threat on each" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      setDamage p 3
      _ <- revealNow p Treacheries.mojo39034
      settle
      damageOf p `shouldReturn` 2
      getCharacterThreat (IdentityCharacter p) `shouldReturn` 2

  it "Curtain Call moves the largest character threat total to the main scheme" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Minions.mojo39029
      resolve [PlaceCharacterThreat (IdentityCharacter p) 1, PlaceCharacterThreat (MinionCharacter mid) 3]
      before <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.mojo39032
      settle
      mainSchemeThreatNow `shouldReturn` before + 3
      getCharacterThreat (MinionCharacter mid) `shouldReturn` 0
      getCharacterThreat (IdentityCharacter p) `shouldReturn` 1

  it "Curtain Call adds threat when no character has any to move" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Treacheries.mojo39032
      settle
      getCharacterThreat (IdentityCharacter p) `shouldReturn` 1

  it "Stinger Tail absorbs all damage and discards itself at five" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      card <- revealNow p Attachments.mojo39028
      settle
      vid <- villainIdNow
      before <- villainDamageNow
      resolve =<< Lifted.runQueueT (Lifted.capture $ Lifted.dealDamage vid p $ toDamage 5 FromAbility)
      villainDamageNow `shouldReturn` before
      let aid = AttachmentId $ unCardId $ ecCardId card
      getsGame (HashMap.member aid . gameAttachments) `shouldReturn` False

  it "Undercover Mojo removes threat instead of damaging Mojo" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      card <- revealNow p Sides.mojo39031
      settle
      vid <- villainIdNow
      before <- villainDamageNow
      resolve =<< Lifted.runQueueT (Lifted.capture $ Lifted.dealDamage vid p $ toDamage 2 FromAbility)
      villainDamageNow `shouldReturn` before
      let sid = sideSchemeOf card
      getsGame (fmap (Side.sideSchemeThreat . toAttrs) . HashMap.lookup sid . gameSideSchemes) `shouldReturn` Just 2

  it "spins to stopped when the encounter deck resets, then recruits the last genre" do
    tg <- mojo
    inGame tg do
      void $ setEncounterDeck [Treacheries.advance] [Treacheries.assault]
      resolve [DiscardTopOfEncounterDeck 1 Nothing]
      selectAny (EnvironmentIs Environments.mojo39026b) `shouldReturn` True
      resolve [VillainPhaseStep StepDealEncounterCards]
      asideGenres `shouldReturn` []
      selectAny (EnvironmentIs Environments.mojo39026a) `shouldReturn` True

  it "advances from Mojo I to Mojo II and places threat on friendly characters" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      toCardCode . getCardDef <$> getVillain `shouldReturn` "39023"
      getCharacterThreat (IdentityCharacter p) `shouldReturn` 2

  it "Major Domo adds attack and scheme and mills damage only after Mojo's attack" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Attachments.mojo39027
      settle
      clearHand p
      void $ setEncounterDeck [Obligations.mojo39062, Treacheries.advance, Treacheries.assault, Treacheries.mojo39033, Treacheries.mojo39034] []
      vid <- villainIdNow
      before <- damageOf p
      resolve [VillainMessage vid (VillainAttacks p)]
      -- This seed starts with Fantasy's amplify icon, so the zero-icon boost
      -- contributes one: Mojo's 1 ATK + Major Domo's 1 ATK + 1 boost.
      damageOf p `shouldReturn` before + 3
      length <$> scenarioEncounterDeckNow `shouldReturn` 1

  it "Paparazzi stays in play and moves its remaining ten threat at turn end" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      card <- revealNow p Obligations.mojo39030
      settle
      before <- mainSchemeThreatNow
      let oid = ObligationId $ unCardId $ ecCardId card
      getsGame (HashMap.member oid . gameObligations) `shouldReturn` True
      resolve [CheckWindows [W.Window W.When $ W.TurnEnded p]]
      mainSchemeThreatNow `shouldReturn` before + 10
      getsGame (HashMap.member oid . gameObligations) `shouldReturn` False

  it "Director's Directions can make Mojo scheme" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      clearHand p
      void $ setEncounterDeck [Obligations.mojo39062, Treacheries.assault] []
      before <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.mojo39033
      pressWhenRevealed
      choose "let Mojo scheme" (isLabel "$label.villainSchemes")
      settle
      -- Fantasy's amplify icon adds one to the zero-icon Growing Pains boost.
      mainSchemeThreatNow `shouldReturn` before + 3

  it "loses when the encounter deck resets after all genres have been used" do
    tg <- mojo
    inGame tg do
      resolve [CardSpecific GameRef "mojoRecruitGenre" (toJSON False)]
      asideGenres `shouldReturn` []
      void $ setEncounterDeck [Treacheries.advance] [Treacheries.assault]
      resolve [DiscardTopOfEncounterDeck 1 Nothing]
      getsGame gameState `shouldReturn` Finished Lost

  it "expert mode starts at Mojo II and advances to III with three threat per friendly character" do
    let setup = ScenarioSetup "39022" [Set.Fantasy, Set.Sitcom] Expert
    (app, ran, logged) <- dealtGameLogged setup [starter "spider-man"] 1
    seats <- runGameApp app (getsGame gameSeats)
    let tg = TestGame app ran logged seats
    inGame tg do
      toFirstTurn
      let p = player1 tg
      toCardCode . getCardDef <$> getVillain `shouldReturn` "39023"
      before <- getCharacterThreat (IdentityCharacter p)
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      toCardCode . getCardDef <$> getVillain `shouldReturn` "39024"
      getCharacterThreat (IdentityCharacter p) `shouldReturn` before + 3

  it "Paparazzi's alter-ego discard action removes three threat before turn end" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      card <- revealNow p Obligations.mojo39030
      settle
      let oid = ObligationId $ unCardId $ ecCardId card
      clearHand p
      _ <- putInHand p "01088"
      choose "discard a card to remove Paparazzi threat" (\case UseAbility ab -> abilitySource ab == ObligationRef oid && abilityIndex ab == 2; _ -> False)
      settle
      length <$> handOf p `shouldReturn` 0
      before <- mainSchemeThreatNow
      resolve [CheckWindows [W.Window W.When $ W.TurnEnded p]]
      mainSchemeThreatNow `shouldReturn` before + 7

  it "Paparazzi remains in play at zero threat until the turn actually ends" do
    tg <- mojo
    inGame tg do
      let p = player1 tg
      card <- revealNow p Obligations.mojo39030
      settle
      let oid = ObligationId $ unCardId $ ecCardId card
      clearHand p
      for_ [1 :: Int .. 4] $ \_ -> do
        _ <- putInHand p "01088"
        choose "remove Paparazzi threat" (\case UseAbility ab -> abilitySource ab == ObligationRef oid && abilityIndex ab == 2; _ -> False)
        settle
      getsGame (HashMap.member oid . gameObligations) `shouldReturn` True
      offers (isAbilityOf $ ObligationRef oid) `shouldReturn` False
      before <- mainSchemeThreatNow
      resolve [CheckWindows [W.Window W.When $ W.TurnEnded p]]
      mainSchemeThreatNow `shouldReturn` before
      getsGame (HashMap.member oid . gameObligations) `shouldReturn` False

  it "Director's Directions finishes without surging after its damage eliminates its player" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      clearHand p
      setDamage p 9
      resolve [PlaceCharacterThreat (IdentityCharacter p) 1]
      before <- length <$> scenarioEncounterDeckNow
      _ <- revealNow p Treacheries.mojo39033
      pressWhenRevealed
      choose "take Director's Directions damage" (isLabel "$label.mojoTakeThreatDamage")
      settle
      getsGame (HashMap.member p . gamePlayers) `shouldReturn` False
      getsGame (HashMap.member other . gamePlayers) `shouldReturn` True
      getsGame gameState `shouldReturn` InProgress
      length <$> scenarioEncounterDeckNow `shouldReturn` before
