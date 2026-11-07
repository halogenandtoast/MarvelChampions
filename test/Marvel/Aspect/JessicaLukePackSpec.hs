module Marvel.Aspect.JessicaLukePackSpec (spec) where

import Marvel.Choice qualified as Choice
import Marvel.Event.Events.JessicaLuke qualified as Events
import Marvel.Event.Types qualified as Event
import Marvel.Payment qualified as Payment
import Data.HashMap.Strict qualified as HashMap
import Marvel.Upgrade.Upgrades.JessicaLuke qualified as Upgrade
import Marvel.Villain.Types qualified as Villain
import Marvel.Phase
import Marvel.Ally.Types qualified as Ally
import Marvel.Game.Source qualified as Engine
import Marvel.JessicaJones.Cards qualified as Cards
import Marvel.JessicaJones.Helpers qualified as JL
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Modifier
import Marvel.Trait qualified as T
import Marvel.Status
import Test.Hspec qualified as Hspec
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

pay :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
pay p c codes = traverse (putInHand p) codes >>= playWith c

spec :: Spec
spec = describe "Jessica Jones and Luke Cage aspect cards" do
  it "Misty Knight enters tough and gains two ATK and THW while tough" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      _ <- putIntoPlay (player1 tg) "62013"
      a <- allyIn "62013"
      allyTough a `shouldBe` True
      Ally.getModifiedAttack a `shouldReturn` 3
      Ally.getModifiedThwart a `shouldReturn` 3

  it "Misty Knight loses her basic-power bonuses when tough is discarded" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      card <- putIntoPlay (player1 tg) "62013"
      resolve [RemoveStatus (AllyRef $ allyIdOf card) ToughStatus]
      a <- allyIn "62013"
      Ally.getModifiedAttack a `shouldReturn` 1
      Ally.getModifiedThwart a `shouldReturn` 1

  it "She-Hulk enters tough and offers a tough card for her controller" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "62021"
      pay p card ["01088", "01089"]
      choose "She-Hulk" (isAbilityOfCard card)
      JL.toughCount p `shouldReturn` 1
      a <- allyIn "62021"
      allyTough a `shouldBe` True

  it "Hulk has no thwart power and starts with two ATK" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      _ <- putIntoPlay (player1 tg) "62020"
      a <- allyIn "62020"
      Ally.getModifiedAttack a `shouldReturn` 2
      offers (isAllyThwart a) `shouldReturn` False

  it "Righteous Purpose gives a one-hit-point ally two ATK and THW" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "62019"
      ally <- putIntoPlay p "62002"
      resolve [AllyMessage (allyIdOf ally) $ AllyDamaged (toSource p) $ toDamage 2 FromAbility]
      a <- allyIn "62002"
      Ally.getModifiedAttack a `shouldReturn` 4
      Ally.getModifiedThwart a `shouldReturn` 3

  it "Righteous Purpose gives no bonus above one remaining hit point" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "62019"
      _ <- putIntoPlay p "62002"
      a <- allyIn "62002"
      Ally.getModifiedAttack a `shouldReturn` 2
      Ally.getModifiedThwart a `shouldReturn` 1

  it "Defensive Formation has three uses and grants a Defender ally tough" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      formation <- putIntoPlay p "62018"
      ally <- putIntoPlay p "62002"
      choose "Defensive Formation" (isAbilityOfCard formation)
      choose "Jessica Jones" (isTargetChoice $ AllyCharacter $ allyIdOf ally)
      a <- allyIn "62002"
      allyTough a `shouldBe` True
      support <- testHead <$> supportsWith "62018"
      supportUses support `shouldBe` 2
      supportExhausted support `shouldBe` True

  it "Size Advantage deals six when Luke has more remaining hit points than the enemy" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      vid <- villainIdNow
      resolve [VillainMessage vid $ VillainDamaged (toSource p) $ toDamage 6 FromAbility]
      card <- putInHand p "62034"
      pay p card ["01088"]
      chooseVillain
      villainDamageNow `shouldReturn` 12

  it "Size Advantage deals three when Luke has fewer remaining hit points" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "62034"
      pay p card ["01088"]
      chooseVillain
      villainDamageNow `shouldReturn` 3

  it "Observe makes the villain scheme and grants Luke tough" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      before <- mainSchemeThreatNow
      card <- putInHand p "62035"
      pay p card ["01088"]
      pass
      after <- mainSchemeThreatNow
      after `shouldSatisfy` (> before)
      JL.toughCount p `shouldReturn` 1

  it "Echo reduces enemy attack damage by Jessica's DEF" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "61038"
      a <- allyIn "61038"
      modifiers <- Engine.getModifiers (AllyRef $ allyId a)
      modifiers `shouldSatisfy` elem (EnemyAttackDamageReduction 1)

  it "Echo's reduction increases with Leather Jacket" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "61011"
      _ <- putIntoPlay p "61038"
      a <- allyIn "61038"
      modifiers <- Engine.getModifiers (AllyRef $ allyId a)
      modifiers `shouldSatisfy` elem (EnemyAttackDamageReduction 3)

  it "Captain Marvel discards four energy-icon cards and offers both effects" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      _ <- stackDeck p ["01088", "01089", "01090", "61006"]
      card <- putInHand p "61015"
      pay p card ["01088", "01089"]
      choose "Captain Marvel" (isAbilityOfCard card)
      chooseMainScheme
      chooseVillain
      mainSchemeThreatNow `shouldReturn` (before - 2)

  it "Squirrel Girl spends a squirrel counter to remove one threat" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "61017"
      _ <- putInHand p "01089"
      _ <- putInHand p "01090"
      pay p card ["01088"]
      choose "Squirrel Girl response" (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf card) && abilityIndex ab == 1; _ -> False)
      a <- allyIn "61017"
      allyCounters a `shouldBe` 2
      placeMainThreat 3
      before <- mainSchemeThreatNow
      choose "Squirrel Girl action" (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf card) && abilityIndex ab == 2; _ -> False)
      chooseMainScheme
      mainSchemeThreatNow `shouldReturn` (before - 1)

  it "Dynamic Duo finds a Team-Up and its named ally from deck and discard" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      clearHand p
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) ->
        PlayerIdentity attrs {playerIdentityDeck = Deck [], playerIdentityDiscard = Discard []}
      team <- putInDiscard p "62023"
      discardedAlly <- putInDiscard p "62012"
      cards <- stackDeck p ["62012", "62024", "01090"]
      card <- putInHand p "62022"
      pay p card ["01088"]
      discardBeforeTeam <- discardOf p
      openQuestion >>= \case
        Just (_, QuestionLabel "$label.dynamicDuoTeamUp" _ (Searched places _)) ->
          [(label, map toCardId found) | (label, found) <- places] `shouldBe`
            [(Just "$game.piles.deck", map toCardId cards), (Just "$game.piles.discard", map toCardId discardBeforeTeam)]
        _ -> liftIO $ expectationFailure "expected a Team-Up search grouped by deck and discard"
      offers (isTargetChoice discardedAlly) `shouldReturn` False
      offers (isTargetChoice $ cards !! 2) `shouldReturn` False
      choose "team-up" (isTargetChoice team)
      discardBeforeAlly <- discardOf p
      openQuestion >>= \case
        Just (_, QuestionLabel "$label.dynamicDuoAlly" _ (Searched places _)) ->
          [(label, map toCardId found) | (label, found) <- places] `shouldBe`
            [(Just "$game.piles.deck", map toCardId cards), (Just "$game.piles.discard", map toCardId discardBeforeAlly)]
        _ -> liftIO $ expectationFailure "expected a named ally search grouped by deck and discard"
      offers (isTargetChoice $ cards !! 1) `shouldReturn` False
      offers (isTargetChoice $ cards !! 2) `shouldReturn` False
      offers (isTargetChoice discardedAlly) `shouldReturn` True
      choose "Iron Fist" (isTargetChoice $ testHead cards)
      found <- map toCardCode <$> handOf p
      found `shouldSatisfy` elem "62023"
      found `shouldSatisfy` elem "62012"

  for_ [False, True] $ \hasTeam ->
    it ("Dynamic Duo shows both searched piles without a matching " <> if hasTeam then "ally" else "Team-Up") do
      tg <- testGame ["luke-cage"]
      inGame tg do
        let p = player1 tg
        clearHand p
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
          PlayerIdentity a {playerIdentityDeck = Deck [], playerIdentityDiscard = Discard []}
        team <- putInDiscard p (if hasTeam then "62023" else "01090")
        _ <- stackDeck p ["01089"]
        card <- putInHand p "62022"
        pay p card ["01088"]
        when hasTeam $ choose "Team-Up" (isTargetChoice team)
        deck <- unDeck . playerIdentityDeck <$> playerAttrs p
        discard <- discardOf p
        openQuestion >>= \case
          Just (_, QuestionLabel _ _ (Searched places _)) ->
            [(place, map toCardId cards) | (place, cards) <- places] `shouldBe`
              [(Just "$game.piles.deck", map toCardId deck), (Just "$game.piles.discard", map toCardId discard)]
          _ -> liftIO $ expectationFailure "expected the complete deck and discard"
        choose "finish the unsuccessful search" (isLabel "$label.noMatchingCardsFound")

  it "Power Man and Iron Fist deals two damage and gives a character tough" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "62012"
      card <- putInHand p "62023"
      playWith card []
      chooseVillain
      choose "tough for Luke" (isTargetChoice $ IdentityCharacter p)
      villainDamageNow `shouldReturn` 2
      JL.toughCount p `shouldReturn` 1

  it "Joys of Life can exhaust a Civilian alter ego to ready an ally" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      ally <- putIntoPlay p "61003"
      resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
      card <- putInHand p "61026"
      playWith card []
      choose "Jessica Jones" (isTargetChoice $ IdentityCharacter p)
      choose "ready Luke Cage" (isTargetChoice $ AllyCharacter $ allyIdOf ally)
      isExhaustedPlayer p `shouldReturn` True
      a <- allyIn "61003"
      allyExhausted a `shouldBe` False

  it "Hellcat can enter play for free from a Defender hero's hand" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "61025"
      choose "Hellcat hand action" (\case UseAbility ab -> abilitySource ab == CardIdRef (pcCardId card); _ -> False)
      allies <- alliesWith "61025"
      length allies `shouldBe` 1
      hand <- map toCardCode <$> handOf p
      hand `shouldSatisfy` notElem "61025"

  it "Valkyrie replaces another ally's defeat and heals it to one hit point" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      victim <- putIntoPlay p "62002"
      valkyrie <- putIntoPlay p "62014"
      start [AllyMessage (allyIdOf victim) $ AllyDamaged (toSource p) $ toDamage 3 FromAbility]
      choose "Valkyrie" (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf valkyrie) && abilityIndex ab == 1; _ -> False)
      a <- allyIn "62002"
      allyDamage a `shouldBe` 2
      alliesWith "62014" `shouldReturn` []

  it "Black Belt grants an attached ally one hit point and Martial Artist" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- putIntoPlay p "62012"
      recipient <- putIntoPlay p "62002"
      belt <- putInHand p "62037"
      playOn belt (allyIdOf recipient) []
      a <- allyIn "62002"
      Ally.getModifiedHitPoints a `shouldReturn` 4
      traits <- Engine.getCharacterTraits (AllyCharacter $ allyIdOf recipient)
      traits `shouldSatisfy` member T.MartialArtist

  it "Strategy Session searches for a player side scheme while in alter ego" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      schemes <- stackDeck p ["61028", "01088"]
      card <- putInHand p "61019"
      pay p card ["01088"]
      choose "Jessica searches" (isTargetChoice p)
      choose "Second Chance" (isTargetChoice $ testHead schemes)
      hand <- map toCardCode <$> handOf p
      hand `shouldSatisfy` elem "61028"

  it "Strategy Session's alter-ego search is not a thwart and leaves confuse in place" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      resolve [GiveStatus (IdentityRef p) ConfusedStatus]
      _ <- stackDeck p ["61028", "01088"]
      card <- putInHand p "61019"
      pay p card ["01088"]
      choose "Jessica searches" (isTargetChoice p)
      pass
      isConfused p `shouldReturn` True

  it "Echo actually reduces a two-damage enemy attack to one damage" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asHero p
      echo <- putIntoPlay p "61038"
      _ <- setEncounterDeck [Cards.jl62029, Cards.jl62029] []
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacksAlly p (allyIdOf echo)]
      choose "no defender" isNoDefender
      pass
      a <- allyIn "61038"
      allyDamage a `shouldBe` 1

  it "Echo's enemy-attack reduction does not reduce friendly attack damage" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asHero p
      echo <- putIntoPlay p "61038"
      messages <- Choice.choiceMessages p (DamageCharacter (AllyCharacter $ allyIdOf echo) (IdentityRef p) (toDamage 2 $ FromPlayerAttack p))
      resolve messages
      a <- allyIn "61038"
      allyDamage a `shouldBe` 2

  it "Spider-Woman gains a limb counter after a controlled character thwarts" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asHero p
      spider <- putIntoPlay p "61016"
      placeMainThreat 4
      choose "basic thwart" (isHeroThwart p)
      chooseMainScheme
      choose "Spider-Woman" (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf spider) && abilityIndex ab == 1; _ -> False)
      choose "keep Spider-Woman" (isLabel "$label.done")
      pass
      a <- allyIn "61016"
      allyCounters a `shouldBe` 1

  for_ ["61027", "62024", "99999"] $ \code ->
    it ("Unbreakable Bond " <> show code <> " removes threat and heals three damage distributed between its named characters") do
      tg <- testGame ["jessica-jones"]
      inGame tg do
        let p = player1 tg
        asHero p
        luke <- putIntoPlay p "61003"
        setDamage p 2
        resolve [RemoveStatus (AllyRef $ allyIdOf luke) ToughStatus, AllyMessage (allyIdOf luke) $ AllyDamaged (toSource p) $ toDamage 1 FromAbility]
        placeMainThreat 4
        before <- mainSchemeThreatNow
        if code == "99999" then do
          -- A future printing keeps its title but has a code unknown to today's registry.
          eid <- getRandom
          let def = Cards.jl61027 {cdCardCode = code}
              event = buildCard (reprintAs def Events.jl61027) (p, eid)
          withGame_ $ entitiesL . eventsL . at eid ?~ Event.Event event
          start [EventMessage eid (PlayedEvent p Payment.NoPayment Nothing)]
        else do
          card <- putInHand p code
          pay p card ["01088"]
        chooseMainScheme
        choose "heal Jessica" (isTargetChoice $ IdentityCharacter p)
        choose "heal Jessica again" (isTargetChoice $ IdentityCharacter p)
        choose "heal Luke" (isTargetChoice $ AllyCharacter $ allyIdOf luke)
        damageOf p `shouldReturn` 0
        a <- allyIn "61003"
        allyDamage a `shouldBe` 0
        mainSchemeThreatNow `shouldReturn` (before - 3)

  it "Shakedown removes the exactly-defeated minion's base SCH as threat" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asHero p
      shake <- putIntoPlay p "61035"
      mid <- addMinion p Minions.hydraBomber
      placeMainThreat 4
      before <- mainSchemeThreatNow
      choose "basic attack" (isHeroAttack p)
      choose "Hydra Bomber" (isTargetChoice $ EnemyMinionId mid)
      choose "Shakedown" (isAbilityOfCard shake)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` (before - 1)
      upgradeIsExhausted (upgradeOf shake) `shouldReturn` True

  it "Mitigated Threat removes a side scheme's hazard icon" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      scheme <- revealNow p SideSchemes.breakinAndTakin
      pass
      before <- Engine.getHazardCount
      card <- putInHand p "61040"
      playOn card (sideSchemeOf scheme) []
      Engine.getHazardCount `shouldReturn` (before - 1)

  it "Cruisin' for a Bruisin' adds two attack damage and grants overkill" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.hydraBomber
      card <- putInHand p "62009"
      playOn card mid []
      pass
      choose "basic attack" (isHeroAttack p)
      choose "upgraded minion" (isTargetChoice $ EnemyMinionId mid)
      villainDamageNow `shouldReturn` 2

  it "Take a Stand boosts every controlled ally after Luke defends" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "62002"
      card <- putInHand p "62015"
      resource <- putInHand p "01088"
      _ <- setEncounterDeck [Cards.jl62029, Cards.jl62029] []
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacks p]
      choose "Luke defends" isDefend
      playWith card [resource]
      a <- allyIn "62002"
      Ally.getModifiedAttack a `shouldReturn` 3
      Ally.getModifiedThwart a `shouldReturn` 2

  it "Toughen Up grants tough after recovery reaches base hit points" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      clearHand p
      toughen <- putIntoPlay p "62036"
      setDamage p 3
      choose "recover" (\case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False)
      choose "Toughen Up" (isAbilityOfCard toughen)
      JL.toughCount p `shouldReturn` 1
      damageOf p `shouldReturn` 0

  it "Shakedown uses Purple Man's defined base SCH rather than his printed star" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asHero p
      shake <- putIntoPlay p "61035"
      mid <- addMinion p Cards.jl61031
      resolve [MinionMessage mid $ MinionDamaged (toSource p) $ toDamage 4 FromAbility]
      placeMainThreat 5
      before <- mainSchemeThreatNow
      choose "basic attack" (isHeroAttack p)
      choose "Purple Man" (isTargetChoice $ EnemyMinionId mid)
      choose "Shakedown" (isAbilityOfCard shake)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` (before - 3)

  it "Joys of Life is unavailable without a readyable destination" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "61026"
      offers (isPlay card) `shouldReturn` False

  it "Joys of Life is unavailable without a Civilian alter ego" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      ally <- putIntoPlay p "62012"
      resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
      card <- putInHand p "61026"
      offers (isPlay card) `shouldReturn` False

  it "Joys of Life asks another player to approve paying its exhaust cost" do
    tg <- testGame ["jessica-jones", "luke-cage"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      asHero p
      clearHand other
      resolve [IdentityMessage p ExhaustedIdentity]
      card <- putInHand p "61026"
      playWith card []
      fmap fst <$> asked `shouldReturn` Just other
      isExhaustedPlayer other `shouldReturn` False
      choose "approve the Civilian exhaustion" (isLabel "$label.contributeToCost")
      choose "Luke Cage pays the exhaustion" (isTargetChoice $ IdentityCharacter other)
      choose "ready Jessica Jones" (isTargetChoice $ IdentityCharacter p)
      isExhaustedPlayer other `shouldReturn` True
      isExhaustedPlayer p `shouldReturn` False

  it "declining a Joys of Life contribution preserves both the card and characters" do
    tg <- testGame ["jessica-jones", "luke-cage"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      asHero p
      clearHand other
      resolve [IdentityMessage p ExhaustedIdentity]
      card <- putInHand p "61026"
      playWith card []
      choose "decline the Civilian exhaustion" (isLabel "$label.declineContribution")
      isExhaustedPlayer other `shouldReturn` False
      isExhaustedPlayer p `shouldReturn` True
      handOf p >>= (`shouldSatisfy` elem card)

  it "deck validation rejects duplicate unique Jessica and Luke pack allies" do
    let justiceDeck = (starter "jessica-jones") {cards = [("61015", 2)]}
        leadershipDeck = (starter "luke-cage") {cards = [("62013", 2)]}
    deckBuildingError justiceDeck `Hspec.shouldBe` Just "error.deckCopyLimit"
    deckBuildingError leadershipDeck `Hspec.shouldBe` Just "error.deckCopyLimit"

  it "Joys of Life only offers exhaust sources for a mode with a readyable destination" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- putIntoPlay p "61003"
      exhausted <- putIntoPlay p "62012"
      resolve [AllyMessage (allyIdOf exhausted) ExhaustedAlly]
      card <- putInHand p "61026"
      playWith card []
      -- The sole legal payer/source is selected automatically. A broad raw
      -- matcher would instead offer a choice between Jessica and the ready ally.
      isExhaustedPlayer p `shouldReturn` True
      choose "ready Iron Fist" (isTargetChoice $ AllyCharacter $ allyIdOf exhausted)
      isExhaustedPlayer p `shouldReturn` True
      allyExhausted <$> allyIn "62012" `shouldReturn` False

  it "JL upgrades load the original attrs-object save format and retain pending Stakeout state" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      card <- putIntoPlay p "61014"
      entity <- getsGame (fromMaybe (error "missing Stakeout") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
      let attrs = toAttrs entity
          legacy = toJSON attrs
          pending = Upgrade.JLUpgrade attrs (Just $ toSource p)
      fromJSON legacy `shouldBe` Success (Upgrade.JLUpgrade attrs Nothing)
      fromJSON (toJSON pending) `shouldBe` Success pending

  it "Unbreakable Bond requires both Jessica Jones and Luke Cage in play" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "61027"
      _ <- putInHand p "01088"
      offers (isPlay card) `shouldReturn` False

  it "the Luke printing of Unbreakable Bond does not count an alter-ego Jessica" do
    tg <- testGame ["luke-cage", "jessica-jones"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "62024"
      _ <- putInHand p "01088"
      offers (isPlay card) `shouldReturn` False

  it "Power Man and Iron Fist requires Iron Fist in play" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "62023"
      offers (isPlay card) `shouldReturn` False

  it "Take a Stand played in the villain phase lasts until the next player phase ends" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "62002"
      card <- putInHand p "62015"
      resource <- putInHand p "01088"
      _ <- setEncounterDeck [Cards.jl62029, Cards.jl62029] []
      withGame_ $ phaseL .~ VillainPhase
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacks p]
      choose "Luke defends" isDefend
      playWith card [resource]
      pass
      initial <- allyIn "62002"
      Ally.getModifiedAttack initial `shouldReturn` 3
      resolve [EndPhase VillainPhase, BeginPhase PlayerPhase]
      ally <- allyIn "62002"
      Ally.getModifiedAttack ally `shouldReturn` 3
      Ally.getModifiedThwart ally `shouldReturn` 2
      clearHand p
      resolve [EndPhase PlayerPhase]
      final <- allyIn "62002"
      Ally.getModifiedAttack final `shouldReturn` 2
      Ally.getModifiedThwart final `shouldReturn` 1

  it "Power Man and Iron Fist may give its tough status card to an enemy" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "62012"
      card <- putInHand p "62023"
      playWith card []
      chooseVillain
      vid <- villainIdNow
      choose "give the villain tough" (isTargetChoice $ VillainCharacter vid)
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True
      JL.toughCount p `shouldReturn` 0

  it "Power Man and Iron Fist chooses a tough recipient after its damage resolves" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "62012"
      mid <- addMinion p Minions.hydraBomber
      card <- putInHand p "62023"
      playWith card []
      choose "defeat Hydra Bomber" (isTargetChoice $ EnemyMinionId mid)
      offered <- offers (isTargetChoice $ MinionCharacter mid)
      offered `shouldBe` False
      choose "give Luke tough" (isTargetChoice $ IdentityCharacter p)
      JL.toughCount p `shouldReturn` 1
