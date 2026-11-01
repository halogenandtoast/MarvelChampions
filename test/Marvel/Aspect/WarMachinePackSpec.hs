module Marvel.Aspect.WarMachinePackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Hp (HP (..))
import Marvel.Identity (getModifiedCost)
import Marvel.MainScheme.Types (Attrs (mainSchemeThreat))
import Marvel.Matchers.Types (EntityMatcher (AllyEntity), AllyMatcher (AllyWithId))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types (overMinion)
import Marvel.Modifier
import Marvel.Phase (Phase (..))
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Villain.Types (villainStunned)
import TestImport hiding (mainSchemeThreat)

warMachine :: IO TestGame
warMachine = testGame ["war-machine"]

ownAbility :: (IsRef a) => a -> Natural -> Choice -> Bool
ownAbility target index = \case
  UseAbility ab -> abilitySource ab == toRef target && abilityIndex ab == index
  _ -> False

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> choose "load ammo" (ownAbility p 1) >> pass >> clearHand p

payCodes :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
payCodes p card codes = traverse (putInHand p) codes >>= playWith card

mainThreat :: GameAppT Natural
mainThreat = getsGame (sum . map (mainSchemeThreat . toAttrs) . HashMap.elems . gameMainSchemes)

allyExhaustedNow :: AllyId -> GameAppT Bool
allyExhaustedNow aid = getsGame (maybe False (allyExhausted . toAttrs) . HashMap.lookup aid . gameAllies)

supportUsesNow :: PlayerCard -> GameAppT Natural
supportUsesNow card = getsGame (maybe 0 (supportUses . toAttrs) . HashMap.lookup (supportIdOf card) . gameSupports)

newtype FixtureModifiers = FixtureModifiers (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage FixtureModifiers where
  runMessage msg (FixtureModifiers a) = FixtureModifiers <$> runMessage msg a

modifyAlly :: AllyId -> [Modifier] -> GameAppT ()
modifyAlly aid modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (FixtureModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = AllyEntity (AllyWithId aid), effectModifiers = modifiers, effectEnds = Nothing })

playPanther :: IdentityId -> GameAppT PlayerCard
playPanther p = do
  card <- putInHand p "23012"
  payCodes p card ["01088", "01089"]
  pure card

playCaptain :: IdentityId -> [CardCode] -> GameAppT (PlayerCard, [PlayerCard])
playCaptain p top = do
  discarded <- stackDeck p top
  card <- putInHand p "23013"
  payCodes p card ["01088", "01089", "01090"]
  pure (card, discarded)

discountedVision :: IdentityId -> GameAppT PlayerCard
discountedVision p = do
  carrier <- putIntoPlay p "01092"
  choose "use Helicarrier" (isAbilityOfCard carrier)
  choose "reduce War Machine's next card" (isTargetChoice p)
  ally <- putInHand p "01068"
  player <- getPlayer p
  getModifiedCost player ally Nothing `shouldReturn` 3
  payCodes p ally ["01088", "23008"]
  pass
  pure ally

spec :: Spec
spec = describe "War Machine pack" do
  describe "Black Panther (23012)" do
    it "attaches only a leadership event from its controller's discard pile" do
      tg <- testGame ["war-machine", "spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        [event, wrongAspect, support] <- stackDiscard p ["23019", "01060", "23016"]
        [otherPlayer] <- stackDiscard (player2 tg) ["01073"]
        panther <- playPanther p
        choose "attach an event" (ownAbility (allyIdOf panther) 1)
        offers (isTargetChoice wrongAspect) `shouldReturn` False
        offers (isTargetChoice support) `shouldReturn` False
        offers (isTargetChoice otherPlayer) `shouldReturn` False
        choose "attach Go Down Swinging" (isTargetChoice event)
        pass
        getAllyHeldCards p `shouldReturn` [event]
        discardOf p >>= (`shouldSatisfy` notElem event)

    it "may decline the attachment response" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        [event] <- stackDiscard p ["23019"]
        _ <- playPanther p
        pass
        getAllyHeldCards p `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem event)

    it "still applies normal form restrictions to an attached event" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        [event] <- stackDiscard p ["23019"]
        panther <- playPanther p
        choose "attach" (ownAbility (allyIdOf panther) 1)
        choose "choose event" (isTargetChoice event)
        pass
        offers (isPlay event) `shouldReturn` False
        changeForm p
        pass
        offers (isPlay event) `shouldReturn` True

    it "can play its attached event while sacrificing itself without duplicating the event" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        [event] <- stackDiscard p ["23019"]
        panther <- playPanther p
        choose "attach" (ownAbility (allyIdOf panther) 1)
        choose "choose Go Down Swinging" (isTargetChoice event)
        pass
        playWith event []
        chooseVillain
        pass
        alliesWith "23012" `shouldReturn` []
        villainDamageNow `shouldReturn` 4
        discard <- discardOf p
        length (filter ((== pcCardId event) . pcCardId) discard) `shouldBe` 1
        discard `shouldSatisfy` elem panther

    it "suppresses held-event permission while blanked without losing the event" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        [event] <- stackDiscard p ["23019"]
        panther <- playPanther p
        choose "attach" (ownAbility (allyIdOf panther) 1)
        choose "event" (isTargetChoice event)
        pass
        modifyAlly (allyIdOf panther) [Blank]
        offers (isPlay event) `shouldReturn` False
        attached <- allyHeldCards <$> allyIn "23012"
        attached `shouldBe` [event]

  describe "Captain Marvel (23013)" do
    it "discards exactly four cards, deals three for one energy icon, and does not stun" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        (captain, top) <- playCaptain p ["23008", "01089", "01090", "01092"]
        choose "Captain Marvel's response" (ownAbility (allyIdOf captain) 1)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        villainStunned . toAttrs <$> getVillain `shouldReturn` False
        discard <- discardOf p
        for_ top (\c -> discard `shouldSatisfy` elem c)

    it "counts both icons on Energy and stuns the same enemy it damaged" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        enemy <- addMinion p Minions.abomination
        (captain, _) <- playCaptain p ["01088", "01089", "01090", "01092"]
        choose "discard four" (ownAbility (allyIdOf captain) 1)
        choose "target Abomination" (isTargetChoice enemy)
        pass
        minionDamageOf enemy `shouldReturn` Just 3
        getsGame (maybe False (minionStunned . toAttrs) . HashMap.lookup enemy . gameMinions) `shouldReturn` True
        villainDamageNow `shouldReturn` 0
        villainStunned . toAttrs <$> getVillain `shouldReturn` False

    it "does not treat a printed wild as energy, but still discards the four cards" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        (captain, top) <- playCaptain p ["21058", "01089", "01090", "01092"]
        choose "discard four" (ownAbility (allyIdOf captain) 1)
        pass
        villainDamageNow `shouldReturn` 0
        villainStunned . toAttrs <$> getVillain `shouldReturn` False
        discard <- discardOf p
        for_ top (\c -> discard `shouldSatisfy` elem c)

    it "its response is not an attack: guard, retaliate and the hero's statuses do not apply" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- addMinion p Minions.hydraMercenary
        enemy <- addMinion p Minions.whiplash
        withGame_ $ entitiesL . minionsL . ix enemy %~ overMinion (\a -> a {minionHitPoints = HP 12})
        resolve [IdentityMessage p IdentityStunned, IdentityMessage p IdentityConfused]
        (captain, _) <- playCaptain p ["23008", "01089", "01090", "01092"]
        choose "discard four" (ownAbility (allyIdOf captain) 1)
        vid <- villainIdNow
        offers (isTargetChoice vid) `shouldReturn` True
        choose "damage Whiplash" (isTargetChoice enemy)
        pass
        minionDamageOf enemy `shouldReturn` Just 3
        damageOf p `shouldReturn` 0
        isStunned p `shouldReturn` True
        isConfused p `shouldReturn` True

    it "can stun a damage-immune enemy when two energy icons were discarded" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        robot <- addMinion p Minions.edisonsGiantRobot
        (captain, _) <- playCaptain p ["01088", "01089", "01090", "01092"]
        choose "discard four" (ownAbility (allyIdOf captain) 1)
        choose "stun the robot" (isTargetChoice robot)
        pass
        minionDamageOf robot `shouldReturn` Just 0
        getsGame (maybe False (minionStunned . toAttrs) . HashMap.lookup robot . gameMinions) `shouldReturn` True

    it "has ATK 3 with two consequential damage and THW 2 with one consequential damage" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        (captain, _) <- playCaptain p ["01088", "01089", "01090", "01092"]
        pass
        ally <- allyIn "23013"
        choose "Captain Marvel attacks" (isAllyAttack ally)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        allyDamage <$> allyIn "23013" `shouldReturn` 2
        resolve [AllyMessage (allyIdOf captain) ReadiedAlly]
        placeMainThreat 3
        choose "Captain Marvel thwarts" (isAllyThwart ally)
        chooseMainScheme
        pass
        mainThreat `shouldReturn` 1
        allyDamage <$> allyIn "23013" `shouldReturn` 3

  describe "Command Team (23016)" do
    it "enters with three uses and exhausts/spends one to ready another player's ally" do
      tg <- testGame ["war-machine", "spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        team <- putInHand p "23016"
        payCodes p team ["01088"]
        ally <- putIntoPlay (player2 tg) "01068"
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        choose "use Command Team" (ownAbility (supportIdOf team) 1)
        choose "ready the other player's ally" (isTargetChoice $ allyIdOf ally)
        allyExhaustedNow (allyIdOf ally) `shouldReturn` False
        supportUsesNow team `shouldReturn` 2
        getsGame (maybe False (supportExhausted . toAttrs) . HashMap.lookup (supportIdOf team) . gameSupports) `shouldReturn` True

    it "the last counter discards Command Team but still readies its target" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        team <- putIntoPlay p "23016"
        ally <- putIntoPlay p "01068"
        for_ [1 :: Int .. 3] \_ -> do
          resolve [AllyMessage (allyIdOf ally) ExhaustedAlly, SupportMessage (supportIdOf team) ReadiedSupport]
          choose "spend a command counter" (ownAbility (supportIdOf team) 1)
          choose "ready Vision" (isTargetChoice $ allyIdOf ally)
        allyExhaustedNow (allyIdOf ally) `shouldReturn` False
        getsGame (HashMap.member (supportIdOf team) . gameSupports) `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` elem team)

    it "has no action without an exhausted ally that can legally be readied" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        team <- putIntoPlay p "23016"
        ally <- putIntoPlay p "01068"
        offers (ownAbility (supportIdOf team) 1) `shouldReturn` False
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        modifyAlly (allyIdOf ally) [CannotReady]
        offers (ownAbility (supportIdOf team) 1) `shouldReturn` False
        supportUsesNow team `shouldReturn` 3

    it "does not spend a counter on an ally that cannot be readied by player cards" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        team <- putIntoPlay p "23016"
        ally <- putIntoPlay p "01068"
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        modifyAlly (allyIdOf ally) [CannotBeReadiedByPlayerCards]
        offers (ownAbility (supportIdOf team) 1) `shouldReturn` False
        supportUsesNow team `shouldReturn` 3

  describe "Sneak Attack (23017)" do
    it "chooses a matching ally as a cost, puts it into play, then discards it at phase end" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        sneak <- putInHand p "23017"
        ally <- putInHand p "23013"
        energy <- putInHand p "01088"
        choose "play Sneak Attack" (isPlay sneak)
        choose "choose Captain Marvel" (isTargetChoice ally)
        choose "pay one resource" (isPayWith energy)
        pass
        alliesWith "23013" >>= (`shouldSatisfy` (not . null))
        runNow [EndPhase PlayerPhase]
        alliesWith "23013" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem ally)

    it "uses traits of the current alter ego rather than its hero face" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        sneak <- putInHand p "23017"
        avenger <- putInHand p "23002"
        shield <- putInHand p "01083"
        _ <- putInHand p "01088"
        choose "play in alter ego" (isPlay sneak)
        offers (isTargetChoice avenger) `shouldReturn` False
        offers (isTargetChoice shield) `shouldReturn` True

    it "includes traits the identity gains in play" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        sneak <- putInHand p "23017"
        _ <- putInHand p "17011"
        _ <- putInHand p "01088"
        offers (isPlay sneak) `shouldReturn` False
        _ <- putIntoPlay p "23004"
        offers (isPlay sneak) `shouldReturn` True

    it "does not count or offer the selected ally as a resource for Sneak Attack" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        sneak <- putInHand p "23017"
        _ <- putInHand p "23012"
        offers (isPlay sneak) `shouldReturn` False

    it "does not offer an ally whose unique title is already in play" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "23012"
        clearHand p
        sneak <- putInHand p "23017"
        _ <- putInHand p "23012"
        _ <- putInHand p "01088"
        offers (isPlay sneak) `shouldReturn` False

    it "puts the ally into play without triggering its played-from-hand response" do
      tg <- testGame ["ms-marvel"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        sneak <- putInHand p "23017"
        ironheart <- putInHand p "13018"
        energy <- putInHand p "01088"
        choose "play Sneak Attack" (isPlay sneak)
        choose "choose Ironheart" (isTargetChoice ironheart)
        choose "pay" (isPayWith energy)
        offers (ownAbility (allyIdOf ironheart) 1) `shouldReturn` False
        pass
        alliesWith "13018" >>= (`shouldSatisfy` (not . null))
        length <$> handOf p `shouldReturn` 0

    it "does not discard a new instance of the same physical ally after it leaves and is replayed" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        clearHand p
        sneak <- putInHand p "23017"
        ally <- putInHand p "01083"
        energy <- putInHand p "01088"
        choose "sneak in Mockingbird" (isPlay sneak)
        choose "choose the ally" (isTargetChoice ally)
        choose "pay" (isPayWith energy)
        pass
        resolve [ReturnToHand $ toRef $ allyIdOf ally]
        payCodes p ally ["01088", "01089"]
        pass
        runNow [EndPhase PlayerPhase]
        alliesWith "01083" >>= (`shouldSatisfy` (not . null))

    it "can put in Black Panther, whose enters-play response holds an event that later sacrifices him" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        [held] <- stackDiscard p ["23019"]
        sneak <- putInHand p "23017"
        panther <- putInHand p "23012"
        energy <- putInHand p "01088"
        choose "sneak in Black Panther" (isPlay sneak)
        choose "choose him" (isTargetChoice panther)
        choose "pay" (isPayWith energy)
        choose "use his enters-play response" (ownAbility (allyIdOf panther) 1)
        choose "hold the event" (isTargetChoice held)
        pass
        playWith held []
        chooseVillain
        pass
        runNow [EndPhase PlayerPhase]
        villainDamageNow `shouldReturn` 4
        discard <- discardOf p
        length (filter ((== pcCardId panther) . pcCardId) discard) `shouldBe` 1
        length (filter ((== pcCardId held) . pcCardId) discard) `shouldBe` 1

  describe "Save the Day (23018)" do
    it "sacrifices its own ally and removes the ally's printed cost rather than its reduced paid cost" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        ally <- discountedVision p
        placeMainThreat 5
        event <- putInHand p "23018"
        payCodes p event ["01088"]
        chooseMainScheme
        pass
        mainThreat `shouldReturn` 1
        discardOf p >>= (`shouldSatisfy` elem ally)

    it "is not a thwart, so it removes main-scheme threat through patrol and leaves confusion" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "01068"
        _ <- addMinion p Minions.headhuntersHenchman
        placeMainThreat 4
        resolve [IdentityMessage p IdentityConfused]
        event <- putInHand p "23018"
        payCodes p event ["01088"]
        chooseMainScheme
        pass
        mainThreat `shouldReturn` 0
        isConfused p `shouldReturn` True

    it "respects crisis while removing as much threat as a side scheme has" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "01068"
        placeMainThreat 4
        side <- revealNow p Schemes.crowdControl
        pass
        event <- putInHand p "23018"
        payCodes p event ["01088"]
        main <- mainSchemeIdNow
        offers (isTargetChoice main) `shouldReturn` False
        choose "remove threat from the crisis scheme" (isTargetChoice $ sideSchemeOf side)
        pass
        sideSchemeThreat (sideSchemeOf side) `shouldReturn` 0
        mainThreat `shouldReturn` 4

    it "cannot sacrifice an ally when no scheme has threat to remove" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        ally <- putIntoPlay p "01068"
        event <- putInHand p "23018"
        _ <- putInHand p "01088"
        offers (isPlay event) `shouldReturn` False
        alliesWith "01068" >>= (`shouldSatisfy` (not . null))
        discardOf p >>= (`shouldSatisfy` notElem ally)

  describe "Go Down Swinging (23019)" do
    it "uses printed cost rather than reduced cost and deals nonattack damage through guard" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- discountedVision p
        _ <- addMinion p Minions.hydraMercenary
        resolve [IdentityMessage p IdentityStunned]
        event <- putInHand p "23019"
        playWith event []
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4
        isStunned p `shouldReturn` True

    it "can discard an exhausted ally and does not gain overkill" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        ally <- putIntoPlay p "01068"
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        guard <- addMinion p Minions.hydraMercenary
        event <- putInHand p "23019"
        playWith event []
        choose "damage the guard" (isTargetChoice guard)
        pass
        villainDamageNow `shouldReturn` 0
        minionDamageOf guard `shouldReturn` Nothing
        damageOf p `shouldReturn` 0

    it "does not trigger a surviving enemy's retaliate" do
      tg <- warMachine
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "01068"
        enemy <- addMinion p Minions.whiplash
        withGame_ $ entitiesL . minionsL . ix enemy %~ overMinion (\a -> a {minionHitPoints = HP 12})
        event <- putInHand p "23019"
        playWith event []
        choose "deal ability damage" (isTargetChoice enemy)
        pass
        minionDamageOf enemy `shouldReturn` Just 4
        damageOf p `shouldReturn` 0

  it "neither sacrifice event can discard another player's ally" do
    tg <- testGame ["war-machine", "spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      _ <- putIntoPlay (player2 tg) "01068"
      placeMainThreat 3
      day <- putInHand p "23018"
      swing <- putInHand p "23019"
      _ <- putInHand p "01088"
      offers (isPlay day) `shouldReturn` False
      offers (isPlay swing) `shouldReturn` False

  it "neither sacrifice event can use a printed-zero-cost ally to create a zero-effect play" do
    tg <- warMachine
    let p = player1 tg
    inGame tg do
      asHero p
      ant <- putInHand p "12011"
      payCodes p ant ["01088"]
      pass
      allyIn "12011" >>= (\a -> allyCounters a `shouldBe` 2)
      placeMainThreat 4
      day <- putInHand p "23018"
      swing <- putInHand p "23019"
      _ <- putInHand p "01088"
      offers (isPlay day) `shouldReturn` False
      offers (isPlay swing) `shouldReturn` False
