module Marvel.Hero.CyclopsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.AlterEgo.Types qualified as AlterEgo
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Message.Identity qualified as Identity
import Marvel.Phase
import Marvel.Stats
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W
import TestImport
import Test.Hspec qualified as Hspec

cyclops :: IO TestGame
cyclops = testGame ["cyclops"]

inHero :: IdentityId -> GameAppT ()
inHero p = changeForm p >> pass >> clearHand p

isOpticBlast :: IdentityId -> Choice -> Bool
isOpticBlast p = \case
  UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1
  _ -> False

isConstantTraining :: IdentityId -> Choice -> Bool
isConstantTraining = isOpticBlast

attach :: (IsRef enemy) => IdentityId -> CardCode -> enemy -> GameAppT PlayerCard
attach p code enemy = do
  card <- putInHand p code
  payment <- if code == "33005" then (:[]) <$> putInHand p "01088" else pure []
  playOn card enemy payment
  pure card

attackWithBlast :: IdentityId -> GameAppT ()
attackWithBlast p = do
  resource <- putInHand p "01090"
  choose "Optic Blast" (isOpticBlast p)
  payWith [resource]

isPresent :: PlayerCard -> GameAppT Bool
isPresent card = getsGame (HashMap.member (upgradeIdOf card) . gameUpgrades)

villainAttack :: GameAppT Natural
villainAttack = Villain.getModifiedAttack . toAttrs =<< getVillain

modifiedMinionAttack :: MinionId -> GameAppT Natural
modifiedMinionAttack m = getsGame (HashMap.lookup m . gameMinions) >>= maybe (error "missing minion") (Minion.getModifiedAttack . toAttrs)

spec :: Spec
spec = describe "Cyclops" do
  describe "Scott Summers" do
    it "has the printed alter-ego stats and finds a Tactic upgrade once per round" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        handSizeOf p `shouldReturn` 6
        hitPointsOf p `shouldReturn` 10
        currentIdentity <$> getPlayer p >>= \case
          AlterEgoSide a -> AlterEgo.alterEgoBaseRecovery (toAttrs a) `shouldBe` Rec 3
          _ -> liftIO $ expectationFailure "Scott Summers was not in alter-ego form"
        clearHand p
        [target] <- stackDeck p ["33006"]
        choose "Constant Training" (isConstantTraining p)
        choose "Practiced Defense" (isTargetChoice target)
        handOf p >>= (`shouldSatisfy` elem target)
        offers (isConstantTraining p) `shouldReturn` False

    it "does not search for Tactic events or non-Tactic upgrades" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        clearHand p
        [eventCard, otherUpgrade] <- stackDeck p ["33022", "33003"]
        choose "Constant Training" (isConstantTraining p)
        offers (isTargetChoice eventCard) `shouldReturn` False
        offers (isTargetChoice otherUpgrade) `shouldReturn` False

    it "accepts the printed cross-aspect X-Men allies in its starter deck" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        cards <- handOf p
        deck <- unDeck . playerIdentityDeck <$> playerAttrs p
        discarded <- discardOf p
        let allCodes = map toCardCode (cards <> deck <> discarded)
        allCodes `shouldSatisfy` \codes -> all (`elem` codes) ["33012", "33013", "33014"]

  describe "Optic Blast" do
    it "has printed hero stats and spends a resource for 3 damage without exhausting" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        handSizeOf p `shouldReturn` 5
        currentIdentity <$> getPlayer p >>= \case
          HeroSide h -> do
            Hero.getModifiedThwart (toAttrs h) `shouldReturn` 2
            Hero.getModifiedAttack (toAttrs h) `shouldReturn` 1
            Hero.getModifiedDefense (toAttrs h) `shouldReturn` 2
          _ -> liftIO $ expectationFailure "Cyclops was not in hero form"
        vid <- villainIdNow
        _ <- attach p "33006" vid
        attackWithBlast p
        choose "villain" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 3
        exhaustedNow p `shouldReturn` False
        offers (isOpticBlast p) `shouldReturn` False

    it "is unavailable without an upgraded enemy" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putInHand p "01088"
        offers (isOpticBlast p) `shouldReturn` False

    it "can spend a resource to clear stun even without an upgraded enemy" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        resolve [IdentityMessage p IdentityStunned]
        _ <- putInHand p "01088"
        offers (isOpticBlast p) `shouldReturn` True
        attackWithBlast p
        isStunned p `shouldReturn` False
        villainDamageNow `shouldReturn` 0
        offers (isOpticBlast p) `shouldReturn` False

    it "does not offer an enemy lacking an upgrade as a target" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        vid <- villainIdNow
        minion <- addMinion p Minions.abomination
        _ <- attach p "33006" minion
        attackWithBlast p
        offers (isTargetChoice vid) `shouldReturn` False
        choose "upgraded minion" (isTargetChoice minion)
        minionDamageOf minion `shouldReturn` Just 3

    it "pays and removes stun instead of attacking, without offering Full Blast" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        vid <- villainIdNow
        _ <- attach p "33006" vid
        _ <- putInHand p "33008"
        _ <- putInHand p "01088"
        resolve [IdentityMessage p IdentityStunned]
        attackWithBlast p
        villainDamageNow `shouldReturn` 0
        offers (isOpticBlast p) `shouldReturn` False
        playerIdentityStunned <$> playerAttrs p `shouldReturn` False

  describe "Phoenix" do
    it "returns a chosen Cyclops signature card after entering play" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        clearHand p
        target <- putInDiscard p "33009"
        other <- putInDiscard p "33022"
        phoenix <- putInHand p "33002"
        payment <- traverse (putInHand p) ["01088", "01090"]
        playWith phoenix payment
        choose "Phoenix response" (isAbilityOfCard phoenix)
        offers (isTargetChoice other) `shouldReturn` False
        choose "Ricochet Beam" (isTargetChoice target)
        handOf p >>= (`shouldSatisfy` elem target)
        let aid = allyIdOf phoenix
        getsGame (fmap (allyAttack . toAttrs) . HashMap.lookup aid . gameAllies) `shouldReturn` Just (Atk 2)

    it "may decline the recovery response" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        clearHand p
        target <- putInDiscard p "33009"
        phoenix <- putInHand p "33002"
        payment <- traverse (putInHand p) ["01088", "01090"]
        playWith phoenix payment
        pass
        handOf p >>= (`shouldSatisfy` notElem target)

  describe "Ruby Quartz Visor" do
    it "alone makes Optic Blast affordable and pays its resource without discarding a card" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        visor <- putIntoPlay p "33003"
        vid <- villainIdNow
        _ <- attach p "33006" vid
        clearHand p
        choose "Optic Blast" (isOpticBlast p)
        choose "Visor resource" (isAbilityOfCard visor)
        payWith []
        choose "villain" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 3
        upgradeIsExhausted (upgradeIdOf visor) `shouldReturn` True

    it "is not offered to pay for a played card" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        visor <- putIntoPlay p "33003"
        event <- putInHand p "33009"
        _ <- putInHand p "01088"
        choose "Ricochet Beam" (isPlay event)
        offers (isAbilityOfCard visor) `shouldReturn` False

    it "grants piercing only when used to pay for this Optic Blast" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        visor <- putIntoPlay p "33003"
        vid <- villainIdNow
        _ <- attach p "33006" vid
        resolve [VillainMessage vid VillainBecomeTough]
        choose "Optic Blast" (isOpticBlast p)
        choose "Visor resource" (isAbilityOfCard visor)
        payWith []
        choose "villain" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 3
        Villain.villainTough . toAttrs <$> getVillain `shouldReturn` False

    it "does not grant piercing when a hand resource pays instead" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        visor <- putIntoPlay p "33003"
        vid <- villainIdNow
        _ <- attach p "33006" vid
        resolve [VillainMessage vid VillainBecomeTough]
        attackWithBlast p
        choose "villain" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 0
        upgradeIsExhausted (upgradeIdOf visor) `shouldReturn` False

    it "ignores retaliate with a Visor-paid blast" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        visor <- putIntoPlay p "33003"
        minion <- addMinion p Minions.whiplash
        _ <- attach p "33006" minion
        before <- damageOf p
        choose "Optic Blast" (isOpticBlast p)
        choose "Visor resource" (isAbilityOfCard visor)
        payWith []
        choose "Whiplash" (isTargetChoice minion)
        minionDamageOf minion `shouldReturn` Just 3
        damageOf p `shouldReturn` before

    it "does not ignore retaliate when the Visor does not pay" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "33003"
        minion <- addMinion p Minions.whiplash
        _ <- attach p "33006" minion
        before <- damageOf p
        attackWithBlast p
        choose "Whiplash" (isTargetChoice minion)
        damageOf p `shouldReturn` before + 1

  describe "Field Commander and Temporary" do
    it "takes Cyclops's turn first without changing the first-player order or granting a second turn" do
      tg <- testGame ["spider-man", "cyclops", "captain-marvel"]
      let p = player2 tg
      inGame tg do
        _ <- putIntoPlay p "33004"
        order <- getsGame gamePlayerOrder
        -- Replace the dealt phase: its unfinished turns must not remain queued
        -- behind the new player phase whose complete turn order is tested here.
        clearQueue
        runNow [BeginPhase PlayerPhase]
        fmap fst <$> asked `shouldReturn` Just p
        pending <- withQueue $ \messages -> (messages, [who | IdentityMessage who BeginTurn <- messages])
        pending `shouldBe` filter (/= p) order
        choose "end Cyclops turn" (== EndTurn)
        fmap fst <$> asked `shouldReturn` headMay order
        choose "end first player's turn" (== EndTurn)
        fmap fst <$> asked `shouldReturn` lastMay order
        getsGame gamePlayerOrder `shouldReturn` order
        isQueued (== IdentityMessage p BeginTurn) `shouldReturn` False

    it "keeps Cyclops upgrades on minions, but Temporary still discards one on the villain" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "33004"
        minion <- addMinion p Minions.abomination
        vid <- villainIdNow
        kept <- attach p "33006" minion
        expired <- attach p "33007" vid
        resolve [CheckWindows [W.Window W.When W.RoundEnded]]
        isPresent kept `shouldReturn` True
        isPresent expired `shouldReturn` False

    it "lets a previously retained minion tactic expire after Field Commander leaves play" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        commander <- putIntoPlay p "33004"
        minion <- addMinion p Minions.abomination
        tactic <- attach p "33006" minion
        resolve [RemoveFromPlay (toRef $ upgradeIdOf commander)]
        resolve [CheckWindows [W.Window W.When W.RoundEnded]]
        isPresent tactic `shouldReturn` False

  describe "Exploit Weakness" do
    it "adds 1 to each instance of Ricochet Beam against the same enemy" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        vid <- villainIdNow
        _ <- attach p "33005" vid
        beam <- putInHand p "33009"
        payment <- putInHand p "01088"
        playWith beam [payment]
        choose "first beam" (isTargetChoice vid)
        choose "second beam" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 8

    it "does not increase nonattack damage or permit a duplicate on the same enemy" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        vid <- villainIdNow
        _ <- attach p "33005" vid
        resolve [VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 2 FromAbility)]
        villainDamageNow `shouldReturn` 2
        duplicate <- putInHand p "33005"
        _ <- putInHand p "01088"
        offers (isPlay duplicate) `shouldReturn` False

  describe "Practiced Defense" do
    it "reduces attached villain and minion ATK by 1" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        vid <- villainIdNow
        minion <- addMinion p Minions.abomination
        vBefore <- villainAttack
        mBefore <- modifiedMinionAttack minion
        _ <- attach p "33006" vid
        _ <- attach p "33006" minion
        villainAttack `shouldReturn` vBefore - 1
        modifiedMinionAttack minion `shouldReturn` mBefore - 1

    it "clamps at zero and combines ATK penalties with Gene Therapy before clamping" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        minion <- addMinion p Minions.misterSinister
        _ <- attach p "33006" minion
        modifiedMinionAttack minion `shouldReturn` 0
        pinned <- attach p "33034" minion
        modifiedMinionAttack minion `shouldReturn` 0
        therapy <- encounterCard Attachments.geneTherapy
        resolve [RevealEncounterCard p therapy]
        modifiedMinionAttack minion `shouldReturn` 0
        resolve [RemoveFromPlay (toRef $ upgradeIdOf pinned)]
        modifiedMinionAttack minion `shouldReturn` 2

  describe "Priority Target" do
    it "gives the player defeating its enemy two cards" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        minion <- addMinion p Minions.shocker
        target <- attach p "33007" minion
        clearHand p
        payment <- putInHand p "01088"
        choose "Optic Blast" (isOpticBlast p)
        payWith [payment]
        choose "Shocker" (isTargetChoice minion)
        choose "Priority Target" (isAbilityOfCard target)
        length <$> handOf p `shouldReturn` 2

    it "offers only the copy attached to the defeated enemy" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        minion <- addMinion p Minions.shocker
        other <- addMinion p Minions.abomination
        target <- attach p "33007" minion
        unrelated <- attach p "33007" other
        clearHand p
        attackWithBlast p
        choose "Shocker" (isTargetChoice minion)
        offers (isAbilityOfCard unrelated) `shouldReturn` False
        choose "attached Priority Target" (isAbilityOfCard target)
        length <$> handOf p `shouldReturn` 2

    it "draws for the teammate who defeated its minion instead of its controller" do
      tg <- testGame ["cyclops", "spider-man"]
      let p = player1 tg
          teammate = player2 tg
      inGame tg do
        inHero p
        minion <- addMinion p Minions.hydraBomber
        target <- attach p "33007" minion
        clearHand p
        endTurn
        inHero teammate
        choose "teammate's basic attack" (isHeroAttack teammate)
        choose "Hydra Bomber" (isTargetChoice minion)
        choose "Priority Target" (isAbilityOfCard target)
        length <$> handOf teammate `shouldReturn` 2
        length <$> handOf p `shouldReturn` 0

  describe "Full Blast" do
    it "exhausts Cyclops, adds 8 damage and grants overkill" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        minion <- addMinion p Minions.shocker
        _ <- attach p "33006" minion
        full <- putInHand p "33008"
        resource <- putInHand p "01088"
        fullPayment <- putInHand p "01090"
        choose "Optic Blast" (isOpticBlast p)
        payWith [resource]
        playWith full [fullPayment]
        choose "Shocker" (isTargetChoice minion)
        villainDamageNow `shouldReturn` 8
        exhaustedNow p `shouldReturn` True

    it "is not offered on a basic attack" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        full <- putInHand p "33008"
        _ <- putInHand p "01088"
        choose "basic attack" (isHeroAttack p)
        offers (isPlay full) `shouldReturn` False

    it "cannot be played when Cyclops cannot pay its exhaustion cost" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        vid <- villainIdNow
        _ <- attach p "33006" vid
        full <- putInHand p "33008"
        _ <- putInHand p "01088"
        resolve [IdentityMessage p Identity.ExhaustedIdentity]
        attackWithBlast p
        offers (isPlay full) `shouldReturn` False
        pass
        choose "villain" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 3

    it "retains Visor piercing through Full Blast's nested payment" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        visor <- putIntoPlay p "33003"
        minion <- addMinion p Minions.shocker
        _ <- attach p "33006" minion
        resolve [MinionMessage minion MinionBecomeTough]
        full <- putInHand p "33008"
        resource <- putInHand p "01088"
        choose "Optic Blast" (isOpticBlast p)
        choose "Visor resource" (isAbilityOfCard visor)
        payWith []
        playWith full [resource]
        choose "Shocker" (isTargetChoice minion)
        villainDamageNow `shouldReturn` 8

  describe "Ricochet Beam" do
    it "allows a plain first target and requires an upgraded second target" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        vid <- villainIdNow
        minion <- addMinion p Minions.abomination
        _ <- attach p "33006" vid
        beam <- putInHand p "33009"
        payment <- putInHand p "01088"
        playWith beam [payment]
        choose "first target" (isTargetChoice minion)
        offers (isTargetChoice minion) `shouldReturn` False
        choose "upgraded second target" (isTargetChoice vid)
        minionDamageOf minion `shouldReturn` Just 3
        villainDamageNow `shouldReturn` 3

    it "still deals its first 3 damage when no enemy has an upgrade" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        vid <- villainIdNow
        beam <- putInHand p "33009"
        payment <- putInHand p "01088"
        playWith beam [payment]
        choose "first beam" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 3

  describe "Tactical Brilliance" do
    it "removes 3 threat and returns a chosen Tactic from the discard pile" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        placeMainThreat 4
        before <- mainSchemeThreatNow
        tactic <- putInDiscard p "33022"
        nonTactic <- putInDiscard p "33003"
        card <- putInHand p "33010"
        payment <- putInHand p "01088"
        playWith card [payment]
        scheme <- mainSchemeIdNow
        choose "main scheme" (isTargetChoice scheme)
        offers (isTargetChoice nonTactic) `shouldReturn` False
        choose "Game Time" (isTargetChoice tactic)
        mainSchemeThreatNow `shouldReturn` before - 3
        handOf p >>= (`shouldSatisfy` elem tactic)

    it "lets confusion replace the entire thwart ability, including Tactic recovery" do
      tg <- cyclops
      let p = player1 tg
      inGame tg do
        inHero p
        placeMainThreat 3
        before <- mainSchemeThreatNow
        tactic <- putInDiscard p "33006"
        card <- putInHand p "33010"
        payment <- putInHand p "01088"
        resolve [IdentityMessage p IdentityConfused]
        playWith card [payment]
        -- Confusion replaces the entire thwart ability, including its recovery.
        mainSchemeThreatNow `shouldReturn` before
        handOf p >>= (`shouldSatisfy` notElem tactic)
  describe "deck building" do
    it "permits X-Men allies from all four aspects alongside its chosen aspect" do
      deckBuildingError (DeckList "33001a" ["leadership"] [("33011", 1), ("33012", 1), ("33013", 1), ("33014", 1), ("01069", 1), ("01088", 1), ("33003", 1)]) `Hspec.shouldBe` Nothing

    it "permits basic-only construction with any-aspect X-Men allies" do
      deckBuildingError (DeckList "33001a" [] [("33011", 1), ("33012", 1), ("33013", 1), ("33014", 1), ("01088", 1), ("33003", 1)]) `Hspec.shouldBe` Nothing

    it "permits ordinary cards and non-X-Men allies of the chosen aspect" do
      deckBuildingError (DeckList "33001b" ["LEADERSHIP"] [("01066", 1), ("01069", 1)]) `Hspec.shouldBe` Nothing

    it "rejects an off-aspect Aggression event" do
      deckBuildingError (DeckList "33001a" ["leadership"] [("01054", 1)]) `Hspec.shouldBe` Just "error.cyclopsAspects"

    it "rejects an off-aspect non-X-Men ally" do
      deckBuildingError (DeckList "33001a" ["leadership"] [("01059", 1)]) `Hspec.shouldBe` Just "error.cyclopsAspects"

    it "does not allow ordinary aspect cards without a chosen aspect" do
      deckBuildingError (DeckList "33001a" [] [("01069", 1)]) `Hspec.shouldBe` Just "error.cyclopsAspects"

    it "rejects multiple chosen aspects and invalid aspect names" do
      deckBuildingError (DeckList "33001a" ["leadership", "justice"] []) `Hspec.shouldBe` Just "error.cyclopsAspects"
      deckBuildingError (DeckList "33001a" ["neutral"] []) `Hspec.shouldBe` Just "error.cyclopsAspects"

    it "does not treat another hero's signature card as its own" do
      deckBuildingError (DeckList "33001a" ["leadership"] [("01003", 1)]) `Hspec.shouldBe` Just "error.cyclopsAspects"

    it "ignores zero-quantity off-aspect cards and leaves other heroes' validation unchanged" do
      deckBuildingError (DeckList "33001a" ["leadership"] [("01054", 0)]) `Hspec.shouldBe` Nothing
      deckBuildingError (DeckList "01001a" ["leadership"] [("01054", 1)]) `Hspec.shouldBe` Nothing
