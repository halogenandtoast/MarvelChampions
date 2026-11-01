module Marvel.Aspect.CyclopsPackSpec (spec) where

import Marvel.Ally.Types qualified as Ally
import Marvel.Card
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Modifier
import Marvel.Identity (getModifiedHp)
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Phase
import Marvel.Query
import Marvel.Trait qualified as Trait
import TestImport

heroTable :: IO (TestGame, IdentityId)
heroTable = do
  tg <- testGame ["cyclops"]
  let p = player1 tg
  inGame tg do
    changeForm p
    clearHand p
  pure (tg, p)

allyPower :: AllyId -> GameAppT (Natural, Natural, Natural)
allyPower aid = do
  a <- getsGame (fmap toAttrs . lookup aid . gameAllies)
  case a of
    Nothing -> fail "ally is missing"
    Just attrs -> (,,) <$> Ally.getModifiedThwart attrs <*> Ally.getModifiedAttack attrs <*> Ally.getModifiedHitPoints attrs

allyDamageNow :: AllyId -> GameAppT (Maybe Natural)
allyDamageNow aid = getsGame (fmap (allyDamage . toAttrs) . lookup aid . gameAllies)

allyIsReady :: AllyId -> GameAppT Bool
allyIsReady aid = getsGame (maybe False (not . allyExhausted . toAttrs) . lookup aid . gameAllies)

basicAttack :: AllyId -> GameAppT ()
basicAttack aid = choose "ally attacks" \case
  UseAbility ab -> abilitySource ab == toSource aid && AllyAttack aid `elem` abilityChoices ab
  _ -> False

attach :: IdentityId -> CardCode -> Target -> GameAppT PlayerCard
attach p code target = do
  card <- putInHand p code
  let costs = if code == "33015" || code == "32043" then ["01088"] else []
  resources <- traverse (putInHand p) costs
  playOn card target resources
  pure card

newtype TestStats = TestStats (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage TestStats where
  runMessage msg (TestStats attrs) = TestStats <$> runMessage msg attrs

boostMinionAttack :: MinionId -> Natural -> GameAppT ()
boostMinionAttack mid n = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (TestStats EffectAttrs
    {effectId = eid, effectCardCode = "33034", effectSource = GameRef,
     effectMatcher = MinionEntity $ MinionWithId mid, effectModifiers = [AttackModifier n], effectEnds = Nothing})

spec :: Spec
spec = describe "Cyclops pack aspect and basic tactics" do
  describe "Danger Room Training" do
    it "attaches to an X-Men ally and increases all three printed stats" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "33019"
        void $ attach p "33015" (AllyRef aid)
        allyPower aid `shouldReturn` (2, 3, 4)

    it "cannot attach to an ally without X-Men" do
      (tg, p) <- heroTable
      inGame tg do
        void $ putIntoPlay p "01002"
        card <- putInHand p "33015"
        void $ putInHand p "01088"
        offers (isPlay card) `shouldReturn` False

    it "cannot attach a second Training, even one with a different name" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "33019"
        void $ attach p "32043" (AllyRef aid)
        card <- putInHand p "33015"
        void $ putInHand p "01088"
        offers (isPlay card) `shouldReturn` False

  for_ [("Coordinated Attack", "33016"), ("Marked", "33032"), ("Befuddle", "33033"), ("Pinned Down", "33034")] \(name, code) ->
    describe name do
      it "attaches only to a minion and permits one copy per minion" do
        (tg, p) <- heroTable
        inGame tg do
          minion <- addMinion p Minions.abomination
          card <- putInHand p code
          choose "play tactic" (isPlay card)
          villain <- villainIdNow
          offers (isTargetChoice villain) `shouldReturn` False
          choose "target minion" (isTargetChoice minion)
          whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
          second <- putInHand p code
          offers (isPlay second) `shouldReturn` False

  describe "Pinned Down" do
    it "reduces attached minion ATK by two, to a minimum of zero" do
      (tg, p) <- heroTable
      inGame tg do
        minion <- addMinion p Minions.hydraMercenary
        void $ attach p "33034" (MinionRef minion)
        attrs <- getsGame (fmap toAttrs . lookup minion . gameMinions)
        traverse Minion.getModifiedAttack attrs `shouldReturn` Just 0
        boostMinionAttack minion 2
        updated <- getsGame (fmap toAttrs . lookup minion . gameMinions)
        traverse Minion.getModifiedAttack updated `shouldReturn` Just 1

  describe "Marked" do
    it "gives a hero attack against its host overkill" do
      (tg, p) <- heroTable
      inGame tg do
        minion <- addMinion p Minions.shocker
        void $ attach p "33032" (MinionRef minion)
        -- Ricochet Beam's first 3 defeats Shocker exactly; target its second
        -- instance at an upgraded villain in the dedicated signature spec.
        messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.dealDamage minion p (toDamage 5 $ FromPlayerAttack p)
        resolve messages
        villainDamageNow `shouldReturn` 2

    it "gives ally attacks overkill and does not give ability damage overkill" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "33013"
        minion <- addMinion p Minions.shocker
        void $ attach p "33032" (MinionRef minion)
        messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.dealDamage minion aid (toDamage 5 $ FromAllyAttack aid)
        resolve messages
        villainDamageNow `shouldReturn` 2
        other <- addMinion p Minions.shocker
        void $ attach p "33032" (MinionRef other)
        ordinary <- Lifted.runQueueT $ Lifted.capture $ Lifted.dealDamage other p (toDamage 5 FromAbility)
        resolve ordinary
        villainDamageNow `shouldReturn` 2

  describe "Honorary X-Men" do
    it "grants an ally X-Men and one hit point, enabling Training" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "01002"
        void $ attach p "33035" (AllyRef aid)
        selectAny (AllyWithId aid <> AllyWithTrait Trait.XMen) `shouldReturn` True
        allyPower aid `shouldReturn` (1, 1, 3)
        void $ attach p "33015" (AllyRef aid)
        allyPower aid `shouldReturn` (2, 2, 4)

    it "can attach to an identity and prohibits a second copy on that character" do
      (tg, p) <- heroTable
      inGame tg do
        void $ attach p "33035" (IdentityRef p)
        identity <- getIdentity p
        getModifiedHp identity `shouldReturn` 11
        second <- putInHand p "33035"
        offers (isPlay second) `shouldReturn` False

    it "follows an attached ally becoming a minion without granting its controller the bonus" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "01002"
        void $ attach p "33035" (AllyRef aid)
        resolve [AllyBecomesMinion aid]
        let mid = MinionId $ unAllyId aid
        selectAny (MinionWithId mid <> MinionWithTrait Trait.XMen) `shouldReturn` True
        attrs <- getsGame (fmap toAttrs . lookup mid . gameMinions)
        traverse Minion.getModifiedHitPoints attrs `shouldReturn` Just 3
        identity <- getIdentity p
        getModifiedHp identity `shouldReturn` 10

    it "requires the playing identity to have X-Men" do
      tg <- testGame ["cyclops"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "33035"
        offers (isPlay card) `shouldReturn` False

    it "allows a different player to receive its identity attachment" do
      tg <- testGame ["cyclops", "spider-man"]
      let p = player1 tg
          q = fromMaybe (error "second player") (atMay tg.seats 1)
      inGame tg do
        changeForm p
        clearHand p
        void $ attach p "33035" (IdentityRef q)
        selectAny (IdentityWithId q <> IdentityWithTrait Trait.XMen) `shouldReturn` True
        identity <- getIdentity q
        getModifiedHp identity `shouldReturn` 11

  describe "Game Time" do
    it "readies and heals a trained ally" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "33019"
        void $ attach p "33015" (AllyRef aid)
        resolve [AllyMessage aid ExhaustedAlly, AllyMessage aid $ AllyDamaged GameRef $ toDamage 2 FromAbility]
        card <- putInHand p "33022"
        playOn card aid []
        allyIsReady aid `shouldReturn` True
        allyDamageNow aid `shouldReturn` Just 1

    it "can heal a ready trained ally or ready an undamaged one" do
      for_ [False, True] \exhausted -> do
        (tg, p) <- heroTable
        inGame tg do
          aid <- allyIdOf <$> putIntoPlay p "33019"
          void $ attach p "33015" (AllyRef aid)
          resolve [AllyMessage aid $ if exhausted then ExhaustedAlly else AllyDamaged GameRef (toDamage 1 FromAbility)]
          card <- putInHand p "33022"
          playOn card aid []
          allyIsReady aid `shouldReturn` True
          allyDamageNow aid `shouldReturn` Just 0

    it "cannot target untrained allies or an undamaged ready trained ally" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "33019"
        resolve [AllyMessage aid ExhaustedAlly]
        card <- putInHand p "33022"
        offers (isPlay card) `shouldReturn` False
        void $ attach p "33015" (AllyRef aid)
        resolve [AllyMessage aid ReadiedAlly]
        offers (isPlay card) `shouldReturn` False

    it "can ready and heal another player's trained ally" do
      tg <- testGame ["cyclops", "spider-man"]
      let p = player1 tg
          q = fromMaybe (error "second player") (atMay tg.seats 1)
      inGame tg do
        changeForm p
        clearHand p
        aid <- allyIdOf <$> putIntoPlay q "33019"
        void $ attach p "33015" (AllyRef aid)
        resolve [AllyMessage aid ExhaustedAlly, AllyMessage aid $ AllyDamaged GameRef $ toDamage 1 FromAbility]
        card <- putInHand p "33022"
        playOn card aid []
        allyIsReady aid `shouldReturn` True
        allyDamageNow aid `shouldReturn` Just 0

    it "is unavailable in alter ego" do
      tg <- testGame ["cyclops"]
      let p = player1 tg
      inGame tg do
        clearHand p
        aid <- allyIdOf <$> putIntoPlay p "33019"
        void $ attach p "33015" (AllyRef aid)
        resolve [AllyMessage aid ExhaustedAlly]
        card <- putInHand p "33022"
        offers (isPlay card) `shouldReturn` False

  describe "Befuddle" do
    it "optionally uses the hero's THW for one basic attack and then expires" do
      (tg, p) <- heroTable
      inGame tg do
        minion <- addMinion p Minions.abomination
        bf <- attach p "33033" (MinionRef minion)
        choose "basic attack" (isHeroAttack p)
        choose "Abomination" (isTargetChoice minion)
        choose "Befuddle" (isAbilityOf $ upgradeIdOf bf)
        pass
        minionDamageOf minion `shouldReturn` Just 2
        resolve [IdentityMessage p ReadiedIdentity]
        choose "basic attack again" (isHeroAttack p)
        choose "Abomination" (isTargetChoice minion)
        pass
        minionDamageOf minion `shouldReturn` Just 3

    it "uses an ally's THW and retains its attack consequential icons" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "33013"
        minion <- addMinion p Minions.abomination
        bf <- attach p "33033" (MinionRef minion)
        basicAttack aid
        choose "Abomination" (isTargetChoice minion)
        choose "Befuddle" (isAbilityOf $ upgradeIdOf bf)
        pass
        minionDamageOf minion `shouldReturn` Just 1
        allyDamageNow aid `shouldReturn` Just 2

    it "can be declined and does not interrupt Optic Blast" do
      (tg, p) <- heroTable
      inGame tg do
        minion <- addMinion p Minions.abomination
        void $ attach p "33033" (MinionRef minion)
        choose "basic attack" (isHeroAttack p)
        choose "Abomination" (isTargetChoice minion)
        pass
        minionDamageOf minion `shouldReturn` Just 1

    it "does not offer its basic attack interrupt during Optic Blast" do
      (tg, p) <- heroTable
      inGame tg do
        minion <- addMinion p Minions.abomination
        bf <- attach p "33033" (MinionRef minion)
        resource <- putInHand p "33022"
        choose "Optic Blast" \case
          UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 1
          _ -> False
        payWith [resource]
        choose "Abomination" (isTargetChoice minion)
        offers (isAbilityOf $ upgradeIdOf bf) `shouldReturn` False
        pass
        minionDamageOf minion `shouldReturn` Just 3

  describe "Coordinated Attack" do
    it "reduces attack consequential damage, including after its host is defeated" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "33013"
        minion <- addMinion p Minions.shocker
        void $ attach p "33016" (MinionRef minion)
        basicAttack aid
        choose "Shocker" (isTargetChoice minion)
        pass
        minionDamageOf minion `shouldReturn` Nothing
        allyDamageNow aid `shouldReturn` Just 1
        resolve [AllyMessage aid ReadiedAlly]
        other <- addMinion p Minions.abomination
        basicAttack aid
        choose "Abomination" (isTargetChoice other)
        pass
        allyDamageNow aid `shouldReturn` Just 3

    it "reduces one attack consequence to zero" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "33019"
        minion <- addMinion p Minions.abomination
        void $ attach p "33016" (MinionRef minion)
        basicAttack aid
        choose "Abomination" (isTargetChoice minion)
        pass
        allyDamageNow aid `shouldReturn` Just 0

    it "does not reduce thwart consequential damage" do
      (tg, p) <- heroTable
      inGame tg do
        aid <- allyIdOf <$> putIntoPlay p "33013"
        minion <- addMinion p Minions.abomination
        void $ attach p "33016" (MinionRef minion)
        resolve [AllyMessage aid AllyThwartConsequential]
        allyDamageNow aid `shouldReturn` Just 1

  describe "Effective Leadership" do
    it "optionally buffs an ally until the end of the phase" do
      (tg, p) <- heroTable
      inGame tg do
        card <- putInHand p "33013"
        resource <- putInHand p "33018"
        double <- putInHand p "01088"
        single <- putInHand p "33022"
        playWith card [resource, double, single]
        choose "Effective Leadership" (isLabel "$label.effectiveLeadership")
        pass
        let aid = allyIdOf card
        allyPower aid `shouldReturn` (2, 4, 6)
        -- Stop at the next question after the phase ends. Automatically resolving
        -- the villain phase can select this ally as a defender and defeat it.
        runNow [EndPhase PlayerPhase]
        allyPower aid `shouldReturn` (1, 3, 6)

    it "stacks two spent copies, and allows declining each interrupt" do
      for_ [False, True] \accept -> do
        (tg, p) <- heroTable
        inGame tg do
          card <- putInHand p "33013"
          resources <- traverse (putInHand p) ["33018", "33018", "01088"]
          playWith card resources
          for_ [1 :: Int, 2] \_ -> choose "spending response"
            (isLabel $ if accept then "$label.effectiveLeadership" else "$label.noResponse")
          pass
          allyPower (allyIdOf card) `shouldReturn` (if accept then (3, 5, 6) else (1, 3, 6))

    it "resolves the spending interrupt before Pym Particles' after-spending response" do
      for_ [False, True] \accept -> do
        tg <- testGame ["ant-man"]
        let p = player1 tg
        inGame tg do
          changeFormTo p A
          pass
          clearHand p
          ally <- putInHand p "33013"
          particles <- putInHand p "12006"
          leadership <- putInHand p "33018"
          energy <- putInHand p "01088"
          -- Particles is spent first so spend-order dispatch cannot accidentally
          -- put its response ahead of Effective Leadership's interrupt.
          playWith ally [particles, leadership, energy]
          offers (isLabel "$label.effectiveLeadership") `shouldReturn` True
          offers (isLabel $ countLabel "pymParticlesDraw" 1) `shouldReturn` False
          choose "Effective Leadership interrupt"
            (isLabel $ if accept then "$label.effectiveLeadership" else "$label.noResponse")
          offers (isLabel $ countLabel "pymParticlesDraw" 1) `shouldReturn` True
          offers (isLabel "$label.effectiveLeadership") `shouldReturn` False
          before <- length <$> handOf p
          choose "Pym Particles response" (isLabel $ countLabel "pymParticlesDraw" 1)
          pass
          (length <$> handOf p) `shouldReturn` (before + 1)
          allyPower (allyIdOf ally) `shouldReturn` (if accept then (2, 4, 6) else (1, 3, 6))

    it "does not trigger when spent on a non-ally card" do
      (tg, p) <- heroTable
      inGame tg do
        card <- putInHand p "33003"
        resource <- putInHand p "33018"
        single <- putInHand p "33022"
        playWith card [resource, single]
        offers (isLabel "$label.effectiveLeadership") `shouldReturn` False

  describe "Psychic Rapport" do
    it "readies Cyclops and Phoenix and recovers a chosen Cyclops signature card" do
      (tg, p) <- heroTable
      inGame tg do
        phoenix <- allyIdOf <$> putIntoPlay p "33002"
        signature <- putInHand p "33009"
        resolve [DiscardedCard $ PlayerCard signature]
        rapport <- putInHand p "33023"
        energy <- putInHand p "01088"
        resolve [IdentityMessage p Lifted.ExhaustedIdentity, AllyMessage phoenix ExhaustedAlly]
        playWith rapport [energy]
        choose "return a Cyclops card" (isLabel "$label.psychicRapportReturnCard")
        choose "Ricochet Beam" (isTargetChoice signature)
        pass
        identity <- getIdentity p
        playerIdentityExhausted (toAttrs identity) `shouldBe` False
        allyIsReady phoenix `shouldReturn` True
        handOf p >>= (`shouldSatisfy` elem signature)
        discardOf p >>= (`shouldSatisfy` notElem signature)

    it "cannot be played without a friendly Phoenix in play" do
      (tg, p) <- heroTable
      inGame tg do
        rapport <- putInHand p "33023"
        void $ putInHand p "01088"
        offers (isPlay rapport) `shouldReturn` False
