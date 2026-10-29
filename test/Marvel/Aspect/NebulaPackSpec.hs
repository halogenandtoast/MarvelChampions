module Marvel.Aspect.NebulaPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Ally.Types qualified as Ally
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Keyword
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types (overMinion)
import Marvel.Modifier
import Marvel.Query
import Marvel.Payment (Payment (..))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport hiding (allyDamage)

newtype TestModifiers = TestModifiers (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage TestModifiers where
  runMessage msg (TestModifiers attrs) = TestModifiers <$> runMessage msg attrs

giveModifiers :: EntityMatcher -> [Modifier] -> GameAppT ()
giveModifiers matcher mods = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (TestModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = mods, effectEnds = Nothing })

inHero :: IdentityId -> GameAppT ()
inHero p = changeForm p >> pass >> clearHand p

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code resourceCodes = do
  card <- putInHand p code
  payment <- traverse (putInHand p) resourceCodes
  playWith card payment
  pure card

isCardResponse :: PlayerCard -> Choice -> Bool
isCardResponse card choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

isBasicAllyThwart :: AllyId -> Choice -> Bool
isBasicAllyThwart aid = \case
  UseAbility ab -> AllyThwart aid `elem` abilityChoices ab
  _ -> False

isBasicAllyAttack :: AllyId -> Choice -> Bool
isBasicAllyAttack aid = \case
  UseAbility ab -> AllyAttack aid `elem` abilityChoices ab
  _ -> False

allyAttrs :: AllyId -> GameAppT (Attrs Ally)
allyAttrs aid = getsGame (maybe (error "ally is not in play") toAttrs . HashMap.lookup aid . gameAllies)

allyDamage :: AllyId -> GameAppT Natural
allyDamage aid = Ally.allyDamage <$> allyAttrs aid

hasUpgrade :: PlayerCard -> GameAppT Bool
hasUpgrade card = selectAny (UpgradeWithId $ upgradeIdOf card)

minionIsConfused :: MinionId -> GameAppT Bool
minionIsConfused mid = selectAny (EnemyWithId (EnemyMinionId mid) <> ConfusedEnemy)

-- Cards that must respond are put in hand before the basic thwart opens its window.
prepareBrains :: IdentityId -> GameAppT (PlayerCard, PlayerCard)
prepareBrains p = do
  card <- putInHand p "22018"
  energy <- putInHand p "01088"
  pure (card, energy)

spec :: Spec
spec = describe "Nebula pack" do
  describe "Eros" do
    it "confuses one new minion for each mental resource paid, choosing sequentially" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        first <- addMinion p Minions.hydraMercenary
        second <- addMinion p Minions.hydraSoldier
        clearHand p
        eros <- playPaying p "22011" ["01089"]
        choose "Eros response" (isCardResponse eros)
        choose "first minion" (isTargetChoice first)
        minionIsConfused first `shouldReturn` True
        offers (isTargetChoice first) `shouldReturn` False
        offers (isTargetChoice second) `shouldReturn` True
        choose "second minion" (isTargetChoice second)
        minionIsConfused second `shouldReturn` True

    it "counts only mental resources, rather than all resources used to pay" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        first <- addMinion p Minions.hydraMercenary
        second <- addMinion p Minions.hydraSoldier
        clearHand p
        eros <- playPaying p "22011" ["22004", "01065"]
        choose "Eros response" (isCardResponse eros)
        choose "one minion" (isTargetChoice first)
        minionIsConfused first `shouldReturn` True
        minionIsConfused second `shouldReturn` False
        offers (isTargetChoice second) `shouldReturn` False

    it "counts a wild declared as mental" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        target <- addMinion p Minions.hydraMercenary
        clearHand p
        eros <- putInHand p "22011"
        wild <- putInHand p "22002"
        energy <- putInHand p "01065"
        choose "play Eros" (isPlay eros)
        choose "pay wild" (isPayWith wild)
        declareWildAs Mental
        choose "pay energy" (isPayWith energy)
        whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
        choose "Eros" (isCardResponse eros)
        choose "the minion" (isTargetChoice target)
        minionIsConfused target `shouldReturn` True

    it "does not count a wild declared as physical" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        target <- addMinion p Minions.hydraMercenary
        clearHand p
        eros <- putInHand p "22011"
        wild <- putInHand p "22002"
        energy <- putInHand p "01065"
        choose "play Eros" (isPlay eros)
        choose "pay wild" (isPayWith wild)
        declareWildAs Physical
        choose "pay energy" (isPayWith energy)
        whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
        offers (isCardResponse eros) `shouldReturn` False
        minionIsConfused target `shouldReturn` False

    it "does not offer a response when no mental resources were paid" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.hydraMercenary
        clearHand p
        eros <- playPaying p "22011" ["01088"]
        offers (isCardResponse eros) `shouldReturn` False

    it "does not trigger when put into play rather than played from hand" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.hydraMercenary
        eros <- putInHand p "22011"
        runNow [PutCardIntoPlay p eros (ResourcePayment Mental <> ResourcePayment Mental) Nothing False]
        offers (isCardResponse eros) `shouldReturn` False

    it "stops when all eligible minions have been confused, without an empty question" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        target <- addMinion p Minions.hydraMercenary
        clearHand p
        eros <- playPaying p "22011" ["01089"]
        choose "Eros" (isCardResponse eros)
        choose "only minion" (isTargetChoice target)
        minionIsConfused target `shouldReturn` True
        offers (isTargetChoice target) `shouldReturn` False

    it "cannot target the villain or a stalwart minion" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        stalwart <- addMinion p Minions.hydraSoldier
        normal <- addMinion p Minions.hydraMercenary
        withGame_ $ entitiesL . minionsL . ix stalwart %~ overMinion \a -> a {minionCardDef = (minionCardDef a) {cdKeywords = HashSet.insert Stalwart (cdKeywords $ minionCardDef a)}}
        clearHand p
        eros <- playPaying p "22011" ["01089"]
        choose "Eros" (isCardResponse eros)
        vid <- villainIdNow
        offers (isTargetChoice vid) `shouldReturn` False
        offers (isTargetChoice stalwart) `shouldReturn` False
        choose "ordinary minion" (isTargetChoice normal)
        minionIsConfused stalwart `shouldReturn` False

    it "does not offer its optional response when every minion is already confused" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        target <- addMinion p Minions.hydraMercenary
        resolve [GiveStatus (MinionRef target) ConfusedStatus]
        clearHand p
        eros <- playPaying p "22011" ["01089"]
        offers (isCardResponse eros) `shouldReturn` False

    it "does not respond to being played from somewhere other than your hand" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.hydraMercenary
        [eros] <- stackDiscard p ["22011"]
        runNow [PutCardIntoPlay p eros (ResourcePayment Mental <> ResourcePayment Mental) Nothing True]
        offers (isCardResponse eros) `shouldReturn` False

  describe "Wraith" do
    it "cancels only the boost ability before reveal, preserving its two boost icons" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        wraith <- putIntoPlay p "22012"
        _ <- stackEncounterDeck [Minions.electro, Treacheries.assault, Treacheries.advance, Treacheries.gangUp]
        beforeDeck <- encounterDeckSize
        beforeThreat <- mainSchemeThreatNow
        vid <- villainIdNow
        runNow [VillainMessage vid VillainSchemes]
        choose "cancel Electro's boost" (isCardResponse wraith)
        pass
        mainSchemeThreatNow `shouldReturn` beforeThreat + 3
        encounterDeckSize `shouldReturn` beforeDeck - 1
        allyDamage (allyIdOf wraith) `shouldReturn` 1
        Ally.allyExhausted <$> allyAttrs (allyIdOf wraith) `shouldReturn` True

    it "cannot pay the damage cost while tough" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        wraith <- putIntoPlay p "22012"
        resolve [GiveStatus (AllyRef $ allyIdOf wraith) ToughStatus]
        _ <- stackEncounterDeck [Minions.electro, Treacheries.assault, Treacheries.advance, Treacheries.gangUp]
        before <- encounterDeckSize
        vid <- villainIdNow
        runNow [VillainMessage vid VillainSchemes]
        offers (isCardResponse wraith) `shouldReturn` False
        pass
        encounterDeckSize `shouldReturn` before - 4
        allyDamage (allyIdOf wraith) `shouldReturn` 0
        Ally.allyTough <$> allyAttrs (allyIdOf wraith) `shouldReturn` True
        Ally.allyExhausted <$> allyAttrs (allyIdOf wraith) `shouldReturn` False

    it "cannot pay the damage cost while immune to damage" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        wraith <- putIntoPlay p "22012"
        giveModifiers (AllyEntity $ AllyWithId $ allyIdOf wraith) [CannotTakeDamage]
        _ <- stackEncounterDeck [Minions.electro]
        vid <- villainIdNow
        runNow [VillainMessage vid VillainSchemes]
        offers (isCardResponse wraith) `shouldReturn` False
        pass
        allyDamage (allyIdOf wraith) `shouldReturn` 0

    it "can pay its last hit point and still cancel the boost ability" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        wraith <- putIntoPlay p "22012"
        resolve [AllyMessage (allyIdOf wraith) $ AllyDamaged GameRef (toDamage 2 FromAbility)]
        _ <- stackEncounterDeck [Minions.electro, Treacheries.assault, Treacheries.advance, Treacheries.gangUp]
        before <- encounterDeckSize
        vid <- villainIdNow
        runNow [VillainMessage vid VillainSchemes]
        choose "pay the last hit point" (isCardResponse wraith)
        pass
        selectAny (AllyWithId $ allyIdOf wraith) `shouldReturn` False
        encounterDeckSize `shouldReturn` before - 1

    it "cannot cancel the next boost card after exhausting for the first" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        wraith <- putIntoPlay p "22012"
        extra <- encounterCard Minions.electro
        _ <- stackEncounterDeck [Minions.electro, Treacheries.assault, Treacheries.advance, Treacheries.gangUp]
        before <- encounterDeckSize
        threat <- mainSchemeThreatNow
        vid <- villainIdNow
        runNow [VillainMessage vid $ VillainDealtBoost extra, VillainMessage vid VillainSchemes]
        choose "cancel one Electro" (isCardResponse wraith)
        pass
        mainSchemeThreatNow `shouldReturn` threat + 5
        encounterDeckSize `shouldReturn` before - 4
        allyDamage (allyIdOf wraith) `shouldReturn` 1

    it "is a hero-only interrupt and cannot cancel a card without a boost ability" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        wraith <- putIntoPlay p "22012"
        _ <- stackEncounterDeck [Minions.electro]
        vid <- villainIdNow
        runNow [VillainMessage vid VillainSchemes]
        offers (isCardResponse wraith) `shouldReturn` False
        pass
        inHero p
        _ <- stackEncounterDeck [Treacheries.assault]
        runNow [VillainMessage vid VillainSchemes]
        offers (isCardResponse wraith) `shouldReturn` False
        pass
        allyDamage (allyIdOf wraith) `shouldReturn` 0

  describe "Venom" do
    it "reduces its attack consequential damage by one while the main scheme is clear" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        venom <- putIntoPlay p "22013"
        choose "Venom attacks" (isBasicAllyAttack $ allyIdOf venom)
        chooseVillain
        villainDamageNow `shouldReturn` 3
        allyDamage (allyIdOf venom) `shouldReturn` 1

    it "takes its full consequential damage while main-scheme threat remains" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        venom <- putIntoPlay p "22013"
        placeMainThreat 1
        choose "Venom attacks" (isBasicAllyAttack $ allyIdOf venom)
        chooseVillain
        allyDamage (allyIdOf venom) `shouldReturn` 2

    it "reads the clear main scheme after its own thwart removes the final threat" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        venom <- putIntoPlay p "22013"
        placeMainThreat 2
        choose "Venom thwarts" (isBasicAllyThwart $ allyIdOf venom)
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` 0
        allyDamage (allyIdOf venom) `shouldReturn` 1

    it "ignores threat on side schemes when checking its main-scheme condition" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        venom <- putIntoPlay p "22013"
        _ <- revealNow p SideSchemes.breakinAndTakin
        pass
        choose "Venom attacks" (isBasicAllyAttack $ allyIdOf venom)
        chooseVillain
        allyDamage (allyIdOf venom) `shouldReturn` 1

    it "does not reduce nonconsequential damage" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        venom <- putIntoPlay p "22013"
        resolve [AllyMessage (allyIdOf venom) $ AllyDamaged GameRef (toDamage 2 FromAbility)]
        allyDamage (allyIdOf venom) `shouldReturn` 2

  describe "Justice Served" do
    it "discards to ready your hero after its basic thwart clears the main scheme" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        justice <- putIntoPlay p "22014"
        placeMainThreat 2
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        isExhaustedNow p `shouldReturn` True
        choose "Justice Served" (isCardResponse justice)
        isExhaustedNow p `shouldReturn` False
        hasUpgrade justice `shouldReturn` False
        discard <- map toCardCode <$> discardOf p
        liftIO $ "22014" `elem` discard `shouldBe` True

    it "also readies your hero after clearing a side scheme" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        justice <- putIntoPlay p "22014"
        side <- revealNow p SideSchemes.crowdControl
        pass
        choose "basic thwart" (isHeroThwart p)
        choose "the side scheme" (isTargetChoice $ sideSchemeOf side)
        choose "Justice Served" (isCardResponse justice)
        isExhaustedNow p `shouldReturn` False
        hasUpgrade justice `shouldReturn` False

    it "also triggers after a thwart event removes the last threat" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        justice <- putIntoPlay p "22014"
        resolve [IdentityMessage p ExhaustedIdentity]
        placeMainThreat 3
        _ <- playPaying p "01060" ["01089"]
        chooseMainScheme
        choose "Justice Served" (isCardResponse justice)
        isExhaustedNow p `shouldReturn` False
        hasUpgrade justice `shouldReturn` False

    it "does not trigger before the last threat is removed" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        justice <- putIntoPlay p "22014"
        placeMainThreat 3
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        offers (isCardResponse justice) `shouldReturn` False
        isExhaustedNow p `shouldReturn` True
        hasUpgrade justice `shouldReturn` True

    it "does not trigger for threat removed without thwarting" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        justice <- putIntoPlay p "22014"
        resolve [IdentityMessage p ExhaustedIdentity]
        placeMainThreat 2
        mid <- mainSchemeIdNow
        runNow [MainSchemeMessage mid $ MainSchemeThreatRemoved (toSource p) 2]
        offers (isCardResponse justice) `shouldReturn` False
        hasUpgrade justice `shouldReturn` True
        isExhaustedNow p `shouldReturn` True

    it "does not trigger for an ally's thwart" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        justice <- putIntoPlay p "22014"
        venom <- putIntoPlay p "22013"
        placeMainThreat 2
        resolve [IdentityMessage p ExhaustedIdentity]
        choose "Venom thwarts" (isBasicAllyThwart $ allyIdOf venom)
        chooseMainScheme
        offers (isCardResponse justice) `shouldReturn` False
        hasUpgrade justice `shouldReturn` True
        isExhaustedNow p `shouldReturn` True

    it "can be played under another player's control" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        card <- putInHand p "22014"
        resource <- putInHand p "01090"
        playOn card other [resource]
        attrs <- upgradesWith "22014"
        liftIO $ map upgradeController attrs `shouldBe` [other]

    it "allows only one copy under each player's control" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22014"
        second <- putInHand p "22014"
        _ <- putInHand p "01090"
        offers (isPlay second) `shouldReturn` False

  describe "One Way or Another" do
    it "reveals the searched side scheme as a cost before drawing three cards" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        [scheme] <- stackEncounterDeck [SideSchemes.underAttack]
        card <- putInHand p "22015"
        playWith card []
        length <$> handOf p `shouldReturn` 0
        choose "reveal Under Attack" (isTargetChoice scheme)
        length <$> handOf p `shouldReturn` 0
        choose "resolve its When Revealed" (isLabel $ countLabel "placeThreatOnUnderAttack" 2)
        length <$> handOf p `shouldReturn` 3
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 5
        deck <- getEncounterDeck
        liftIO $ scheme `elem` deck `shouldBe` False

    it "cannot search a side scheme that is only in the encounter discard pile" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- setEncounterDeck [Treacheries.assault] [SideSchemes.crowdControl]
        card <- putInHand p "22015"
        offers (isPlay card) `shouldReturn` False

    it "cannot play into an empty search when the encounter deck has no side schemes" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- setEncounterDeck [] [Treacheries.assault, Treacheries.advance]
        card <- putInHand p "22015"
        offers (isPlay card) `shouldReturn` False
        length <$> handOf p `shouldReturn` 1

    it "has a once-per-round limit shared by its copies and refreshes next round" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        [first] <- stackEncounterDeck [SideSchemes.crowdControl]
        card <- putInHand p "22015"
        playWith card []
        choose "first scheme" (isTargetChoice first)
        clearHand p
        _ <- stackEncounterDeck [SideSchemes.breakinAndTakin]
        second <- putInHand p "22015"
        offers (isPlay second) `shouldReturn` False
        runNow [EndRound]
        pass
        offers (isPlay second) `shouldReturn` True

    it "is a hero action" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [SideSchemes.crowdControl]
        card <- putInHand p "22015"
        offers (isPlay card) `shouldReturn` False

  describe "Brains Over Brawn" do
    it "deals current THW as an attack after your hero makes a basic thwart" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "01065"
        (brains, energy) <- prepareBrains p
        placeMainThreat 4
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        playWith brains [energy]
        chooseVillain
        villainDamageNow `shouldReturn` 3
        isExhaustedNow p `shouldReturn` True

    it "uses THW at resolution rather than remembering the earlier basic-thwart value" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        (brains, energy) <- prepareBrains p
        placeMainThreat 4
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        giveModifiers (IdentityEntity $ IdentityWithId p) [ThwartModifier 1]
        playWith brains [energy]
        chooseVillain
        villainDamageNow `shouldReturn` 3

    it "obeys guard when selecting the target of its attack" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        guard <- addMinion p Minions.hydraMercenary
        (brains, energy) <- prepareBrains p
        placeMainThreat 2
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        playWith brains [energy]
        vid <- villainIdNow
        offers (isTargetChoice vid) `shouldReturn` False
        offers (isTargetChoice guard) `shouldReturn` True
        choose "attack the guard" (isTargetChoice guard)
        villainDamageNow `shouldReturn` 0

    it "a stun replaces the attack after its cost has been paid" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        resolve [GiveStatus (IdentityRef p) StunnedStatus]
        (brains, energy) <- prepareBrains p
        placeMainThreat 2
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        playWith brains [energy]
        isStunned p `shouldReturn` False
        villainDamageNow `shouldReturn` 0
        discard <- discardOf p
        liftIO $ all (`elem` discard) [brains, energy] `shouldBe` True

    it "cannot be played without the basic-thwart response window" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        (brains, _) <- prepareBrains p
        offers (isPlay brains) `shouldReturn` False

    it "does not trigger after a thwart event" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        (brains, _) <- prepareBrains p
        placeMainThreat 4
        _ <- playPaying p "01060" ["01089"]
        chooseMainScheme
        offers (isPlay brains) `shouldReturn` False
        villainDamageNow `shouldReturn` 0

    it "does not trigger after an ally's basic thwart" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        eros <- putIntoPlay p "22011"
        (brains, _) <- prepareBrains p
        placeMainThreat 2
        choose "Eros thwarts" (isBasicAllyThwart $ allyIdOf eros)
        chooseMainScheme
        offers (isPlay brains) `shouldReturn` False
        villainDamageNow `shouldReturn` 0

    it "does not trigger after another player's basic thwart" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        inHero p
        (brains, _) <- prepareBrains p
        placeMainThreat 4
        runNow [IdentityMessage other $ ChangedToForm A, IdentityMessage other $ SideMessage $ BasicThwart 0]
        chooseMainScheme
        offers (isPlay brains) `shouldReturn` False
        villainDamageNow `shouldReturn` 0
