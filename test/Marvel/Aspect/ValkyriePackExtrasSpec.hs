module Marvel.Aspect.ValkyriePackExtrasSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (windowAbility)
import Marvel.Ability.Types (AbilityType (..), HasAbilities (..))
import Marvel.Card.Side (Side (A))
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Matchers.Types hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (Modifier (..), HasModifiersFor)
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Status (statusLabel)
import Marvel.Trait (Trait (Asgard, Avenger, Guardian))
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import Marvel.Window qualified as W
import TestImport

newtype TestModifiers = TestModifiers (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage TestModifiers where
  runMessage msg (TestModifiers a) = TestModifiers <$> runMessage msg a

giveModifiers :: EntityMatcher -> [Modifier] -> GameAppT ()
giveModifiers matcher ms = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (TestModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = ms, effectEnds = Nothing })

-- The response is a checkpoint in the real defeat window; all simultaneous
-- removals must have happened before this question is offered.
data ThwartObserver = ThwartObserver (Attrs Upgrade) [Ability]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasModifiersFor)
instance IsUpgrade ThwartObserver where
  toUpgradeAttrs (ThwartObserver attrs _) = attrs
instance HasAbilities ThwartObserver where
  getAbilities (ThwartObserver _ abilities) = abilities
instance RunMessage ThwartObserver where
  runMessage msg (ThwartObserver attrs abilities) =
    (\a -> ThwartObserver a abilities) <$> runMessage msg attrs

observeDefeat :: IdentityId -> GameAppT PlayerCard
observeDefeat p = do
  card <- putIntoPlay p "01008"
  attrs <- getsGame (toAttrs . fromMaybe (error "missing observer") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
  let ab = windowAbility attrs 1 (W.SideSchemeDefeated W.When AnySideScheme) Interrupt NoCost (Run [])
  withGame_ $ entitiesL . upgradesL . at (upgradeIdOf card) ?~ Upgrade (ThwartObserver attrs [ab])
  pure card

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card index choice = isAbilityOfCard card choice && case choice of
  UseAbility a -> abilityIndex a == index
  _ -> False

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code costs = do
  card <- putInHand p code
  money <- traverse (putInHand p) costs
  playWith card money
  pure card

finishOwnProblemCosts :: IdentityId -> GameAppT ()
finishOwnProblemCosts p = do
  chooseIf (isTargetChoice p)
  guardians <- alliesWith "25014"
  for_ [a | a <- guardians, allyController a == p] $ \a -> chooseIf (isTargetChoice $ allyId a)

payFour :: IdentityId -> PlayerCard -> GameAppT ()
payFour p card = do
  energy <- putInHand p "25025"
  genius <- putInHand p "25026"
  playWith card [energy, genius]
  finishOwnProblemCosts p

readyCosmicTargets :: IdentityId -> PlayerCard -> GameAppT ()
readyCosmicTargets p ally = do
  chooseIf (isTargetChoice p)
  chooseIf (isTargetChoice $ allyIdOf ally)
  pass

spec :: Spec
spec = describe "Valkyrie pack extra aspect cards" do
  describe "Godlike Stamina" do
    for_ [False, True] $ \hero ->
      it ("heals two damage from its Asgard identity in either form; hero=" <> show hero) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          when hero $ changeForm p
          setDamage p 4
          card <- putInHand p "25024"
          playWith card []
          pass
          damageOf p `shouldReturn` 2
          discardOf p >>= (`shouldSatisfy` elem card)

    it "heals only the remaining one damage and does not open an empty status question" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        setDamage p 1
        card <- putInHand p "25024"
        playWith card []
        pass
        damageOf p `shouldReturn` 0
        offers (isLabel "$label.done") `shouldReturn` False

    for_ [StunnedStatus, ConfusedStatus, ToughStatus] $ \status ->
      it ("may discard exactly the chosen status after healing: " <> show status) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          setDamage p 3
          resolve [GiveStatus (IdentityRef p) s | s <- [StunnedStatus, ConfusedStatus, ToughStatus]]
          card <- putInHand p "25024"
          playWith card []
          damageOf p `shouldReturn` 1
          choose "discard a status" (isLabel $ statusLabel status)
          pass
          isStunned p `shouldReturn` (status /= StunnedStatus)
          isConfused p `shouldReturn` (status /= ConfusedStatus)
          playerIdentityTough <$> playerAttrs p `shouldReturn` (status /= ToughStatus)

    it "can decline optional status removal" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        setDamage p 3
        resolve [GiveStatus (IdentityRef p) StunnedStatus]
        card <- putInHand p "25024"
        playWith card []
        choose "keep the status" (isLabel "$label.done")
        pass
        damageOf p `shouldReturn` 1
        isStunned p `shouldReturn` True

    it "can remove a status at full health but cannot be played with neither damage nor status" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        card <- putInHand p "25024"
        offers (isPlay card) `shouldReturn` False
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        playWith card []
        choose "discard tough" (isLabel $ statusLabel ToughStatus)
        pass
        playerIdentityTough <$> playerAttrs p `shouldReturn` False

    it "cannot be played by a non-Asgard identity even when it is damaged and stunned" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        setDamage p 3
        resolve [GiveStatus (IdentityRef p) StunnedStatus]
        card <- putInHand p "25024"
        offers (isPlay card) `shouldReturn` False

    it "checks the current form's traits rather than traits on a different identity side" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        giveModifiers (IdentityEntity $ IdentityWithId p <> HeroIdentity) [TraitModifier Asgard]
        setDamage p 4
        card <- putInHand p "25024"
        offers (isPlay card) `shouldReturn` False
        changeForm p
        playWith card []
        pass
        damageOf p `shouldReturn` 2

    it "heals and removes statuses only from the resolving identity" do
      tg <- testGame ["valkyrie", "thor"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        setDamage p 3
        setDamage other 4
        resolve [GiveStatus (IdentityRef other) ConfusedStatus]
        card <- putInHand p "25024"
        playWith card []
        pass
        damageOf p `shouldReturn` 1
        damageOf other `shouldReturn` 4
        isConfused other `shouldReturn` True

  describe "Problem Solvers" do
    it "a confused hero can pay all character and resource costs with no threat and zero combined THW" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        throg <- putIntoPlay p "25014"
        giveModifiers (IdentityEntity $ IdentityWithId p) [ThwartPenalty 1]
        giveModifiers (AllyEntity $ AllyWithId $ allyIdOf throg) [ThwartPenalty 1]
        resolve [IdentityMessage p IdentityConfused]
        mainSchemeThreatNow `shouldReturn` 0
        card <- putInHand p "25033"
        payFour p card
        pass
        isConfused p `shouldReturn` False
        exhaustedNow p `shouldReturn` True
        allyExhausted <$> allyIn "25014" `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` 0
        discardOf p >>= (`shouldSatisfy` elem card)

    it "exhausts distinct Avenger and Guardian characters, sums their modified THW, and removes that much from every scheme" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        guardian <- putIntoPlay p "25014"
        giveModifiers (AllyEntity $ AllyWithId $ allyIdOf guardian) [ThwartModifier 2]
        first <- revealNow p Schemes.bombScare
        pass
        second <- revealNow p Schemes.breakinAndTakin
        pass
        placeMainThreat 6
        card <- putInHand p "25033"
        payFour p card
        pass
        mainSchemeThreatNow `shouldReturn` 2
        sideSchemeThreat (sideSchemeOf first) `shouldReturn` 0
        sideSchemeThreat (sideSchemeOf second) `shouldReturn` 0
        exhaustedNow p `shouldReturn` True
        allyExhausted <$> allyIn "25014" `shouldReturn` True
        allyDamage <$> allyIn "25014" `shouldReturn` 0

    it "removes threat simultaneously, before any side scheme's defeat interrupt resolves" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        throg <- putIntoPlay p "25014"
        giveModifiers (AllyEntity $ AllyWithId $ allyIdOf throg) [ThwartModifier 2]
        observer <- observeDefeat p
        side <- revealNow p Schemes.bombScare
        pass
        placeMainThreat 6
        card <- putInHand p "25033"
        payFour p card
        offers (cardAbility observer 1) `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` 2
        sideSchemeThreat (sideSchemeOf side) `shouldReturn` 0
        choose "finish the defeat checkpoint" (cardAbility observer 1)
        pass

    it "accepts resources another player voluntarily contributes, while the original player resolves the thwart" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "25014"
        clearHand p
        clearHand other
        placeMainThreat 8
        card <- putInHand p "25033"
        energy <- putInHand other "25025"
        genius <- putInHand other "25026"
        choose "play Problem Solvers" (isPlay card)
        choose "invite resource help" (isTargetChoice other)
        fmap fst <$> asked `shouldReturn` Just other
        choose "contribute Energy" (isPayWith energy)
        choose "invite help again" (isTargetChoice other)
        choose "contribute Genius" (isPayWith genius)
        finishOwnProblemCosts p
        pass
        mainSchemeThreatNow `shouldReturn` 6
        exhaustedNow p `shouldReturn` True
        discardOf p >>= (`shouldSatisfy` elem card)
        discardOf other >>= (`shouldSatisfy` (\cs -> energy `elem` cs && genius `elem` cs))

    it "asks each other controller to consent before exhausting contributed characters" do
      tg <- testGame ["spider-man", "captain-marvel", "rocket-raccoon"]
      let p = player1 tg
          avenger = player2 tg
          guardian = tg.seats !! 2
      inGame tg do
        resolve [IdentityMessage i (ChangedToForm A) | i <- [p, avenger, guardian]]
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- putIntoPlay avenger "01068"
        placeMainThreat 10
        card <- putInHand p "25033"
        payFour p card
        fmap fst <$> asked `shouldReturn` Just avenger
        choose "approve Avenger contribution" (isLabel "$label.contributeToCost")
        fmap fst <$> asked `shouldReturn` Just guardian
        choose "approve Guardian contribution" (isLabel "$label.contributeToCost")
        choose "choose Captain Marvel" (isTargetChoice avenger)
        chooseIf (isTargetChoice guardian)
        pass
        exhaustedNow avenger `shouldReturn` True
        exhaustedNow guardian `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` 6

    it "refunds the entire cost if a necessary character contributor refuses" do
      tg <- testGame ["spider-man", "captain-marvel", "rocket-raccoon"]
      let p = player1 tg
          avenger = player2 tg
          guardian = tg.seats !! 2
      inGame tg do
        resolve [IdentityMessage i (ChangedToForm A) | i <- [p, avenger, guardian]]
        resolve [IdentityMessage p ExhaustedIdentity]
        clearHand p
        placeMainThreat 8
        card <- putInHand p "25033"
        energy <- putInHand p "25025"
        genius <- putInHand p "25026"
        playWith card [energy, genius]
        choose "Avenger agrees" (isLabel "$label.contributeToCost")
        choose "Guardian refuses" (isLabel "$label.declineContribution")
        pass
        mainSchemeThreatNow `shouldReturn` 8
        exhaustedNow avenger `shouldReturn` False
        exhaustedNow guardian `shouldReturn` False
        handOf p >>= (`shouldSatisfy` (\cs -> all (`elem` cs) [card, energy, genius]))

    it "cannot exhaust the same dual-trait character for both costs" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "01068"
        resolve [IdentityMessage p ExhaustedIdentity]
        placeMainThreat 4
        card <- putInHand p "25033"
        _ <- putInHand p "25025"
        _ <- putInHand p "25026"
        offers (isPlay card) `shouldReturn` False

    for_ ["alter ego", "no resources", "missing guardian", "exhausted guardian", "no threat"] $ \condition ->
      it ("cannot play with " <> show condition) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          clearHand p
          unless (condition == "alter ego") $ changeForm p
          when (condition == "alter ego") $ giveModifiers (IdentityEntity $ IdentityWithId p) [TraitModifier Avenger]
          unless (condition == "missing guardian") do
            throg <- putIntoPlay p "25014"
            when (condition == "exhausted guardian") $ resolve [AllyMessage (allyIdOf throg) ExhaustedAlly]
          unless (condition == "no threat") $ placeMainThreat 4
          card <- putInHand p "25033"
          unless (condition == "no resources") do
            void $ putInHand p "25025"
            void $ putInHand p "25026"
          offers (isPlay card) `shouldReturn` False

    it "cannot initiate a zero-THW thwart when neither selected trait can contribute threat removal" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        throg <- putIntoPlay p "25014"
        giveModifiers (IdentityEntity $ IdentityWithId p) [ThwartPenalty 1]
        giveModifiers (AllyEntity $ AllyWithId $ allyIdOf throg) [ThwartPenalty 1]
        placeMainThreat 4
        card <- putInHand p "25033"
        _ <- putInHand p "25025"
        _ <- putInHand p "25026"
        offers (isPlay card) `shouldReturn` False

    it "confuse replaces the thwart after all resource and character costs are paid" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "25014"
        placeMainThreat 5
        resolve [GiveStatus (IdentityRef p) ConfusedStatus]
        card <- putInHand p "25033"
        payFour p card
        pass
        isConfused p `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` 5
        exhaustedNow p `shouldReturn` True
        allyExhausted <$> allyIn "25014" `shouldReturn` True
        discardOf p >>= (`shouldSatisfy` elem card)

  describe "Leadership Training" do
    it "enters with two training counters and shuffles a chosen Leadership event from its own discard into its deck" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        training <- putIntoPlay p "25034"
        supportUses . head <$> supportsWith "25034" `shouldReturn` 2
        [chosen, otherLeadership, aggression, support] <- stackDiscard p ["01070", "03017", "25018", "25034"]
        [foreignEvent] <- stackDiscard other ["01070"]
        choose "use Leadership Training" (cardAbility training 1)
        offers (isTargetChoice aggression) `shouldReturn` False
        offers (isTargetChoice support) `shouldReturn` False
        offers (isTargetChoice foreignEvent) `shouldReturn` False
        choose "shuffle the chosen event" (isTargetChoice chosen)
        pass
        supportUses . head <$> supportsWith "25034" `shouldReturn` 1
        supportExhausted . head <$> supportsWith "25034" `shouldReturn` True
        unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` elem chosen)
        discardOf p >>= (`shouldSatisfy` (\cs -> otherLeadership `elem` cs && chosen `notElem` cs))
        discardOf other >>= (`shouldSatisfy` elem foreignEvent)
        offers (cardAbility training 1) `shouldReturn` False

    it "discards itself when spending its final training counter and still resolves that final shuffle" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        training <- putIntoPlay p "25034"
        [first, second] <- stackDiscard p ["01070", "03017"]
        choose "first use" (cardAbility training 1)
        choose "first event" (isTargetChoice first)
        resolve [SupportMessage (supportIdOf training) ReadiedSupport]
        choose "last use" (cardAbility training 1)
        choose "second event" (isTargetChoice second)
        pass
        supportsWith "25034" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem training)
        deck <- unDeck . playerIdentityDeck <$> playerAttrs p
        deck `shouldSatisfy` (\cs -> first `elem` cs && second `elem` cs)

    it "is an alter-ego action and cannot be used in hero form" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        training <- putIntoPlay p "25034"
        _ <- stackDiscard p ["01070"]
        changeForm p
        offers (cardAbility training 1) `shouldReturn` False
        supportUses . head <$> supportsWith "25034" `shouldReturn` 2

    it "cannot spend a counter without a Leadership event in its own discard" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        training <- putIntoPlay p "25034"
        _ <- stackDiscard p ["25018", "25034"]
        _ <- stackDiscard (player2 tg) ["01070"]
        offers (cardAbility training 1) `shouldReturn` False
        supportUses . head <$> supportsWith "25034" `shouldReturn` 2

  describe "Anticipation" do
    it "discards itself to ready the hero in the engagement interrupt, before quickstrike" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        anticipation <- putIntoPlay p "25035"
        resolve [IdentityMessage p ExhaustedIdentity]
        vulture <- revealNow p Minions.vulture
        choose "anticipate engagement" (cardAbility anticipation 1)
        exhaustedNow p `shouldReturn` False
        upgradesWith "25035" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem anticipation)
        damageOf p `shouldReturn` 0
        choose "defend quickstrike" $ \case Defend (EnemyMinionId m) -> m == minionOf vulture; _ -> False
        pass
        damageOf p `shouldReturn` 2
        exhaustedNow p `shouldReturn` True

    it "can be declined without discarding the upgrade or readying the hero" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        anticipation <- putIntoPlay p "25035"
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow p Minions.hydraSoldier
        offers (cardAbility anticipation 1) `shouldReturn` True
        pass
        exhaustedNow p `shouldReturn` True
        map upgradeId <$> upgradesWith "25035" `shouldReturn` [upgradeIdOf anticipation]

    for_ ["alter ego", "already ready", "cannot ready", "player effects cannot ready"] $ \condition ->
      it ("is unavailable when " <> show condition) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          unless (condition == "alter ego") $ changeForm p
          anticipation <- putIntoPlay p "25035"
          unless (condition == "already ready") $ resolve [IdentityMessage p ExhaustedIdentity]
          when (condition == "cannot ready") $ giveModifiers (IdentityEntity $ IdentityWithId p) [CannotReady]
          when (condition == "player effects cannot ready") $ giveModifiers (IdentityEntity $ IdentityWithId p) [CannotBeReadiedByPlayerCards]
          _ <- revealNow p Minions.hydraSoldier
          offers (cardAbility anticipation 1) `shouldReturn` False
          pass
          upgradesWith "25035" >>= (`shouldSatisfy` not . null)

    it "does not trigger for another player's engagement" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        anticipation <- putIntoPlay p "25035"
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow (player2 tg) Minions.hydraSoldier
        offers (cardAbility anticipation 1) `shouldReturn` False
        pass
        exhaustedNow p `shouldReturn` True

    it "can interrupt an existing minion engaging the controller, not just a minion being revealed" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        mid <- addMinion other Minions.hydraSoldier
        changeForm p
        anticipation <- putIntoPlay p "25035"
        resolve [IdentityMessage p ExhaustedIdentity]
        runNow (engageMinion mid p)
        choose "anticipate the transferred minion" (cardAbility anticipation 1)
        pass
        exhaustedNow p `shouldReturn` False
        minionEngagedIdentity . toAttrs . fromMaybe (error "missing minion") . HashMap.lookup mid <$> getsGame gameMinions `shouldReturn` p

    it "allows only one Anticipation per player" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "25035"
        card <- putInHand p "25035"
        _ <- putInHand p "25025"
        offers (isPlay card) `shouldReturn` False

  describe "Cosmic Alliance" do
    it "chooses two distinct exhausted trait-matching characters and readies both without exhaustion costs" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        throg <- putIntoPlay p "25014"
        resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf throg) ExhaustedAlly]
        card <- playPaying p "25036" ["25025", "25024"]
        readyCosmicTargets p throg
        exhaustedNow p `shouldReturn` False
        allyExhausted <$> allyIn "25014" `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` elem card)

    it "chooses other players' characters without asking them to contribute the choose-only cost" do
      tg <- testGame ["ms-marvel", "captain-marvel", "rocket-raccoon"]
      let p = player1 tg
          avenger = player2 tg
          guardian = tg.seats !! 2
      inGame tg do
        resolve [IdentityMessage i (ChangedToForm A) | i <- [p, avenger, guardian]]
        vision <- putIntoPlay avenger "01068"
        resolve ([IdentityMessage i ExhaustedIdentity | i <- [avenger, guardian]] <> [AllyMessage (allyIdOf vision) ExhaustedAlly])
        card <- playPaying p "25036" ["25025", "25024"]
        fmap fst <$> asked `shouldReturn` Just p
        offers (isLabel "$label.contributeToCost") `shouldReturn` False
        choose "choose the other player's Avenger" (isTargetChoice avenger)
        chooseIf (isTargetChoice guardian)
        pass
        exhaustedNow avenger `shouldReturn` False
        exhaustedNow guardian `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` elem card)

    it "resource contributors consent and select their own payment cards" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        changeForm p
        throg <- putIntoPlay p "25014"
        resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf throg) ExhaustedAlly]
        clearHand p
        clearHand other
        card <- putInHand p "25036"
        money <- putInHand other "25025"
        lastMoney <- putInHand other "25024"
        choose "play Cosmic Alliance" (isPlay card)
        choose "invite help" (isTargetChoice other)
        fmap fst <$> asked `shouldReturn` Just other
        choose "contribute two resources" (isPayWith money)
        choose "invite the remaining resource" (isTargetChoice other)
        choose "contribute one resource" (isPayWith lastMoney)
        readyCosmicTargets p throg
        exhaustedNow p `shouldReturn` False
        allyExhausted <$> allyIn "25014" `shouldReturn` False
        discardOf other >>= (`shouldSatisfy` (\cs -> money `elem` cs && lastMoney `elem` cs))

    it "a resource contributor can decline without spending anything or readying either target" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        changeForm p
        throg <- putIntoPlay p "25014"
        resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf throg) ExhaustedAlly]
        clearHand p
        clearHand other
        card <- putInHand p "25036"
        money <- putInHand other "25025"
        _ <- putInHand other "25024"
        choose "play" (isPlay card)
        choose "invite contribution" (isTargetChoice other)
        choose "decline resources" (isLabel "$label.doneContributing")
        handOf other >>= (`shouldSatisfy` elem money)
        exhaustedNow p `shouldReturn` True
        allyExhausted <$> allyIn "25014" `shouldReturn` True
        fmap fst <$> asked `shouldReturn` Just p

    it "cannot use a single dual-trait character to pay both choices" do
      tg <- testGame ["ms-marvel"]
      let p = player1 tg
      inGame tg do
        changeForm p
        giveModifiers (IdentityEntity $ IdentityWithId p) [TraitModifier Avenger, TraitModifier Guardian]
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- putInHand p "25036"
        _ <- putInHand p "25025"
        _ <- putInHand p "25024"
        offers (isPlay card) `shouldReturn` False

    for_ ["alter ego", "no resources", "missing guardian", "both ready", "ready Avenger", "ready Guardian", "both cannot ready"] $ \condition ->
      it ("cannot play with " <> show condition) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          clearHand p
          unless (condition == "alter ego") $ changeForm p
          when (condition == "alter ego") $ giveModifiers (IdentityEntity $ IdentityWithId p) [TraitModifier Avenger]
          unless (condition `elem` ["both ready", "ready Avenger"]) $ resolve [IdentityMessage p ExhaustedIdentity]
          unless (condition == "missing guardian") do
            throg <- putIntoPlay p "25014"
            unless (condition `elem` ["both ready", "ready Guardian"]) $ resolve [AllyMessage (allyIdOf throg) ExhaustedAlly]
            when (condition == "both cannot ready") $ giveModifiers (AllyEntity $ AllyWithId $ allyIdOf throg) [CannotReady]
          when (condition == "both cannot ready") $ giveModifiers (IdentityEntity $ IdentityWithId p) [CannotReady]
          card <- putInHand p "25036"
          unless (condition == "no resources") do
            void $ putInHand p "25025"
            void $ putInHand p "25024"
          offers (isPlay card) `shouldReturn` False

    it "cannot choose a hero that is forbidden from readying, even with an otherwise legal exhausted Guardian" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        throg <- putIntoPlay p "25014"
        resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf throg) ExhaustedAlly]
        giveModifiers (IdentityEntity $ IdentityWithId p) [CannotReady]
        card <- putInHand p "25036"
        _ <- putInHand p "25025"
        _ <- putInHand p "25024"
        offers (isPlay card) `shouldReturn` False
        exhaustedNow p `shouldReturn` True
        allyExhausted <$> allyIn "25014" `shouldReturn` True

    it "cannot choose an ally forbidden from being readied by player cards, even with an otherwise legal exhausted Avenger" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        throg <- putIntoPlay p "25014"
        resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf throg) ExhaustedAlly]
        giveModifiers (AllyEntity $ AllyWithId $ allyIdOf throg) [CannotBeReadiedByPlayerCards]
        card <- putInHand p "25036"
        _ <- putInHand p "25025"
        _ <- putInHand p "25024"
        offers (isPlay card) `shouldReturn` False
        exhaustedNow p `shouldReturn` True
        allyExhausted <$> allyIn "25014" `shouldReturn` True
