module Marvel.Hero.VisionSpec (spec, changeMass, massForm) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (HasAbilities)
import Marvel.Card.Side (Side (A, B))
import Marvel.Ally.Runner qualified as Ally
import Marvel.AlterEgo.Runner qualified as AlterEgo
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity, getModifiedKeywords)
import Marvel.Keyword (Keyword (Retaliate, Stalwart, Permanent, MassForm))
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Modifier (Modifier (..), HasModifiersFor (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types qualified as Obligation
import Marvel.Query
import Marvel.Trait (Trait (Aerial))
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import TestImport

-- A blanking/permission fixture independent of any encounter-card implementation.
data ModifierProbe = ModifierProbe (Attrs Upgrade) Target [Modifier]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasAbilities)
instance IsUpgrade ModifierProbe where
  toUpgradeAttrs (ModifierProbe attrs _ _) = attrs
instance HasModifiersFor ModifierProbe where
  getModifiersFor _ target (ModifierProbe _ affected modifiers) = pure [m | target == affected, m <- modifiers]
instance RunMessage ModifierProbe where
  runMessage msg (ModifierProbe attrs target modifiers) =
    (\a -> ModifierProbe a target modifiers) <$> runMessage msg attrs

-- Blanking comes from an encounter source, as it does on Corrupted
-- Programming. Player upgrades are intentionally outside isBlanked's providers.
data BlankingProbe = BlankingProbe (Attrs Obligation.Obligation) Target [Modifier]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasAbilities)
instance Obligation.IsObligation BlankingProbe where
  toObligationAttrs (BlankingProbe attrs _ _) = attrs
instance HasModifiersFor BlankingProbe where
  getModifiersFor _ target (BlankingProbe _ affected modifiers) = pure [m | target == affected, m <- modifiers]
instance RunMessage BlankingProbe where
  runMessage msg (BlankingProbe attrs target modifiers) =
    (\a -> BlankingProbe a target modifiers) <$> runMessage msg attrs

withModifiers :: IdentityId -> Target -> [Modifier] -> GameAppT PlayerCard
withModifiers p target modifiers = do
  card <- putIntoPlay p "01008"
  attrs <- getsGame (toAttrs . fromMaybe (error "missing modifier fixture") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
  if Blank `elem` modifiers then do
    oid <- getRandom
    let blanking = Obligation.ObligationAttrs
          { Obligation.obligationId = oid
          , Obligation.obligationCardDef = Obligations.corruptedProgramming
          , Obligation.obligationSurge = False
          , Obligation.obligationHeldCards = []
          , Obligation.obligationOwner = Just p
          }
    withGame_ $ entitiesL . obligationsL . at oid ?~ Obligation.Obligation (BlankingProbe blanking target modifiers)
  else withGame_ $ entitiesL . upgradesL . at (upgradeIdOf card) ?~ Upgrade (ModifierProbe attrs target modifiers)
  pure card

vision :: IO TestGame
vision = testGame ["vision"]

heroStats :: IdentityId -> GameAppT (Natural, Natural, Natural)
heroStats p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> (,,) <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
  _ -> pure (0, 0, 0)

recovery :: IdentityId -> GameAppT Natural
recovery p = currentIdentity <$> getPlayer p >>= \case
  AlterEgoSide a -> AlterEgo.getModifiedRecovery (toAttrs a)
  HeroSide _ -> pure 0

-- Public helpers retained for VisionSignatureSpec and VisionNemesisSpec.
massForm :: GameAppT (Attrs Upgrade)
massForm = do
  front <- upgradesWith "26002"
  back <- upgradesWith "26002b"
  case front <> back of
    [u] -> pure u
    _ -> error "expected one mass form upgrade"

isDensityManipulation :: IdentityId -> Choice -> Bool
isDensityManipulation p = \case
  UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 1 && ChangeForm `notElem` abilityChoices ab
  _ -> False

changeMass :: IdentityId -> GameAppT ()
changeMass p = choose "Density Manipulation" (isDensityManipulation p)

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card index choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == index
  _ -> False

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code costs = do
  card <- putInHand p code
  money <- traverse (putInHand p) costs
  playWith card money
  pass
  pure card

flipDirect :: GameAppT ()
flipDirect = do
  form <- massForm
  runNow [CardSpecific (UpgradeRef $ upgradeId form) "flipMassForm" Null]

becomeDense :: IdentityId -> GameAppT ()
becomeDense p = changeForm p >> changeMass p >> pass

spec :: Spec
spec = describe "Vision" do
  describe "setup and mass form structure" do
    it "starts with the physical Intangible upgrade in play, forty ordinary cards, and the printed pre-Setup five-card hand" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        form <- massForm
        toCardCode form `shouldBe` "26002"
        upgradeOwner form `shouldBe` p
        upgradeController form `shouldBe` p
        handSizeOf p `shouldReturn` 6
        length <$> handOf p `shouldReturn` 5
        length . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` 35
        hand <- handOf p
        discard <- discardOf p
        map toCardCode (hand <> discard) `shouldSatisfy` notElem "26002"
        getSetAsidePlayerCards >>= (\cards -> map toCardCode cards `shouldSatisfy` notElem "26002")

    it "gives the two physical sides distinct definitions with Permanent and MassForm, and a side flip preserves identity, counters and exhaustion" do
      sameCard Upgrades.intangible Upgrades.dense `shouldBe` False :: IO ()
      tg <- vision
      let p = player1 tg
      inGame tg do
        before <- massForm
        cdKeywords (getCardDef before) `shouldSatisfy` (\ks -> Permanent `elem` ks && MassForm `elem` ks)
        resolve [UpgradeMessage (upgradeId before) ExhaustedUpgrade, UpgradeMessage (upgradeId before) $ AddUpgradeUses 3]
        changeForm p
        changeMass p
        pass
        after <- massForm
        upgradeId after `shouldBe` upgradeId before
        upgradeOwner after `shouldBe` upgradeOwner before
        upgradeController after `shouldBe` upgradeController before
        upgradeUses after `shouldBe` 3
        upgradeExhausted after `shouldBe` True
        toCardCode after `shouldBe` "26002b"
        cdKeywords (getCardDef after) `shouldSatisfy` (\ks -> Permanent `elem` ks && MassForm `elem` ks)

    for_ [False, True] $ \dense ->
      it ("the permanent upgrade cannot be discarded or returned to hand; Dense=" <> show dense) do
        tg <- vision
        let p = player1 tg
        inGame tg do
          when dense $ becomeDense p
          form <- massForm
          resolve [RemoveFromPlay $ UpgradeRef $ upgradeId form, ReturnToHand $ UpgradeRef $ upgradeId form]
          upgradeId <$> massForm `shouldReturn` upgradeId form
          discardOf p >>= (\cs -> map toCardCode cs `shouldSatisfy` notElem (toCardCode form))

  describe "Density Manipulation" do
    it "flips to Dense, offers one optional draw after the new side is active, and is limited to once per round" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        [top] <- stackDeck p ["26027"]
        changeMass p
        form <- massForm
        toCardCode form `shouldBe` "26002b"
        heroStats p `shouldReturn` (2, 2, 2)
        handOf p `shouldReturn` []
        choose "Dense's response" (isAbilityOf form)
        handOf p `shouldReturn` [top]
        offers (isDensityManipulation p) `shouldReturn` False
        offers (isAbilityOf form) `shouldReturn` False

    it "can decline Dense's draw without undoing the flip" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        changeMass p
        pass
        length <$> handOf p `shouldReturn` 0
        toCardCode <$> massForm `shouldReturn` "26002b"

    it "does not draw when flipping back to Intangible, and its limit resets in a new round" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        becomeDense p
        clearHand p
        runNow [EndRound]
        pass
        changeMass p
        pass
        toCardCode <$> massForm `shouldReturn` "26002"
        handOf p `shouldReturn` []
        offers (isDensityManipulation p) `shouldReturn` False

    it "is on the hero side only and changing identity form neither changes mass nor resets the round limit" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        offers (isDensityManipulation p) `shouldReturn` False
        becomeDense p
        clearHand p
        resolve [IdentityMessage p $ ChangedToForm B, IdentityMessage p $ ChangedToForm A]
        toCardCode <$> massForm `shouldReturn` "26002b"
        handOf p `shouldReturn` []
        offers (isDensityManipulation p) `shouldReturn` False

    it "does not offer a flip while changing form is prohibited, and does not spend the limit" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        changeForm p
        prohibition <- withModifiers p (IdentityRef p) [CannotChangeForm]
        offers (isDensityManipulation p) `shouldReturn` False
        resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf prohibition]
        offers (isDensityManipulation p) `shouldReturn` True

    it "does not offer a no-effect action if its mass form upgrade is absent" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        changeForm p
        form <- massForm
        withGame_ $ entitiesL . upgradesL . at (upgradeId form) .~ Nothing
        offers (isDensityManipulation p) `shouldReturn` False

  describe "mass form text and identity sides" do
    it "has printed hero stats while Intangible and prohibits basic attacks, attack events, and defending" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        changeForm p
        heroStats p `shouldReturn` (2, 0, 0)
        identityMatches IdentityCanBasicAttack p `shouldReturn` False
        identityMatches IdentityCanAttack p `shouldReturn` False
        identityMatches IdentityCanDefend p `shouldReturn` False
        attack <- putInHand p "01005"
        _ <- putInHand p "26025"
        _ <- putInHand p "26026"
        offers (isPlay attack) `shouldReturn` False
        mid <- addMinion p Minions.hydraSoldier
        runNow [MinionMessage mid $ MinionAttacks p]
        offers (\case Defend {} -> True; _ -> False) `shouldReturn` False
        pass
        damageOf p `shouldReturn` 0

    for_ [False, True] $ \hero ->
      it ("reduces damage from each attack by two in either identity form; hero=" <> show hero) do
        tg <- vision
        let p = player1 tg
        inGame tg do
          when hero $ changeForm p
          mid <- addMinion p Minions.titania
          resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 1 FromAbility]
          runNow [MinionMessage mid $ MinionAttacks p]
          pass
          damageOf p `shouldReturn` 3
          runNow [MinionMessage mid $ MinionAttacks p]
          pass
          damageOf p `shouldReturn` 6

    it "does not reduce damage from non-attack abilities" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p $ IdentityDamaged GameRef $ toDamage 3 FromAbility]
        damageOf p `shouldReturn` 3

    it "allows allies to attack and defend while Vision is Intangible" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        changeForm p
        ally <- putIntoPlay p "01051"
        attrs <- allyIn "01051"
        choose "attack with Tigra" (isAllyAttack attrs)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        resolve [AllyMessage (allyIdOf ally) ReadiedAlly]
        mid <- addMinion p Minions.hydraSoldier
        runNow [MinionMessage mid $ MinionAttacks p]
        choose "Tigra defends" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
        pass
        damageOf p `shouldReturn` 0

    it "Dense grants +2ATK and +2DEF only while a hero; alter-ego gains +2REC instead" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        recovery p `shouldReturn` 3
        handSizeOf p `shouldReturn` 6
        becomeDense p
        heroStats p `shouldReturn` (2, 2, 2)
        handSizeOf p `shouldReturn` 5
        resolve [IdentityMessage p $ ChangedToForm B]
        recovery p `shouldReturn` 5
        handSizeOf p `shouldReturn` 5
        setDamage p 7
        choose "recover with Dense Vision" (\case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False)
        pass
        damageOf p `shouldReturn` 2

    it "Intangible's hand-size modifier belongs to alter-ego only" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        handSizeOf p `shouldReturn` 6
        changeForm p
        handSizeOf p `shouldReturn` 5
        recovery p `shouldReturn` 0

    it "Dense's draw response can resolve in alter-ego when another effect changes mass form" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        clearHand p
        flipDirect
        form <- massForm
        recovery p `shouldReturn` 5
        choose "Dense response in alter-ego" (isAbilityOf form)
        length <$> handOf p `shouldReturn` 1

    it "a blank mass form still flips and remains identifiable, but loses its own modifiers and Dense draw" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        form <- massForm
        _ <- withModifiers p (UpgradeRef $ upgradeId form) [Blank]
        resolve [RemoveFromPlay $ UpgradeRef $ upgradeId form]
        upgradeId <$> massForm `shouldReturn` upgradeId form
        -- Another card still sees Intangible for the alter-ego hand size.
        handSizeOf p `shouldReturn` 6
        changeForm p
        identityMatches IdentityCanAttack p `shouldReturn` True
        identityMatches IdentityCanDefend p `shouldReturn` True
        clearHand p
        changeMass p
        pass
        toCardCode <$> massForm `shouldReturn` "26002b"
        heroStats p `shouldReturn` (2, 0, 0)
        handOf p `shouldReturn` []
        resolve [IdentityMessage p $ ChangedToForm B]
        recovery p `shouldReturn` 5

    it "blanking Intangible removes its attack-damage reduction" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        form <- massForm
        _ <- withModifiers p (UpgradeRef $ upgradeId form) [Blank]
        changeForm p
        mid <- addMinion p Minions.hydraSoldier
        runNow [MinionMessage mid $ MinionAttacks p]
        pass
        damageOf p `shouldReturn` 2

  describe "Vivian" do
    it "gets +2THW in Intangible and +2ATK in Dense, in either identity form" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        _ <- playPaying p "26003" ["26025"]
        a <- allyIn "26003"
        Ally.getModifiedThwart a `shouldReturn` 3
        Ally.getModifiedAttack a `shouldReturn` 1
        becomeDense p
        b <- allyIn "26003"
        Ally.getModifiedThwart b `shouldReturn` 1
        Ally.getModifiedAttack b `shouldReturn` 3
        resolve [IdentityMessage p $ ChangedToForm B]
        c <- allyIn "26003"
        Ally.getModifiedAttack c `shouldReturn` 3

    it "uses the conditional THW in an actual basic thwart and takes one consequential damage" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "26003"
        placeMainThreat 5
        choose "Vivian thwarts" (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf card) && abilityIndex ab == 300; _ -> False)
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` 2
        allyDamage <$> allyIn "26003" `shouldReturn` 1

    it "uses the conditional ATK in an actual attack and takes one consequential damage" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        becomeDense p
        _ <- putIntoPlay p "26003"
        attrs <- allyIn "26003"
        choose "Vivian attacks" (isAllyAttack attrs)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        allyDamage <$> allyIn "26003" `shouldReturn` 1

    it "still reads the side of a blank mass form, while blanking Vivian removes her own bonus" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        form <- massForm
        _ <- withModifiers p (UpgradeRef $ upgradeId form) [Blank]
        card <- putIntoPlay p "26003"
        a <- allyIn "26003"
        Ally.getModifiedThwart a `shouldReturn` 3
        _ <- withModifiers p (AllyRef $ allyIdOf card) [Blank]
        b <- allyIn "26003"
        Ally.getModifiedThwart b `shouldReturn` 1
        Ally.getModifiedAttack b `shouldReturn` 1

    it "follows the controller's mass form, not the owner's or another player's" do
      tg <- testGame ["vision", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        card <- putIntoPlay p "26003"
        resolve [AllyMessage (allyIdOf card) $ AllyChangedController other]
        a <- allyIn "26003"
        Ally.getModifiedThwart a `shouldReturn` 1
        Ally.getModifiedAttack a `shouldReturn` 1

    it "cannot play a second copy of unique Vivian" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "26003"
        card <- putInHand p "26003"
        _ <- putInHand p "26025"
        offers (isPlay card) `shouldReturn` False

  describe "616 Hickory Branch Lane" do
    it "searches the owner's deck and discard for one Android ally, without playing it" do
      tg <- testGame ["vision", "spider-man"]
      let p = player1 tg
      inGame tg do
        home <- playPaying p "26004" ["26025"]
        [deckAlly, nonAndroid] <- stackDeck p ["01068", "01051"]
        [discardAlly, nonAlly] <- stackDiscard p ["26003", "26008"]
        [otherDiscard] <- stackDiscard (player2 tg) ["26003"]
        choose "search" (cardAbility home 1)
        offers (isTargetChoice nonAndroid) `shouldReturn` False
        offers (isTargetChoice nonAlly) `shouldReturn` False
        offers (isTargetChoice otherDiscard) `shouldReturn` False
        offers (isTargetChoice deckAlly) `shouldReturn` True
        choose "return Vivian" (isTargetChoice discardAlly)
        pass
        handOf p >>= (`shouldSatisfy` elem discardAlly)
        discardOf p >>= (`shouldSatisfy` notElem discardAlly)
        alliesWith "26003" `shouldReturn` []
        supportExhausted . head <$> supportsWith "26004" `shouldReturn` True
        offers (cardAbility home 1) `shouldReturn` False

    it "can find an Android ally in the deck even when its title conflicts with an identity in play" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        home <- putIntoPlay p "26004"
        [ally] <- stackDeck p ["01068"]
        choose "search" (cardAbility home 1)
        choose "find the Vision ally" (isTargetChoice ally)
        pass
        handOf p >>= (`shouldSatisfy` elem ally)
        deck <- unDeck . playerIdentityDeck <$> playerAttrs p
        deck `shouldSatisfy` notElem ally

    it "cannot search in hero form, and the printed support remains unique" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        home <- putIntoPlay p "26004"
        changeForm p
        offers (cardAbility home 1) `shouldReturn` False
        duplicate <- putInHand p "26004"
        _ <- putInHand p "26025"
        offers (isPlay duplicate) `shouldReturn` False

    it "requires paying its printed resource cost" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        clearHand p
        home <- putInHand p "26004"
        offers (isPlay home) `shouldReturn` False

    it "handles a failed search without an empty question or changing unrelated cards" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        home <- putIntoPlay p "26004"
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
          PlayerIdentity a {playerIdentityDeck = Deck [], playerIdentityDiscard = Discard []}
        [one, two] <- stackDeck p ["26025", "26026"]
        choose "search an area with no Android allies" (cardAbility home 1)
        chooseIf (isLabel "$label.noMatchingCardsFound")
        pass
        deck <- unDeck . playerIdentityDeck <$> playerAttrs p
        deck `shouldMatchList` [one, two]
        supportExhausted . head <$> supportsWith "26004" `shouldReturn` True

  describe "Solar Gem" do
    for_ [False, True] $ \hero ->
      it ("grants Aerial and generates one wild resource in either identity form; hero=" <> show hero) do
        tg <- vision
        let p = player1 tg
        inGame tg do
          when hero $ changeForm p
          clearHand p
          gem <- playPaying p "26005" ["26025"]
          identityMatches (IdentityWithTrait Aerial) p `shouldReturn` True
          offers (cardAbility gem 1) `shouldReturn` False
          cape <- putInHand p "26006"
          single <- putInHand p "26024"
          playWith cape [single]
          choose "generate a wild resource" (cardAbility gem 1)
          payWith []
          pass
          length <$> upgradesWith "26006" `shouldReturn` 1
          upgradeIsExhausted (upgradeIdOf gem) `shouldReturn` True
          offers (cardAbility gem 1) `shouldReturn` False

    it "can declare its wild as mental to pay an event's typed-resource bonus" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        gem <- putIntoPlay p "26005"
        placeMainThreat 6
        event <- putInHand p "01060"
        single <- putInHand p "26024"
        playWith event [single]
        choose "generate wild" (cardAbility gem 1)
        declareWildAs Mental
        payWith []
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` 2

    it "cannot be used twice while exhausted and does not provide a free second resource" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        clearHand p
        gem <- putIntoPlay p "26005"
        card <- putInHand p "26004"
        playWith card []
        choose "use Gem" (cardAbility gem 1)
        payWith []
        pass
        clearHand p
        other <- putInHand p "26003"
        _ <- putInHand p "26024"
        offers (isPlay other) `shouldReturn` False
        upgradeIsExhausted (upgradeIdOf gem) `shouldReturn` True

    it "loses both Aerial and its resource ability while blank, and is unique" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        gem <- putIntoPlay p "26005"
        _ <- withModifiers p (UpgradeRef $ upgradeIdOf gem) [Blank]
        identityMatches (IdentityWithTrait Aerial) p `shouldReturn` False
        clearHand p
        cape <- putInHand p "26006"
        single <- putInHand p "26024"
        offers (isPlay cape) `shouldReturn` False
        duplicate <- putInHand p "26005"
        _ <- putInHand p "26025"
        offers (isPlay duplicate) `shouldReturn` False
        handOf p >>= (`shouldSatisfy` elem single)

  describe "Vision's Cape" do
    for_ [False, True] $ \hero ->
      it ("Stalwart removes existing stun/confuse and prevents new ones while Intangible; hero=" <> show hero) do
        tg <- vision
        let p = player1 tg
        inGame tg do
          when hero $ changeForm p
          resolve [IdentityMessage p IdentityStunned, IdentityMessage p IdentityConfused]
          _ <- playPaying p "26006" ["26025"]
          getPlayer p >>= getModifiedKeywords >>= (`shouldSatisfy` (\ks -> Stalwart `elem` ks && Retaliate 1 `notElem` ks))
          isStunned p `shouldReturn` False
          isConfused p `shouldReturn` False
          resolve [IdentityMessage p IdentityStunned, IdentityMessage p IdentityConfused]
          isStunned p `shouldReturn` False
          isConfused p `shouldReturn` False

    it "Dense replaces Stalwart with Retaliate1, including after an attack that deals no damage" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        becomeDense p
        _ <- putIntoPlay p "26006"
        getPlayer p >>= getModifiedKeywords >>= (`shouldSatisfy` (\ks -> Retaliate 1 `elem` ks && Stalwart `notElem` ks))
        mid <- addMinion p Minions.hydraSoldier
        runNow [MinionMessage mid $ MinionAttacks p]
        choose "defend with Dense Vision" (\case Defend (EnemyMinionId target) -> target == mid; _ -> False)
        pass
        damageOf p `shouldReturn` 0
        minionDamageOf mid `shouldReturn` Just 1
        resolve [IdentityMessage p IdentityStunned, IdentityMessage p IdentityConfused]
        isStunned p `shouldReturn` True
        isConfused p `shouldReturn` True

    it "keeps reading the side of a blank mass form, but loses its keywords when the Cape is blank" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        cape <- putIntoPlay p "26006"
        form <- massForm
        _ <- withModifiers p (UpgradeRef $ upgradeId form) [Blank]
        getPlayer p >>= getModifiedKeywords >>= (`shouldSatisfy` elem Stalwart)
        _ <- withModifiers p (UpgradeRef $ upgradeIdOf cape) [Blank]
        getPlayer p >>= getModifiedKeywords >>= (`shouldSatisfy` notElem Stalwart)
        resolve [IdentityMessage p IdentityStunned]
        isStunned p `shouldReturn` True

    it "grants retaliate in Dense alter-ego as well as hero form and cannot be played twice" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        cape <- putIntoPlay p "26006"
        flipDirect
        pass
        getPlayer p >>= getModifiedKeywords >>= (`shouldSatisfy` elem (Retaliate 1))
        duplicate <- putInHand p "26006"
        _ <- putInHand p "26025"
        offers (isPlay duplicate) `shouldReturn` False

  describe "Density Control" do
    for_ ["26008", "26009", "26010", "26011", "26012"] $ \eventCode ->
      it ("can discard to recover a Vision event after changing mass form: " <> show eventCode) do
        tg <- vision
        let p = player1 tg
        inGame tg do
          changeForm p
          control <- playPaying p "26007" ["26024"]
          event <- putInDiscard p eventCode
          changeMass p
          choose "discard Density Control" (cardAbility control 1)
          upgradesWith "26007" `shouldReturn` []
          discardOf p >>= (`shouldSatisfy` elem control)
          choose "return the Vision event" (isTargetChoice event)
          pass
          handOf p >>= (`shouldSatisfy` elem event)

    it "can decline without discarding itself or recovering an event" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        changeForm p
        control <- putIntoPlay p "26007"
        event <- putInDiscard p "26008"
        changeMass p
        pass
        map upgradeId <$> upgradesWith "26007" `shouldReturn` [upgradeIdOf control]
        discardOf p >>= (`shouldSatisfy` elem event)

    it "only offers Vision events in its own discard, excluding other heroes, aspects, non-events and other players' cards" do
      tg <- testGame ["vision", "spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        control <- putIntoPlay p "26007"
        [event, otherHero, aspect, upgrade] <- stackDiscard p ["26008", "01005", "26024", "26005"]
        [otherEvent] <- stackDiscard (player2 tg) ["26009"]
        changeMass p
        choose "respond" (cardAbility control 1)
        offers (isTargetChoice otherHero) `shouldReturn` False
        offers (isTargetChoice aspect) `shouldReturn` False
        offers (isTargetChoice upgrade) `shouldReturn` False
        offers (isTargetChoice otherEvent) `shouldReturn` False
        choose "recover Solar Beam" (isTargetChoice event)
        pass
        handOf p >>= (`shouldSatisfy` elem event)

    it "cannot respond without a Vision event in its own discard" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        changeForm p
        control <- putIntoPlay p "26007"
        _ <- stackDiscard p ["01005", "26024", "26005"]
        changeMass p
        offers (cardAbility control 1) `shouldReturn` False
        pass
        map upgradeId <$> upgradesWith "26007" `shouldReturn` [upgradeIdOf control]

    it "has no response while its text box is blank" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        changeForm p
        control <- putIntoPlay p "26007"
        _ <- withModifiers p (UpgradeRef $ upgradeIdOf control) [Blank]
        event <- putInDiscard p "26008"
        changeMass p
        offers (cardAbility control 1) `shouldReturn` False
        pass
        discardOf p >>= (`shouldSatisfy` elem event)
        map upgradeId <$> upgradesWith "26007" `shouldReturn` [upgradeIdOf control]

    it "responds when changing to Intangible too, without requiring the upgrade to be ready" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        becomeDense p
        control <- putIntoPlay p "26007"
        event <- putInDiscard p "26008"
        runNow [UpgradeMessage (upgradeIdOf control) ExhaustedUpgrade, EndRound]
        pass
        changeMass p
        choose "recover on the Intangible change" (cardAbility control 1)
        choose "recover event" (isTargetChoice event)
        pass
        handOf p >>= (`shouldSatisfy` elem event)
        toCardCode <$> massForm `shouldReturn` "26002"

    it "does not respond to an ordinary identity-form change, another player's change, or an alter-ego mass change" do
      tg <- testGame ["vision", "ant-man"]
      let p = player1 tg
      inGame tg do
        control <- putIntoPlay p "26007"
        _ <- putInDiscard p "26008"
        changeForm p
        offers (cardAbility control 1) `shouldReturn` False
        resolve [IdentityMessage (player2 tg) $ ChangedToForm A]
        offers (cardAbility control 1) `shouldReturn` False
        resolve [IdentityMessage p $ ChangedToForm B]
        flipDirect
        offers (cardAbility control 1) `shouldReturn` False
        pass
        map upgradeId <$> upgradesWith "26007" `shouldReturn` [upgradeIdOf control]

    for_ [False, True] $ \drawFirst ->
      it ("two copies can each respond to the same change, alongside Dense's draw; draw first=" <> show drawFirst) do
        tg <- vision
        let p = player1 tg
        inGame tg do
          changeForm p
          first <- putIntoPlay p "26007"
          second <- putIntoPlay p "26007"
          [one, two] <- stackDiscard p ["26008", "26009"]
          changeMass p
          form <- massForm
          when drawFirst $ choose "draw for Dense first" (isAbilityOf form)
          choose "first Density Control" (cardAbility first 1)
          choose "first event" (isTargetChoice one)
          unless drawFirst $ choose "draw for Dense between responses" (isAbilityOf form)
          choose "second Density Control" (cardAbility second 1)
          choose "second event" (isTargetChoice two)
          pass
          hand <- handOf p
          hand `shouldSatisfy` (\cs -> one `elem` cs && two `elem` cs)
          upgradesWith "26007" `shouldReturn` []
