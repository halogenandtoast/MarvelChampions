module Marvel.Aspect.ValkyriePackSpec (spec) where

import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Hp (HP (..))
import Marvel.Stats (Atk (..), Thw (..))
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Matchers.Types hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (Modifier (..))
import Marvel.Query (selectList)
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

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card index choice = isAbilityOfCard card choice && case choice of
  UseAbility a -> abilityIndex a == index
  _ -> False

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code costs = do
  card <- putInHand p code
  money <- traverse (putInHand p) costs
  playWith card money
  pass
  pure card

markEnemy :: IdentityId -> MinionId -> GameAppT ()
markEnemy p mid = do
  glow <- putIntoPlay p "25002"
  resolve [UpgradeMessage (upgradeIdOf glow) $ UpgradeAttachedToEnemy $ EnemyMinionId mid]

heroDefense :: MinionId -> Choice -> Bool
heroDefense mid = \case Defend (EnemyMinionId target) -> mid == target; _ -> False

startThorAttack :: PlayerCard -> MinionId -> GameAppT ()
startThorAttack _ mid = do
  attrs <- allyIn "25013"
  choose "attack with Thor" (isAllyAttack attrs)
  choose "initial attack target" (isTargetChoice mid)

zeroAttackVision :: IdentityId -> GameAppT ()
zeroAttackVision p = do
  changeForm p
  choose "change Vision to Dense" (\case
    UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1
    _ -> False)
  pass
  giveModifiers (IdentityEntity $ IdentityWithId p) [AttackPenalty 2]

spec :: Spec
spec = describe "Valkyrie pack main aspect cards" do
  describe "Thor, Odinson" do
    it "has Toughness on entry and the printed basic powers" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "25013"
        thor <- allyIn "25013"
        allyTough thor `shouldBe` True
        allyAttack thor `shouldBe` Atk 3
        allyThwart thor `shouldBe` Thw 1
        allyHitPoints thor `shouldBe` HP 3

    it "spends one energy for one attack against all of the chosen player's minions, in chosen order, and one consequential damage" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        thor <- putIntoPlay p "25013"
        resolve [RemoveStatus (AllyRef $ allyIdOf thor) ToughStatus]
        first <- addMinion other Minions.hydraSoldier
        second <- addMinion other Minions.hydraSoldier
        excluded <- addMinion p Minions.hydraSoldier
        money <- putInHand p "25025"
        startThorAttack thor first
        choose "use Thor's interrupt" (cardAbility thor 1)
        payWith [money]
        choose "resolve the second target first" (isTargetChoice second)
        minionDamageOf second `shouldReturn` Just 3
        minionDamageOf first `shouldReturn` Just 0
        offers (cardAbility thor 1) `shouldReturn` False
        choose "then the first target" (isTargetChoice first)
        pass
        minionDamageOf first `shouldReturn` Just 3
        minionDamageOf excluded `shouldReturn` Just 0
        allyDamage <$> allyIn "25013" `shouldReturn` 1
        allyExhausted <$> allyIn "25013" `shouldReturn` True
        discardOf p >>= (`shouldSatisfy` elem money)

    it "declining the optional interrupt resolves only the original target" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        thor <- putIntoPlay p "25013"
        first <- addMinion p Minions.hydraSoldier
        second <- addMinion p Minions.hydraSoldier
        _ <- putInHand p "25025"
        startThorAttack thor first
        offers (cardAbility thor 1) `shouldReturn` True
        pass
        minionDamageOf first `shouldReturn` Just 3
        minionDamageOf second `shouldReturn` Just 0
        -- Tough prevents the one consequential damage.
        allyTough <$> allyIn "25013" `shouldReturn` False
        allyDamage <$> allyIn "25013" `shouldReturn` 0

    for_ [[], ["25026"], ["25027"]] $ \resources ->
      it ("cannot use the interrupt without an energy resource: " <> show resources) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          clearHand p
          thor <- putIntoPlay p "25013"
          mid <- addMinion p Minions.hydraSoldier
          _ <- traverse (putInHand p) resources
          startThorAttack thor mid
          offers (cardAbility thor 1) `shouldReturn` False
          pass
          minionDamageOf mid `shouldReturn` Just 3

    it "does not offer the interrupt against a villain" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        thor <- putIntoPlay p "25013"
        _ <- putInHand p "25025"
        attrs <- allyIn "25013"
        vid <- villainIdNow
        choose "attack with Thor" (isAllyAttack attrs)
        choose "attack Rhino" (isTargetChoice vid)
        offers (cardAbility thor 1) `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 3

    it "damages every target before a surviving enemy's lethal retaliate defeats Thor" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        thor <- putIntoPlay p "25013"
        resolve [RemoveStatus (AllyRef $ allyIdOf thor) ToughStatus]
        resolve [AllyMessage (allyIdOf thor) $ AllySetDamage 1]
        first <- addMinion p Minions.modok
        second <- addMinion p Minions.hydraSoldier
        money <- putInHand p "25025"
        startThorAttack thor second
        choose "use Thor's interrupt" (cardAbility thor 1)
        payWith [money]
        choose "resolve the lethal retaliating enemy first" (isTargetChoice first)
        minionDamageOf first `shouldReturn` Just 3
        minionDamageOf second `shouldReturn` Just 0
        choose "resolve the other target before retaliation" (isTargetChoice second)
        pass
        alliesWith "25013" `shouldReturn` []
        minionDamageOf first `shouldReturn` Just 3
        minionDamageOf second `shouldReturn` Just 3

    it "cannot play a second Thor while Odinson is the identity in play" do
      tg <- testGame ["thor"]
      let p = player1 tg
      inGame tg do
        card <- putInHand p "25013"
        _ <- putInHand p "25025"
        _ <- putInHand p "25026"
        offers (isPlay card) `shouldReturn` False

  describe "Throg, Puddlegulp" do
    it "optionally gains tough after being played with a minion engaged with its controller" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.hydraSoldier
        card <- putInHand p "25014"
        money <- putInHand p "25025"
        playWith card [money]
        allyTough <$> allyIn "25014" `shouldReturn` False
        choose "give Throg tough" (cardAbility card 1)
        pass
        allyTough <$> allyIn "25014" `shouldReturn` True
        offers (cardAbility card 1) `shouldReturn` False

    it "the entry response can be declined" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.hydraSoldier
        card <- putInHand p "25014"
        money <- putInHand p "25025"
        playWith card [money]
        offers (cardAbility card 1) `shouldReturn` True
        pass
        allyTough <$> allyIn "25014" `shouldReturn` False

    it "also triggers when put into play instead of played" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.hydraSoldier
        card <- putInHand p "25014"
        runNow [PutCardIntoPlay p card NoPayment Nothing False]
        choose "Throg responds to entering without being played" (cardAbility card 1)
        pass
        allyTough <$> allyIn "25014" `shouldReturn` True

    it "another player's engaged minion does not satisfy the condition" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion (player2 tg) Minions.hydraSoldier
        card <- putInHand p "25014"
        money <- putInHand p "25025"
        playWith card [money]
        offers (cardAbility card 1) `shouldReturn` False
        pass
        allyTough <$> allyIn "25014" `shouldReturn` False

    it "does not gain tough if no minion is engaged with its controller" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "25014"
        allyTough <$> allyIn "25014" `shouldReturn` False

    it "cannot play a second copy of the same unique Throg" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "25014"
        card <- putInHand p "25014"
        _ <- putInHand p "25025"
        offers (isPlay card) `shouldReturn` False

  describe "Quick Strike" do
    for_ [Schemes.theRedHouse, Schemes.sanctuary] $ \immunity -> do
      it ("cannot pay for an attack whose only target cannot take its damage: " <> show (toCardCode immunity)) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          changeForm p
          _ <- revealNow p immunity
          pass
          clearHand p
          card <- putInHand p "25018"
          money <- putInHand p "25025"
          offers (isPlay card) `shouldReturn` False
          handOf p >>= (`shouldSatisfy` (\cs -> card `elem` cs && money `elem` cs))

      it ("excludes the immune enemy while allowing another positive-damage target: " <> show (toCardCode immunity)) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          changeForm p
          _ <- revealNow p immunity
          pass
          target <- addMinion p Minions.modok
          card <- putInHand p "25018"
          money <- putInHand p "25025"
          playWith card [money]
          vid <- villainIdNow
          offers (isTargetChoice vid) `shouldReturn` False
          choose "attack the enemy that can take damage" (isTargetChoice target)
          pass
          minionDamageOf target `shouldReturn` Just 2
          villainDamageNow `shouldReturn` 0

      it ("a stunned hero can still pay to remove stun despite the only enemy's immunity: " <> show (toCardCode immunity)) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          changeForm p
          _ <- revealNow p immunity
          pass
          resolve [IdentityMessage p IdentityStunned]
          card <- putInHand p "25018"
          money <- putInHand p "25025"
          playWith card [money]
          pass
          isStunned p `shouldReturn` False
          villainDamageNow `shouldReturn` 0
          discardOf p >>= (`shouldSatisfy` elem card)

    it "cannot charge resources for a zero-ATK attack even when a legal enemy exists" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        zeroAttackVision p
        clearHand p
        card <- putInHand p "25018"
        money <- putInHand p "25025"
        offers (isPlay card) `shouldReturn` False
        handOf p >>= (`shouldSatisfy` (\cs -> card `elem` cs && money `elem` cs))
        exhaustedNow p `shouldReturn` False

    it "a stunned zero-ATK hero can pay to attempt the attack and remove stun" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        zeroAttackVision p
        clearHand p
        resolve [IdentityMessage p IdentityStunned]
        card <- putInHand p "25018"
        money <- putInHand p "25025"
        playWith card [money]
        pass
        isStunned p `shouldReturn` False
        villainDamageNow `shouldReturn` 0
        discardOf p >>= (`shouldSatisfy` (\cs -> card `elem` cs && money `elem` cs))

    it "allows a positive marked-target ATK even when ordinary ATK is zero and excludes zero-ATK enemy choices" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "25006"
        giveModifiers (IdentityEntity $ IdentityWithId p) [AttackPenalty 3]
        marked <- addMinion p Minions.modok
        other <- addMinion p Minions.hydraSoldier
        markEnemy p marked
        card <- putInHand p "25018"
        money <- putInHand p "25025"
        playWith card [money]
        vid <- villainIdNow
        offers (isTargetChoice vid) `shouldReturn` False
        offers (isTargetChoice other) `shouldReturn` False
        choose "attack the only positive-ATK target" (isTargetChoice marked)
        pass
        minionDamageOf marked `shouldReturn` Just 1
        minionDamageOf other `shouldReturn` Just 0

    for_ [(False, 3), (True, 4)] $ \(marked, expected) ->
      it ("uses target-specific modified ATK with Dragonfang; marked=" <> show marked) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          changeForm p
          _ <- putIntoPlay p "25006"
          mid <- addMinion p Minions.modok
          when marked $ markEnemy p mid
          card <- putInHand p "25018"
          money <- putInHand p "25025"
          playWith card [money]
          choose "attack the minion" (isTargetChoice mid)
          pass
          minionDamageOf mid `shouldReturn` Just expected
          exhaustedNow p `shouldReturn` False

    it "a separate enemy bearing Death-Glow does not grant Dragonfang's conditional bonus" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "25006"
        marked <- addMinion p Minions.hydraSoldier
        target <- addMinion p Minions.hydraSoldier
        markEnemy p marked
        card <- putInHand p "25018"
        money <- putInHand p "25025"
        playWith card [money]
        choose "attack the unmarked minion" (isTargetChoice target)
        pass
        minionDamageOf target `shouldReturn` Just 3
        minionDamageOf marked `shouldReturn` Just 0

    it "can attack the villain while the hero is exhausted" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- putInHand p "25018"
        money <- putInHand p "25025"
        playWith card [money]
        vid <- villainIdNow
        choose "attack Rhino" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 2
        exhaustedNow p `shouldReturn` True

    it "obeys guard and an identity-specific prohibition when choosing attack targets" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        guard <- addMinion p Minions.hydraMercenary
        forbidden <- addMinion p Minions.hydraSoldier
        giveModifiers (IdentityEntity $ IdentityWithId p) [CannotAttackEnemy $ EnemyMinionId forbidden]
        card <- putInHand p "25018"
        money <- putInHand p "25025"
        playWith card [money]
        vid <- villainIdNow
        offers (isTargetChoice vid) `shouldReturn` False
        offers (isTargetChoice forbidden) `shouldReturn` False
        choose "attack the legal guard" (isTargetChoice guard)
        pass
        minionDamageOf guard `shouldReturn` Just 2

    it "stun replaces the attack and is removed without dealing damage" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        resolve [IdentityMessage p IdentityStunned]
        card <- putInHand p "25018"
        money <- putInHand p "25025"
        playWith card [money]
        pass
        isStunned p `shouldReturn` False
        villainDamageNow `shouldReturn` 0

    for_ [False, True] $ \hero ->
      it ("cannot play without its hero form or two resources; hero=" <> show hero) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          clearHand p
          when hero $ changeForm p
          card <- putInHand p "25018"
          _ <- putInHand p (if hero then "25024" else "25025")
          offers (isPlay card) `shouldReturn` False

  describe "Smash the Problem" do
    it "cannot pay its resource or exhaustion costs for a zero-ATK thwart" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        zeroAttackVision p
        clearHand p
        placeMainThreat 4
        card <- putInHand p "25019"
        money <- putInHand p "25024"
        offers (isPlay card) `shouldReturn` False
        exhaustedNow p `shouldReturn` False
        handOf p >>= (`shouldSatisfy` (\cs -> card `elem` cs && money `elem` cs))

    for_ ["valkyrie", "vision"] $ \hero ->
      it ("a confused hero with no threat can pay to remove confuse, including zero ATK: " <> show hero) do
        tg <- testGame [hero]
        let p = player1 tg
        inGame tg do
          if hero == "vision" then zeroAttackVision p else changeForm p
          clearHand p
          mainSchemeThreatNow `shouldReturn` 0
          resolve [IdentityMessage p IdentityConfused]
          card <- putInHand p "25019"
          money <- putInHand p "25024"
          playWith card [money]
          pass
          isConfused p `shouldReturn` False
          exhaustedNow p `shouldReturn` True
          mainSchemeThreatNow `shouldReturn` 0
          discardOf p >>= (`shouldSatisfy` (\cs -> card `elem` cs && money `elem` cs))

    it "pays one resource and exhausts the hero to remove modified ATK threat, ignoring marked-attack bonuses" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "25006"
        mid <- addMinion p Minions.hydraSoldier
        markEnemy p mid
        placeMainThreat 6
        card <- putInHand p "25019"
        money <- putInHand p "25024"
        playWith card [money]
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` 3
        exhaustedNow p `shouldReturn` True
        discardOf p >>= (`shouldSatisfy` elem money)

    it "chooses a side scheme as well as a main scheme" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        side <- revealNow p Schemes.bombScare
        pass
        let sid = sideSchemeOf side
        card <- putInHand p "25019"
        money <- putInHand p "25024"
        playWith card [money]
        choose "thwart the side scheme" (isTargetChoice sid)
        pass
        sideSchemeThreat sid `shouldReturn` 1

    for_ [("alter ego" :: String), "exhausted", "no resources", "no threat"] $ \condition ->
      it ("is unavailable with " <> show condition) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          clearHand p
          unless (condition == "alter ego") $ changeForm p
          unless (condition == "no threat") $ placeMainThreat 3
          when (condition == "exhausted") $ resolve [IdentityMessage p ExhaustedIdentity]
          card <- putInHand p "25019"
          unless (condition == "no resources") $ void $ putInHand p "25024"
          offers (isPlay card) `shouldReturn` False

    it "cannot thwart the main scheme through patrol, but can still thwart a side scheme" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- addMinion p Minions.badoonLieutenant
        placeMainThreat 4
        card <- putInHand p "25019"
        money <- putInHand p "25024"
        offers (isPlay card) `shouldReturn` False
        side <- revealNow p Schemes.bombScare
        pass
        playWith card [money]
        mid <- mainSchemeIdNow
        offers (isTargetChoice mid) `shouldReturn` False
        choose "thwart the side scheme" (isTargetChoice $ sideSchemeOf side)
        pass
        mainSchemeThreatNow `shouldReturn` 4

    it "confuse replaces the thwart after the resource and exhaustion costs are paid" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        placeMainThreat 5
        resolve [IdentityMessage p IdentityConfused]
        card <- putInHand p "25019"
        money <- putInHand p "25024"
        playWith card [money]
        pass
        mainSchemeThreatNow `shouldReturn` 5
        isConfused p `shouldReturn` False
        exhaustedNow p `shouldReturn` True

  describe "The Best Defense…" do
    it "uses modified ATK instead of DEF for the defended attack only" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "25006"
        attacker <- addMinion p Minions.titania
        resolve [MinionMessage attacker $ MinionDamaged GameRef $ toDamage 1 FromAbility]
        card <- putInHand p "25020"
        runNow [MinionMessage attacker $ MinionAttacks p]
        choose "defend" (heroDefense attacker)
        playWith card []
        pass
        damageOf p `shouldReturn` 2
        resolve [IdentityMessage p ReadiedIdentity]
        runNow [MinionMessage attacker $ MinionAttacks p]
        choose "defend the next attack normally" (heroDefense attacker)
        pass
        damageOf p `shouldReturn` 6

    it "ignores Spear and defense-only bonuses and uses only Dragonfang's baseline while defending a marked enemy" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "25006"
        _ <- putIntoPlay p "25005"
        attacker <- addMinion p Minions.titania
        resolve [MinionMessage attacker $ MinionDamaged GameRef $ toDamage 1 FromAbility]
        markEnemy p attacker
        card <- putInHand p "25020"
        expert <- putInHand p "03033"
        runNow [MinionMessage attacker $ MinionAttacks p]
        choose "defend" (heroDefense attacker)
        playWith expert []
        playWith card []
        pass
        damageOf p `shouldReturn` 2

    it "is not playable as an ordinary action or when declining to defend" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "25020"
        offers (isPlay card) `shouldReturn` False
        attacker <- addMinion p Minions.titania
        resolve [MinionMessage attacker $ MinionDamaged GameRef $ toDamage 3 FromAbility]
        runNow [MinionMessage attacker $ MinionAttacks p]
        offers (isPlay card) `shouldReturn` False
        choose "do not defend" isNoDefender
        pass
        discardOf p >>= (`shouldSatisfy` notElem card)

    it "does not apply when another player's hero defends" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        changeForm p
        resolve [IdentityMessage other $ ChangedToForm A]
        card <- putInHand p "25020"
        attacker <- addMinion other Minions.titania
        resolve [MinionMessage attacker $ MinionDamaged GameRef $ toDamage 3 FromAbility]
        runNow [MinionMessage attacker $ MinionAttacks other]
        -- Other players may be asked to defend before the attacked player.
        asked >>= \case
          Just (who, _) | who == p -> choose "Valkyrie declines to defend" isNoDefender
          _ -> pure ()
        choose "the attacked player's hero defends" (heroDefense attacker)
        offers (isPlay card) `shouldReturn` False
        pass

  describe "The Bifrost" do
    for_ [False, True] $ \hero ->
      it ("can be played by an Asgard identity in either form; hero=" <> show hero) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          when hero $ changeForm p
          card <- playPaying p "25023" ["25024"]
          map supportId <$> supportsWith "25023" `shouldReturn` [supportIdOf card]

    it "cannot be played without a resource to pay its printed cost" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "25023"
        offers (isPlay card) `shouldReturn` False

    it "cannot be played by a non-Asgard identity" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- putInHand p "25023"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False

    it "is unique across players" do
      tg <- testGame ["valkyrie", "thor"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- putIntoPlay p "25023"
        card <- putInHand other "25023"
        _ <- putInHand other "01088"
        -- Check the second player's normal play validation directly.
        unique <- selectList $ AffordableCardBy (IdentityWithId other) (InHandOf (IdentityWithId other) $ BasicCardMatches $ CardIs $ getCardDef card)
        unique `shouldSatisfy` notElem card

    it "searches only the controller's deck for Asgard allies, pays ordinary costs, and exhausts once" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        bifrost <- putIntoPlay p "25023"
        clearHand p
        [throg, nonAsgard, nonAlly] <- stackDeck p ["25014", "01050", "25004"]
        [otherAlly] <- stackDeck (player2 tg) ["25013"]
        [discardAlly] <- stackDiscard p ["25013"]
        money <- putInHand p "25025"
        choose "search with The Bifrost" (cardAbility bifrost 1)
        offers (isTargetChoice nonAsgard) `shouldReturn` False
        offers (isTargetChoice nonAlly) `shouldReturn` False
        offers (isTargetChoice otherAlly) `shouldReturn` False
        offers (isTargetChoice discardAlly) `shouldReturn` False
        choose "play Throg from the deck" (isTargetChoice throg)
        alliesWith "25014" `shouldReturn` []
        handOf p >>= (`shouldSatisfy` notElem throg)
        payWith [money]
        pass
        map allyId <$> alliesWith "25014" `shouldReturn` [allyIdOf throg]
        handOf p >>= (`shouldSatisfy` notElem throg)
        discardOf p >>= (`shouldSatisfy` elem money)
        supportExhausted . testHead <$> supportsWith "25023" `shouldReturn` True
        offers (cardAbility bifrost 1) `shouldReturn` False
        deck <- unDeck . playerIdentityDeck <$> playerAttrs p
        deck `shouldSatisfy` notElem throg
        map toCardCode deck `shouldSatisfy` elem "01050"

    it "emits ordinary play windows for Knowhere and entry windows for Throg before shuffling" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        bifrost <- putIntoPlay p "25023"
        knowhere <- putIntoPlay p "17022"
        _ <- addMinion p Minions.hydraSoldier
        [throg, top] <- stackDeck p ["25014", "25027"]
        clearHand p
        money <- putInHand p "25025"
        choose "search with The Bifrost" (cardAbility bifrost 1)
        choose "play Throg" (isTargetChoice throg)
        payWith [money]
        choose "Throg's entry response" (cardAbility throg 1)
        allyTough <$> allyIn "25014" `shouldReturn` True
        choose "Knowhere's play response" (cardAbility knowhere 1)
        pass
        handOf p >>= (`shouldSatisfy` elem top)
        supportExhausted . testHead <$> supportsWith "17022" `shouldReturn` True

    it "does not offer an unaffordable searched ally or play one for free" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        bifrost <- putIntoPlay p "25023"
        clearHand p
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs {playerIdentityDeck = Deck []}
        [throg] <- stackDeck p ["25014"]
        choose "search with no resources" (cardAbility bifrost 1)
        pass
        offers (isTargetChoice throg) `shouldReturn` False
        alliesWith "25014" `shouldReturn` []
        deck <- unDeck . playerIdentityDeck <$> playerAttrs p
        deck `shouldSatisfy` elem throg
        supportExhausted . testHead <$> supportsWith "25023" `shouldReturn` True

    it "does not offer a unique ally already in play under another player's control" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        bifrost <- putIntoPlay p "25023"
        _ <- putIntoPlay (player2 tg) "25014"
        [throg] <- stackDeck p ["25014"]
        _ <- putInHand p "25025"
        choose "search" (cardAbility bifrost 1)
        offers (isTargetChoice throg) `shouldReturn` False
        pass
        length <$> alliesWith "25014" `shouldReturn` 1

    it "applies the ordinary ally limit after playing the searched ally" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        bifrost <- putIntoPlay p "25023"
        first <- putIntoPlay p "01050"
        _ <- putIntoPlay p "01051"
        _ <- putIntoPlay p "01083"
        [throg] <- stackDeck p ["25014"]
        money <- putInHand p "25025"
        choose "search" (cardAbility bifrost 1)
        choose "play the fourth ally" (isTargetChoice throg)
        payWith [money]
        choose "discard the first ally to meet the ally limit" (isTargetChoice $ allyIdOf first)
        pass
        alliesWith "01050" `shouldReturn` []
        length . toList . playerIdentityAllies <$> playerAttrs p `shouldReturn` 3

    it "exhaustion is the activation limit; readying it allows a later search" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        bifrost <- putIntoPlay p "25023"
        clearHand p
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs {playerIdentityDeck = Deck []}
        choose "search the empty deck" (cardAbility bifrost 1)
        offers (cardAbility bifrost 1) `shouldReturn` False
        resolve [SupportMessage (supportIdOf bifrost) ReadiedSupport]
        offers (cardAbility bifrost 1) `shouldReturn` True
