module Marvel.Aspect.WarMachinePackExtrasSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Hero ()
import Marvel.Stats (Atk (..))
import Marvel.Count (selectCount)
import Marvel.Card.Side (Side (A))
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Game.Source (getModifiers)
import Marvel.Keyword (Keyword (Ranged))
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Trait (Trait (Avenger, Guardian))
import TestImport

ownAbility :: (IsRef a) => a -> Natural -> Choice -> Bool
ownAbility target index = \case
  UseAbility ab -> abilitySource ab == toRef target && abilityIndex ab == index
  _ -> False

asHero :: IdentityId -> GameAppT ()
asHero p = resolve [IdentityMessage p (ChangedToForm A)] >> clearHand p

payCodes :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
payCodes p card codes = traverse (putInHand p) codes >>= playWithoutDiscount card

-- Star-Lord's optional play interrupt is a real window before resource payment;
-- these tests exercise the printed cost rather than taking an encounter card.
playWithoutDiscount :: PlayerCard -> [PlayerCard] -> GameAppT ()
playWithoutDiscount card payment = do
  choose "play the card" (isPlay card)
  chooseIf isNoResponse
  payWith payment

allyExhaustedNow :: PlayerCard -> GameAppT Bool
allyExhaustedNow card = getsGame (maybe False (allyExhausted . toAttrs) . HashMap.lookup (allyIdOf card) . gameAllies)

setDeck :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
setDeck p codes = do
  withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck []}
  stackDeck p codes

usesOf :: PlayerCard -> GameAppT Natural
usesOf card = getsGame (maybe 0 (supportUses . toAttrs) . HashMap.lookup (supportIdOf card) . gameSupports)

newtype FixtureModifiers = FixtureModifiers (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage FixtureModifiers where
  runMessage msg (FixtureModifiers a) = FixtureModifiers <$> runMessage msg a

modifyAlly :: PlayerCard -> [Modifier] -> GameAppT ()
modifyAlly card modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (FixtureModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = AllyEntity (AllyWithId $ allyIdOf card), effectModifiers = modifiers, effectEnds = Nothing })

-- Retain the real hero/ally implementation (including Star-Lord's trait aura)
-- while setting its printed base stat to zero. Modifiers contain Naturals.
zeroAttackField :: (ToJSON a, FromJSON a) => Key -> a -> a
zeroAttackField field entity = case toJSON entity of
  Object object -> case fromJSON (Object $ KeyMap.insert field (toJSON $ Atk 0) object) of
    Success replacement -> replacement
    Error message -> error $ "zero-ATK fixture: " <> show message
  _ -> error "zero-ATK fixture expects an object"

zeroHeroAttack :: IdentityId -> GameAppT ()
zeroHeroAttack ident = withGame_ $ entitiesL . playersL . ix ident %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentitySides = HashMap.map zeroSide (playerIdentitySides a)}
 where
  zeroSide (HeroSide hero) = HeroSide (zeroAttackField "heroBaseAttack" hero)
  zeroSide side = side

zeroAllyAttack :: PlayerCard -> GameAppT ()
zeroAllyAttack card = withGame_ $ entitiesL . alliesL . ix (allyIdOf card) %~ zeroAttackField "allyAttack"

preparePair :: IdentityId -> GameAppT PlayerCard
preparePair p = do
  asHero p
  putIntoPlay p "01068"

payPairEvent :: IdentityId -> PlayerCard -> PlayerCard -> [CardCode] -> GameAppT ()
payPairEvent p event avenger resources = do
  payCodes p event resources
  choose "exhaust the Avenger" (isTargetChoice $ allyIdOf avenger)
  -- Star-Lord is the remaining ready Guardian, selected automatically.

spec :: Spec
spec = describe "War Machine extra aspect cards" do
  describe "Two Against the World (23024)" do
    it "puts a Tech upgrade from its player's deck into play and readies both named characters" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        iron <- putIntoPlay p "23002"
        resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf iron) ExhaustedAlly]
        [tech, _] <- stackDeck p ["23004", "01088"]
        event <- putInHand p "23024"
        payCodes p event ["01088", "01089"]
        choose "find Upgraded Chassis" (isTargetChoice tech)
        pass
        upgradesWith "23004" >>= (`shouldSatisfy` notNull)
        handOf p >>= (`shouldSatisfy` notElem tech)
        playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` False
        allyExhaustedNow iron `shouldReturn` False

    it "still readies the team when no Tech upgrade is found" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        iron <- putIntoPlay p "23002"
        resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf iron) ExhaustedAlly]
        _ <- setDeck p ["01088"]
        event <- putInHand p "23024"
        payCodes p event ["01088", "01089"]
        choose "no matching card" (isLabel "$label.noMatchingCardsFound")
        pass
        playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` False
        allyExhaustedNow iron `shouldReturn` False

    it "cannot be played when either named friendly character is absent" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        event <- putInHand p "23024"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay event) `shouldReturn` False
        _ <- putIntoPlay p "23002"
        offers (isPlay event) `shouldReturn` True

    it "readies a named hero controlled by another player" do
      tg <- testGame ["war-machine", "iron-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        asHero other
        resolve [IdentityMessage p ExhaustedIdentity, IdentityMessage other ExhaustedIdentity]
        _ <- setDeck p ["01088"]
        event <- putInHand p "23024"
        payCodes p event ["01088", "01089"]
        choose "no matching card" (isLabel "$label.noMatchingCardsFound")
        pass
        playerIdentityExhausted . toAttrs <$> getPlayer other `shouldReturn` False

    it "requires hero form and does not treat James Rhodes as War Machine in play" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "23002"
        event <- putInHand p "23024"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay event) `shouldReturn` False

    it "does not search its discard pile or another player's deck" do
      tg <- testGame ["war-machine", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "23002"
        _ <- setDeck p ["01088"]
        [discardedTech] <- stackDiscard p ["23004"]
        [foreignTech] <- stackDeck other ["23005"]
        event <- putInHand p "23024"
        payCodes p event ["01088", "01089"]
        offers (isTargetChoice discardedTech) `shouldReturn` False
        offers (isTargetChoice foreignTech) `shouldReturn` False
        choose "no matching card" (isLabel "$label.noMatchingCardsFound")
        upgradesWith "23004" `shouldReturn` []
        upgradesWith "23005" `shouldReturn` []

    it "chooses a legal attachment host for the Tech upgrade before putting it into play" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        iron <- putIntoPlay p "23002"
        [gloves] <- setDeck p ["12017"]
        event <- putInHand p "23024"
        payCodes p event ["01088", "01089"]
        choose "find Power Gloves" (isTargetChoice gloves)
        choose "attach to Iron Man" (isTargetChoice $ allyIdOf iron)
        pass
        [upgrade] <- upgradesWith "12017"
        upgradeAttachedAlly upgrade `shouldBe` Just (allyIdOf iron)

  describe "As One! (23032)" do
    it "exhausts distinct Avenger and Guardian characters and deals their combined current ATK" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        vision <- preparePair p
        modifyAlly vision [AttackModifier 2]
        event <- putInHand p "23032"
        payPairEvent p event vision ["01088"]
        chooseVillain
        pass
        -- Star-Lord 2 + Vision's printed 2 + modifier 2.
        villainDamageNow `shouldReturn` 6
        allyExhaustedNow vision `shouldReturn` True
        playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` True
        (.allyDamage) <$> allyIn "01068" `shouldReturn` 0

    it "lets another player consent to and select their own character contribution" do
      tg <- testGame ["star-lord", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        asHero other
        event <- putInHand p "23032"
        payCodes p event ["01088"]
        fmap fst <$> asked `shouldReturn` Just other
        choose "Captain Marvel agrees" (isLabel "$label.contributeToCost")
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4
        playerIdentityExhausted . toAttrs <$> getPlayer other `shouldReturn` True
        playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` True

    it "overkills a defeated minion and creates one player attack" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        vision <- preparePair p
        minion <- addMinion p Minions.hydraRegular
        event <- putInHand p "23032"
        payPairEvent p event vision ["01088"]
        choose "attack the regular" (isTargetChoice minion)
        pass
        minionDamageOf minion `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 2

    it "cannot pay both exhaustions with its only dual-trait ally" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        _ <- preparePair p
        resolve [IdentityMessage p ExhaustedIdentity]
        event <- putInHand p "23032"
        _ <- putInHand p "01088"
        offers (isPlay event) `shouldReturn` False

    it "does not offer already exhausted cost characters" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        vision <- preparePair p
        resolve [AllyMessage (allyIdOf vision) ExhaustedAlly]
        event <- putInHand p "23032"
        _ <- putInHand p "01088"
        offers (isPlay event) `shouldReturn` False

    it "pays its characters even when stun replaces the attack" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        vision <- preparePair p
        resolve [GiveStatus (toRef p) StunnedStatus]
        event <- putInHand p "23032"
        payPairEvent p event vision ["01088"]
        pass
        villainDamageNow `shouldReturn` 0
        allyExhaustedNow vision `shouldReturn` True
        playerIdentityStunned . toAttrs <$> getPlayer p `shouldReturn` False

    it "requires a possible positive-damage effect unless stun will replace it" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        vision <- preparePair p
        zeroAllyAttack vision
        zeroHeroAttack p
        event <- putInHand p "23032"
        _ <- putInHand p "01088"
        offers (isPlay event) `shouldReturn` False
        resolve [GiveStatus (toRef p) StunnedStatus]
        offers (isPlay event) `shouldReturn` True

    it "cannot finalize a zero-ATK pair when a positive alternative pair exists" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        vision <- preparePair p
        positive <- putIntoPlay p "01083"
        zeroAllyAttack vision
        zeroHeroAttack p
        event <- putInHand p "23032"
        payCodes p event ["01088"]
        choose "choose zero-ATK Vision first" (isTargetChoice $ allyIdOf vision)
        offers (isTargetChoice p) `shouldReturn` False
        offers (isTargetChoice $ allyIdOf positive) `shouldReturn` True
        choose "complete a meaningful pair" (isTargetChoice $ allyIdOf positive)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 1
        allyExhaustedNow vision `shouldReturn` True
        allyExhaustedNow positive `shouldReturn` True
        playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` False

    it "does not offer a contributor allocation whose only complete pair has zero ATK" do
      tg <- testGame ["rocket-raccoon", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        asHero other
        zeroHeroAttack p
        zeroHeroAttack other
        positive <- putIntoPlay p "01068"
        event <- putInHand p "23032"
        payCodes p event ["01088"]
        fmap fst <$> asked `shouldReturn` Just p
        offers (isTargetChoice other) `shouldReturn` False
        offers (isLabel "$label.contributeToCost") `shouldReturn` False
        choose "the positive Avenger" (isTargetChoice $ allyIdOf positive)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        playerIdentityExhausted . toAttrs <$> getPlayer other `shouldReturn` False

    it "does not initiate under Peril when only another player's pair could deal positive damage" do
      tg <- testGame ["rocket-raccoon", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        asHero other
        zeroHeroAttack p
        zero <- putIntoPlay p "01068"
        zeroAllyAttack zero
        event <- putInHand p "23032"
        _ <- putInHand p "01088"
        offers (isPlay event) `shouldReturn` True
        resolve [SetPeril (Just p)]
        offers (isPlay event) `shouldReturn` False
        playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` False
        allyExhaustedNow zero `shouldReturn` False

    it "uses the initiating player's stun exception when another player contributes a zero-ATK character" do
      tg <- testGame ["rocket-raccoon", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        asHero other
        zeroHeroAttack p
        zeroHeroAttack other
        _ <- putIntoPlay p "01068"
        resolve [GiveStatus (toRef p) StunnedStatus]
        event <- putInHand p "23032"
        payCodes p event ["01088"]
        choose "invite the zero-ATK Avenger" (isTargetChoice other)
        fmap fst <$> asked `shouldReturn` Just other
        choose "Captain Marvel agrees" (isLabel "$label.contributeToCost")
        pass
        villainDamageNow `shouldReturn` 0
        playerIdentityStunned . toAttrs <$> getPlayer p `shouldReturn` False
        playerIdentityExhausted . toAttrs <$> getPlayer other `shouldReturn` True
        playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` True

    it "requires hero form" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "01068"
        _ <- putIntoPlay p "19013"
        event <- putInHand p "23032"
        _ <- putInHand p "01088"
        offers (isPlay event) `shouldReturn` False

  describe "Vigilante Training (23033)" do
    it "starts with two counters and shuffles only a Justice event from its own discard" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        support <- putIntoPlay p "23033"
        [justice, aggression, ally] <- stackDiscard p ["01060", "01053", "01059"]
        [otherPlayersCard] <- stackDiscard other ["01060"]
        usesOf support `shouldReturn` 2
        choose "train" (ownAbility (supportIdOf support) 1)
        offers (isTargetChoice aggression) `shouldReturn` False
        offers (isTargetChoice ally) `shouldReturn` False
        offers (isTargetChoice otherPlayersCard) `shouldReturn` False
        choose "shuffle For Justice" (isTargetChoice justice)
        discardOf p >>= (`shouldSatisfy` notElem justice)
        playerIdentityDeck . toAttrs <$> getPlayer p >>= (\(Deck cards) -> cards `shouldSatisfy` elem justice)
        usesOf support `shouldReturn` 1
        offers (ownAbility (supportIdOf support) 1) `shouldReturn` False

    it "does not offer the action without a Justice event in its discard" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        support <- putIntoPlay p "23033"
        _ <- stackDiscard p ["01053"]
        offers (ownAbility (supportIdOf support) 1) `shouldReturn` False
        usesOf support `shouldReturn` 2

    it "cannot use its action in hero form" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        support <- putIntoPlay p "23033"
        _ <- stackDiscard p ["01060"]
        offers (ownAbility (supportIdOf support) 1) `shouldReturn` False

    it "discards itself after spending its last training counter" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        support <- putIntoPlay p "23033"
        [justice] <- stackDiscard p ["01060"]
        choose "train once" (ownAbility (supportIdOf support) 1)
        choose "shuffle" (isTargetChoice justice)
        [again] <- stackDiscard p ["01060"]
        resolve [SupportMessage (supportIdOf support) ReadiedSupport]
        choose "train again" (ownAbility (supportIdOf support) 1)
        choose "shuffle again" (isTargetChoice again)
        supportsWith "23033" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem support)

  describe "Stand Together (23034)" do
    it "prevents all pending hero attack damage and deals that amount to the attacking enemy" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        vision <- preparePair p
        event <- putInHand p "23034"
        energy <- putInHand p "01088"
        genius <- putInHand p "01089"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 4 FromAttack))
        playWithoutDiscount event [energy, genius]
        choose "contribute the Avenger" (isTargetChoice $ allyIdOf vision)
        pass
        damageOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 4
        allyExhaustedNow vision `shouldReturn` True
        playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` True

    it "protects an ally and its returned damage is not a player attack" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        vision <- preparePair p
        victim <- putIntoPlay p "01083"
        event <- putInHand p "23034"
        energy <- putInHand p "01088"
        genius <- putInHand p "01089"
        resolve [GiveStatus (toRef p) StunnedStatus]
        vid <- villainIdNow
        start (damageAlly (allyIdOf victim) (toSource vid) (toDamage 3 FromAttack))
        playWithoutDiscount event [energy, genius]
        choose "use Vision for the Avenger cost" (isTargetChoice $ allyIdOf vision)
        -- Mockingbird also has Guardian from Star-Lord, so explicitly choose
        -- the hero rather than leaving this cost question unanswered.
        choose "Star-Lord pays the Guardian cost" (isTargetChoice p)
        pass
        (.allyDamage) <$> allyIn "01083" `shouldReturn` 0
        villainDamageNow `shouldReturn` 3
        playerIdentityStunned . toAttrs <$> getPlayer p `shouldReturn` True

    it "does not trigger for nonattack damage" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        _ <- preparePair p
        event <- putInHand p "23034"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 2 FromAbility))
        offers (isPlay event) `shouldReturn` False
        resolve []
        damageOf p `shouldReturn` 2

    it "requires two distinct ready cost characters before offering its interrupt" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        _ <- preparePair p
        resolve [IdentityMessage p ExhaustedIdentity]
        event <- putInHand p "23034"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 2 FromAttack))
        offers (isPlay event) `shouldReturn` False
        resolve []
        damageOf p `shouldReturn` 2

    it "cannot interrupt while its controller is in alter-ego form" do
      tg <- testGame ["star-lord"]
      let p = player1 tg
      inGame tg do
        clearHand p
        event <- putInHand p "23034"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 2 FromAttack))
        offers (isPlay event) `shouldReturn` False

  describe "Sidearm (23035)" do
    it "attaches to another player's ally, adds one ATK and gives that ally ranged" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        ally <- putIntoPlay other "01068"
        sidearm <- putInHand p "23035"
        energy <- putInHand p "01088"
        choose "play Sidearm" (isPlay sidearm)
        choose "attach to Vision" (isTargetChoice $ allyIdOf ally)
        payWith [energy]
        pass
        attack <- selectCount AllyAttackValue (AllyWithId $ allyIdOf ally)
        attack `shouldBe` 3
        modifiers <- getModifiers (allyIdOf ally)
        modifiers `shouldSatisfy` elem (KeywordModifier Ranged)

    it "cannot attach a second Sidearm to the same ally" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "01068"
        first <- putInHand p "23035"
        energy <- putInHand p "01088"
        choose "play first" (isPlay first)
        choose "attach" (isTargetChoice $ allyIdOf ally)
        payWith [energy]
        second <- putInHand p "23035"
        _ <- putInHand p "01088"
        offers (isPlay second) `shouldReturn` False

    it "does not become playable without any ally" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        sidearm <- putInHand p "23035"
        _ <- putInHand p "01088"
        offers (isPlay sidearm) `shouldReturn` False

    it "adds damage to the ally's ordinary attack without extra consequential damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "01068"
        sidearm <- putInHand p "23035"
        energy <- putInHand p "01088"
        choose "play" (isPlay sidearm)
        choose "attach" (isTargetChoice $ allyIdOf ally)
        payWith [energy]
        a <- allyIn "01068"
        choose "attack with Vision" (isAllyAttack a)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        (.allyDamage) <$> allyIn "01068" `shouldReturn` 1

    it "its ranged attack prevents retaliate while still dealing consequential damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "01068"
        sidearm <- putInHand p "23035"
        energy <- putInHand p "01088"
        choose "play" (isPlay sidearm)
        choose "attach" (isTargetChoice $ allyIdOf ally)
        payWith [energy]
        whiplash <- addMinion p Minions.whiplash
        a <- allyIn "01068"
        choose "attack" (isAllyAttack a)
        choose "Whiplash" (isTargetChoice whiplash)
        pass
        minionDamageOf whiplash `shouldReturn` Just 3
        (.allyDamage) <$> allyIn "01068" `shouldReturn` 1
