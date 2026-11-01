module Marvel.Hero.AntManSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.Minion.Types qualified as Minion
import Marvel.Villain.Types (villainHp, villainStunned)
import Marvel.Matchers (IdentityMatcher (IdentityCanChangeForm), identityMatches)
import Marvel.Window qualified as W
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

antMan :: IO TestGame
antMan = testGame ["ant-man"]

-- | The side of Ant-Man's card showing: A Tiny, B Scott Lang, C Giant.
sideOf :: IdentityId -> GameAppT Side
sideOf p = (.playerIdentitySide) <$> playerAttrs p

-- | Ant-Man in the form given, his hand empty and his turn's form change spent.
inForm :: IdentityId -> Side -> GameAppT ()
inForm p side = do
  changeFormTo p side
  pass
  clearHand p

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

isChangeForm :: IdentityId -> Choice -> Bool
isChangeForm p = \case
  UseAbility ab -> ChangeForm `elem` abilityChoices ab && abilitySource ab == toSource p
  _ -> False

data Stats = Stats {thwart :: Natural, attack :: Natural, defense :: Natural}

-- | The hero's THW, ATK and DEF as modified.
heroStats :: IdentityId -> GameAppT Stats
heroStats p =
  currentIdentity <$> getPlayer p >>= \case
    HeroSide h -> Stats <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
    AlterEgoSide _ -> pure (Stats 0 0 0)

allyOn :: AllyId -> GameAppT (Attrs Ally)
allyOn a = getsGame (maybe (error "no such ally") toAttrs . HashMap.lookup a . gameAllies)

allyAttackOf, allyHitPointsOf :: AllyId -> GameAppT Natural
allyAttackOf a = Ally.getModifiedAttack =<< allyOn a
allyHitPointsOf a = Ally.getModifiedHitPoints =<< allyOn a

allAllies :: GameAppT [AllyId]
allAllies = getsGame (HashMap.keys . gameAllies)

minionAttackOf :: MinionId -> GameAppT Natural
minionAttackOf m = getsGame (HashMap.lookup m . gameMinions) >>= maybe (error "no such minion") (Minion.getModifiedAttack . toAttrs)

minionCodes :: GameAppT [CardCode]
minionCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMinions)

villainIsStunned :: GameAppT Bool
villainIsStunned = villainStunned . toAttrs <$> getVillain

spec :: Spec
spec = describe "Ant-Man" do
  describe "his three forms" do
    it "Scott Lang changes to either hero form, showing each face" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        sideOf p `shouldReturn` B
        choose "change form" (isChangeForm p)
        offers (isFormChoice $ Just A) `shouldReturn` True
        offers (isFormChoice $ Just C) `shouldReturn` True
        offers \case
          CardLabel code (ChangeToForm C) -> code == "12001c"
          _ -> False
          `shouldReturn` True

    it "Giant Nuisance: after changing to Giant form, deal 1 damage to an enemy" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        changeFormTo p C
        sideOf p `shouldReturn` C
        choose "Giant Nuisance" (isNamedAbility "Giant Nuisance")
        vid <- villainIdNow
        choose "the target" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 1
        hitPointsOf p `shouldReturn` 12
        handSizeOf p `shouldReturn` 4

    it "Puny Pest: after changing to Tiny form, remove 1 threat from a scheme" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        placeMainThreat 3
        before <- mainSchemeThreatNow
        changeFormTo p A
        choose "Puny Pest" (isNamedAbility "Puny Pest")
        m <- mainSchemeIdNow
        choose "the target" (isTargetChoice m)
        mainSchemeThreatNow `shouldReturn` before - 1
        handSizeOf p `shouldReturn` 5

    it "from Tiny, the change of form is to Scott Lang or Giant, once a turn" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        changeFormTo p A
        pass
        offers (isChangeForm p) `shouldReturn` False
        endTurn
        pass
        -- the villain phase over, the next turn may change again
        offers (isChangeForm p) `shouldReturn` True
        choose "change form" (isChangeForm p)
        offers (isFormChoice $ Just B) `shouldReturn` True
        offers (isFormChoice $ Just C) `shouldReturn` True
        offers (isFormChoice $ Just A) `shouldReturn` False

    it "Time to Unwind: after changing to Scott Lang, heal 1 damage" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        endTurn
        pass
        setDamage p 3
        changeFormTo p B
        choose "Time to Unwind" (isNamedAbility "Time to Unwind")
        damageOf p `shouldReturn` 2

    it "an obligation that flips him to alter-ego takes Giant Ant-Man to Scott Lang" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        _ <- revealNow p Obligations.careForCassie
        choose "flip to alter-ego" (isLabel "$label.changeToB")
        sideOf p `shouldReturn` B

  describe "Care for Cassie (12025)" do
    it "exhausting Scott Lang removes it from the game" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.careForCassie
        choose "exhaust Scott" (isLabel "$label.exhaustScottLangRemoveCareForCassie")
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "12025")

    it "otherwise a card is discarded and he cannot change form until his next turn ends" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p A
        card <- putInHand p "01088"
        _ <- revealNow p Obligations.careForCassie
        choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
        choose "discard" (isTargetChoice card)
        discardOf p >>= (`shouldSatisfy` elem card)
        resize <- putInHand p "12005"
        playWith resize []
        sideOf p `shouldReturn` A
        let canChange = identityMatches IdentityCanChangeForm p
        -- revealed during his turn: that turn's end is not the next turn's
        resolve [IdentityEndedTurn p]
        canChange `shouldReturn` False
        resolve [CheckWindows [W.Window W.After (W.TurnBegan p)]]
        canChange `shouldReturn` False
        resolve [IdentityEndedTurn p]
        canChange `shouldReturn` True

  describe "Resize (12005)" do
    it "changes to the other hero form and draws, without spending the turn's change" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        changeFormTo p A
        pass
        clearHand p
        _ <- stackDeck p ["01088"]
        resize <- putInHand p "12005"
        playWith resize []
        pass
        sideOf p `shouldReturn` C
        map toCardCode <$> handOf p `shouldReturn` ["01088"]

  describe "Swarm Tactics (12020)" do
    it "changes to the other hero form and readies the hero" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        resolve [IdentityMessage p ExhaustedIdentity]
        tactics <- putInHand p "12020"
        genius <- putInHand p "01089"
        -- Team-Up (Ant-Man and Wasp): Wasp must be in play too
        offers (isPlay tactics) `shouldReturn` False
        _ <- putIntoPlay p "12002"
        pass
        playWith tactics [genius]
        pass
        sideOf p `shouldReturn` A
        isExhaustedNow p `shouldReturn` False

  describe "Giant Stomp (12003)" do
    it "is played only in Giant form" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p A
        stomp <- putInHand p "12003"
        _ <- traverse (putInHand p) ["01088", "01088"]
        offers (isPlay stomp) `shouldReturn` False

    it "deals 1 damage to each minion, then 8 to an enemy" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        m <- addMinion p Minions.whirlwind
        pass
        stomp <- putInHand p "12003"
        playPaying p stomp ["01088", "01088"]
        pass
        vid <- villainIdNow
        choose "the villain" (isTargetChoice (EnemyVillainId vid))
        pass
        minionDamageOf m `shouldReturn` Just 1
        villainDamageNow `shouldReturn` 8

  describe "Hive Mind (12004)" do
    it "removes 2 threat, and 1 more for each Army of Ants" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p A
        placeMainThreat 6
        before <- mainSchemeThreatNow
        _ <- putIntoPlay p "12007"
        _ <- putIntoPlay p "12007"
        hive <- putInHand p "12004"
        playPaying p hive ["01089", "01089"]
        m <- mainSchemeIdNow
        choose "the target" (isTargetChoice m)
        pass
        mainSchemeThreatNow `shouldReturn` before - 4

  describe "Army of Ants (12007)" do
    it "deals 1 damage in Tiny form only" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        ants <- putIntoPlay p "12007"
        offers (isAbilityOfCard ants) `shouldReturn` False
        resize <- putInHand p "12005"
        playWith resize []
        pass
        choose "Army of Ants" (isAbilityOfCard ants)
        vid <- villainIdNow
        choose "the target" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 1

  describe "Pym Particles (12006)" do
    it "spent in Giant form heals 2 damage" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        setDamage p 3
        particles <- putInHand p "12006"
        ronin <- putInHand p "12013"
        payment <- traverse (putInHand p) ["01088", "01088"]
        playWith ronin (particles : payment)
        choose "heal" (isLabel (countLabel "pymParticlesHeal" 2))
        pass
        damageOf p `shouldReturn` 1
        alliesWith "12013" >>= (`shouldSatisfy` (== 1) . length)

    it "spent in Tiny form draws 1 card" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p A
        _ <- stackDeck p ["01089"]
        particles <- putInHand p "12006"
        ants <- putInHand p "12007"
        playWith ants [particles]
        choose "draw" (isLabel (countLabel "pymParticlesDraw" 1))
        pass
        map toCardCode <$> handOf p `shouldReturn` ["01089"]

  describe "Ant-Man's Helmet (12008)" do
    it "heals 2 after changing to Giant form, draws 1 after changing to Tiny form" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        helmet <- putIntoPlay p "12008"
        setDamage p 4
        changeFormTo p C
        choose "helmet" (isAbilityOfCard helmet)
        pass
        damageOf p `shouldReturn` 2
        clearHand p
        _ <- stackDeck p ["01089"]
        resize <- putInHand p "12005"
        _ <- stackDeck p ["01088"]
        playWith resize []
        choose "helmet" (isAbilityOfCard helmet)
        pass
        length <$> handOf p `shouldReturn` 2

  describe "Giant Strength (12009)" do
    it "gives +1 ATK until the end of the turn after changing to Giant form" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        strength <- putIntoPlay p "12009"
        changeFormTo p C
        choose "Giant Strength" (isAbilityOfCard strength)
        pass
        (.attack) <$> heroStats p `shouldReturn` 4
        endTurn
        pass
        (.attack) <$> heroStats p `shouldReturn` 3

  describe "Wrist Gauntlets (12010)" do
    it "stuns an enemy in Giant form, paid with [physical][physical]" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        gauntlets <- putIntoPlay p "12010"
        strength <- putInHand p "01090"
        choose "gauntlets" (isAbilityOfCard gauntlets)
        choose "pay" (isPayWith strength)
        chooseIf isFinishPayment
        vid <- villainIdNow
        choose "the target" (isTargetChoice vid)
        villainIsStunned `shouldReturn` True
        upgradeIsExhausted (upgradeOf gauntlets) `shouldReturn` True

  describe "Wasp (12002)" do
    it "deals 2 damage as she enters play in Giant form" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        wasp <- putInHand p "12002"
        playPaying p wasp ["01088", "01088", "01088"]
        choose "Wasp" (isAbilityOfCard wasp)
        vid <- villainIdNow
        choose "the target" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 2

  describe "Ant-Man, Hank Pym (12011)" do
    it "gets a pym counter, and a hit point, for each resource overpaid" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p A
        hank <- putInHand p "12011"
        payment <- traverse (putInHand p) ["01088", "01089"]
        choose "play Hank" (isPlay hank)
        payWith payment
        pass
        [a] <- alliesWith "12011"
        allyCounters a `shouldBe` 4
        allyHitPointsOf (allyIdOf hank) `shouldReturn` 4

    it "paid nothing for, he has no hit points and is defeated" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p A
        hank <- putInHand p "12011"
        playWith hank []
        pass
        alliesWith "12011" `shouldReturn` []

  describe "Giant-Man (12012)" do
    it "gets +2 ATK while he has 3 or more remaining hit points" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        giant <- putIntoPlay p "12012"
        allyAttackOf (allyIdOf giant) `shouldReturn` 4
        resolve [AllyMessage (allyIdOf giant) (AllyDamaged GameRef (toDamage 2 FromAbility))]
        allyAttackOf (allyIdOf giant) `shouldReturn` 2

  describe "Ronin (12013)" do
    it "gets +1 THW and +1 ATK with an upgrade attached" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        ronin <- putIntoPlay p "12013"
        allyAttackOf (allyIdOf ronin) `shouldReturn` 2
        suit <- putInHand p "12018"
        playOn suit (allyIdOf ronin) =<< traverse (putInHand p) ["01088"]
        pass
        allyAttackOf (allyIdOf ronin) `shouldReturn` 3
        allyHitPointsOf (allyIdOf ronin) `shouldReturn` 5

  describe "Stinger (12014)" do
    it "does not count against the ally limit" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p A
        for_ ["12012", "12013", "12002"] (putIntoPlay p)
        stinger <- putInHand p "12014"
        playPaying p stinger ["01088"]
        pass
        offers (isLabel "$label.discardAnAlly") `shouldReturn` False
        length <$> allAllies `shouldReturn` 4

  describe "Call for Aid (12015)" do
    it "discards to an Avenger ally and adds it to hand" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p A
        [_, _, ronin] <- stackDeck p ["01088", "12007", "12013"]
        aid <- putInHand p "12015"
        playWith aid []
        pass
        map pcCardId <$> handOf p `shouldReturn` [pcCardId ronin]
        length <$> discardOf p `shouldReturn` 3

  describe "Moxie (12016)" do
    it "gives +1 THW, ATK and DEF until the end of the round after changing form" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        clearHand p
        moxie <- putInHand p "12016"
        changeFormTo p C
        choose "Moxie" (isPlay moxie)
        payWith []
        pass
        heroStats p >>= \s -> (s.thwart, s.attack, s.defense) `shouldBe` (2, 4, 4)

  describe "Lay Down the Law (12031)" do
    it "removes 4 threat when paid with a [mental] resource" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        law <- putInHand p "12031"
        genius <- putInHand p "01089"
        changeFormTo p A
        choose "Lay Down the Law" (isPlay law)
        payWith [genius]
        m <- mainSchemeIdNow
        choose "the target" (isTargetChoice m)
        pass
        mainSchemeThreatNow `shouldReturn` before - 4

  describe "Muster Courage (12032)" do
    it "gives as many friendly characters a tough status card as the villain's stage" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        muster <- putInHand p "12032"
        playPaying p muster ["01088", "01088", "01088"]
        choose "Ant-Man" (isTargetChoice p)
        pass
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True

  describe "Assess the Situation (12033)" do
    it "gives +1 hand size until the end of the phase" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        assess <- putInHand p "12033"
        playWith assess []
        handSizeOf p `shouldReturn` 7

  describe "Moment of Triumph (12030)" do
    it "heals the excess damage of an attack that defeats an enemy" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        setDamage p 5
        m <- addMinion p Minions.hydraMercenary
        pass
        resolve [MinionMessage m (MinionDamaged GameRef (toDamage 2 FromAbility))]
        triumph <- putInHand p "12030"
        choose "attack" \case
          UseAbility ab -> Attack `elem` abilityChoices ab && abilitySource ab == toSource p
          _ -> False
        choose "the target" (isTargetChoice m)
        choose "Moment of Triumph" (isPlay triumph)
        payWith []
        pass
        -- 3 ATK into a minion with 1 hit point left: 2 past it
        damageOf p `shouldReturn` 3

    it "heals the excess damage of an attack that defeats a villain stage" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        setDamage p 5
        vid <- villainIdNow
        hp <- villainHp . toAttrs <$> getVillain
        resolve [VillainMessage vid (VillainDamaged GameRef (toDamage (fromIntegral (Hero.unHp hp) - 1) FromAbility))]
        triumph <- putInHand p "12030"
        choose "attack" \case
          UseAbility ab -> Attack `elem` abilityChoices ab && abilitySource ab == toSource p
          _ -> False
        choose "the target" (isTargetChoice vid)
        -- the next stage's forced abilities first, then the window to respond
        let untilOffered n = unless (n == (0 :: Int)) $ whenM (not <$> offers (isPlay triumph)) do
              asked >>= traverse_ \(who, _) -> answerQuestion who 0
              untilOffered (n - 1)
        untilOffered 10
        choose "Moment of Triumph" (isPlay triumph)
        payWith []
        pass
        -- 3 ATK into a stage with 1 hit point left: 2 past it
        damageOf p `shouldReturn` 3

  describe "Team-Building Exercise (12024)" do
    it "plays a card sharing a trait with the hero for 1 less" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        team <- putIntoPlay p "12024"
        stomp <- putInHand p "12003"
        payment <- traverse (putInHand p) ["01088"]
        choose "Team-Building Exercise" (isAbilityOfCard team)
        choose "Giant Stomp" (isPlay stomp)
        payWith payment
        pass
        vid <- villainIdNow
        choose "the target" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 8

  describe "Power Gloves (12017) and Reinforced Suit (12018)" do
    it "Power Gloves deal 1 damage after the attached Avenger ally attacks" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        ronin <- putIntoPlay p "12013"
        gloves <- putInHand p "12017"
        playOn gloves (allyIdOf ronin) =<< traverse (putInHand p) ["01089"]
        pass
        attacker <- allyOn (allyIdOf ronin)
        choose "Ronin attacks" (isAllyAttack attacker)
        vid <- villainIdNow
        choose "the target" (isTargetChoice vid)
        choose "gloves" (isAbilityOfCard gloves)
        choose "the target" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 4

  describe "nemesis set" do
    it "Yellowjacket gains Giant and retaliate 1 while Ant-Man is Giant, +1 ATK while Tiny" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        m <- addMinion p Minions.yellowjacket
        pass
        minionAttackOf m `shouldReturn` 2
        resolve [IdentityMessage p (ChangedToForm A)]
        pass
        minionAttackOf m `shouldReturn` 3

    it "Tech Theft blanks the text box of Tech cards" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        ronin <- putIntoPlay p "12013"
        suit <- putInHand p "12018"
        playOn suit (allyIdOf ronin) =<< traverse (putInHand p) ["01088"]
        pass
        allyHitPointsOf (allyIdOf ronin) `shouldReturn` 5
        _ <- revealNow p SideSchemes.techTheft
        pass
        allyHitPointsOf (allyIdOf ronin) `shouldReturn` 3

    it "Size Increase attaches to Yellowjacket, else the villain, and wears off" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        m <- addMinion p Minions.yellowjacket
        pass
        _ <- revealNow p Attachments.sizeIncrease
        pass
        minionAttackOf m `shouldReturn` 4

    it "Yellowjacket's Plan discards to a card of the nemesis set and reveals it" do
      tg <- antMan
      let p = player1 tg
      inGame tg do
        inForm p C
        _ <- stackEncounterDeck [Minions.hydraMercenary, Minions.yellowjacket]
        _ <- revealNow p Treacheries.yellowjacketsPlan
        pass
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01101")
        minionCodes >>= (`shouldSatisfy` elem "12027")
