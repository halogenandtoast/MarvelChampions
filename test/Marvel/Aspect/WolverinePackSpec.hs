module Marvel.Aspect.WolverinePackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Modifier
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Matchers.Types
import Marvel.Status
import Marvel.Villain.Types qualified as Villain
import TestImport

isVillainChoice :: Choice -> Bool
isVillainChoice = \case
  TargetLabel (VillainRef _) _ -> True
  TargetLabel (EnemyRef (EnemyVillainId _)) _ -> True
  TargetLabel (CharacterRef (VillainCharacter _)) _ -> True
  CardLabel _ choice -> isVillainChoice choice
  _ -> False

newtype TestAttackModifiers = TestAttackModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage TestAttackModifiers where
  runMessage msg (TestAttackModifiers a) = TestAttackModifiers <$> runMessage msg a

giveAttackModifiers :: MinionId -> [Modifier] -> GameAppT ()
giveAttackModifiers mid modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (TestAttackModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = MinionEntity (MinionWithId mid), effectModifiers = modifiers, effectEnds = Nothing })

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

responseOf :: PlayerCard -> Choice -> Bool
responseOf c ch = isAbilityOfCard c ch && case ch of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

attackWith :: PlayerCard -> GameAppT ()
attackWith c = do
  a <- allyIn (toCardCode c)
  choose "ally attacks" (isAllyAttack a)

payOne :: IdentityId -> CardCode -> GameAppT PlayerCard
payOne p code = do
  c <- putInHand p code
  pay <- putInHand p "01090"
  playWith c [pay]
  pure c

attachmentCodes :: GameAppT [CardCode]
attachmentCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameAttachments)

spec :: Spec
spec = describe "Wolverine pack aspect and basic cards" do
  describe "Psylocke" do
    it "enters with two counters; spends one to confuse and deal one damage before her attack" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35013"
        (.allyCounters) <$> allyIn "35013" `shouldReturn` 2
        attackWith c
        choose "villain" isVillainChoice
        choose "Psylocke interrupt" (responseOf c)
        pass
        villainDamageNow `shouldReturn` 2
        Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` True
        (.allyCounters) <$> allyIn "35013" `shouldReturn` 1
        (.allyDamage) <$> allyIn "35013" `shouldReturn` 1

    it "can decline her interrupt without removing a counter" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35013"
        attackWith c
        choose "villain" isVillainChoice
        pass
        villainDamageNow `shouldReturn` 1
        (.allyCounters) <$> allyIn "35013" `shouldReturn` 2

    it "her interrupt defeat cancels attack consequential damage" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35013"
        mid <- addMinion p Minions.hydraMercenary
        resolve [MinionMessage mid (MinionDamaged (toSource p) (toDamage 2 FromAbility))]
        attackWith c
        choose "minion" (isTargetChoice mid)
        choose "Psylocke interrupt" (responseOf c)
        pass
        minionDamageOf mid `shouldReturn` Nothing
        (.allyDamage) <$> allyIn "35013" `shouldReturn` 0

    it "does not offer her interrupt after both counters have been spent" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35013"
        for_ [1 :: Int, 2] \_ -> do
          resolve [AllyMessage (allyIdOf c) ReadiedAlly]
          attackWith c
          choose "villain" isVillainChoice
          choose "Psylocke interrupt" (responseOf c)
          pass
        resolve [AllyMessage (allyIdOf c) ReadiedAlly]
        attackWith c
        choose "villain" isVillainChoice
        offers (responseOf c) `shouldReturn` False
        pass

  describe "Sunfire" do
    it "spends an energy resource after being played from hand to discard a printed Hero Action attachment" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- revealNow p Attachments.enhancedIvoryHorn
        pass
        c <- putInHand p "35014"
        payment <- putInHand p "01090"
        energy <- putInHand p "01088"
        playWith c [payment]
        choose "Sunfire response" (responseOf c)
        payWith [energy]
        choose "discard horn" (const True)
        attachmentCodes `shouldReturn` []

    it "does not respond to being put into play" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- revealNow p Attachments.enhancedIvoryHorn
        pass
        _ <- putInHand p "01088"
        c <- putIntoPlay p "35014"
        offers (responseOf c) `shouldReturn` False
        attachmentCodes `shouldReturn` ["01100"]

    it "does not offer a paid response when no eligible attachment exists" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- revealNow p Attachments.charge
        pass
        c <- putInHand p "35014"
        payment <- putInHand p "01090"
        _ <- putInHand p "01088"
        playWith c [payment]
        offers (responseOf c) `shouldReturn` False

  describe "Warrior Skill" do
    it "starts with three uses and adds one damage to a basic attack" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35016"
        map (.upgradeUses) <$> upgradesWith "35016" `shouldReturn` [3]
        choose "hero attack" (isHeroAttack p)
        choose "villain" isVillainChoice
        choose "Warrior Skill" (responseOf c)
        pass
        villainDamageNow `shouldReturn` 3
        map (.upgradeUses) <$> upgradesWith "35016" `shouldReturn` [2]

    it "adds damage to an attack event and combines with Aggressive Energy" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35016"
        event <- putInHand p "35017"
        energy <- putInHand p "35020"
        strength <- putInHand p "01090"
        playWith event [energy, strength]
        choose "Aggressive Energy" (isLabel "$label.aggressiveEnergy")
        choose "villain" isVillainChoice
        choose "Warrior Skill" (responseOf c)
        pass
        villainDamageNow `shouldReturn` 5

    it "is discarded after its third use and the last attack still gains damage" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35016"
        for_ [1 :: Natural .. 3] \n -> do
          resolve [IdentityMessage p ReadiedIdentity]
          choose "hero attack" (isHeroAttack p)
          choose "villain" isVillainChoice
          choose "Warrior Skill" (responseOf c)
          pass
          villainDamageNow `shouldReturn` (3 * n)
        upgradesWith "35016" `shouldReturn` []
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "35016")

    it "discards tough before applying reduction that would reduce the attack to zero" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        mid <- addMinion p Minions.yonRogg
        giveAttackModifiers mid [AttackDamageReduction 2]
        resolve [GiveStatus (MinionRef mid) ToughStatus]
        choose "hero attack" (isHeroAttack p)
        choose "tough minion" (isTargetChoice mid)
        pass
        minionDamageOf mid `shouldReturn` Just 0
        getsGame (fmap (minionTough . toAttrs) . HashMap.lookup mid . gameMinions) `shouldReturn` Just False

    it "applies the damage cap after Warrior Skill increases attack damage" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35016"
        mid <- addMinion p Minions.yonRogg
        giveAttackModifiers mid [AttackDamageCap 2]
        choose "hero attack" (isHeroAttack p)
        choose "capped minion" (isTargetChoice mid)
        choose "Warrior Skill" (responseOf c)
        pass
        minionDamageOf mid `shouldReturn` Just 2
        map (.upgradeUses) <$> upgradesWith "35016" `shouldReturn` [2]

    it "boosts all simultaneous targets of Giant Stomp using one counter" do
      tg <- testGame ["ant-man"]
      inGame tg do
        let p = player1 tg
        changeFormTo p C
        pass
        clearHand p
        c <- putIntoPlay p "35016"
        first <- addMinion p Minions.yonRogg
        second <- addMinion p Minions.titania
        event <- putInHand p "12003"
        payment <- traverse (putInHand p) ["01090", "01089"]
        playWith event payment
        choose "Warrior Skill" (responseOf c)
        pass
        minionDamageOf first `shouldReturn` Just 2
        minionDamageOf second `shouldReturn` Just 2
        choose "main strike" isVillainChoice
        offers (responseOf c) `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 9
        map (.upgradeUses) <$> upgradesWith "35016" `shouldReturn` [2]

    it "boosts every damage instance in one labeled attack" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35016"
        mid <- addMinion p Minions.yonRogg
        event <- putInHand p "05030"
        payment <- traverse (putInHand p) ["01090", "01089"]
        playWith event payment
        choose "first enemy" (isTargetChoice mid)
        choose "Warrior Skill" (responseOf c)
        pass
        choose "second enemy" isVillainChoice
        offers (responseOf c) `shouldReturn` False
        pass
        minionDamageOf mid `shouldReturn` Just 4
        villainDamageNow `shouldReturn` 4
        map (.upgradeUses) <$> upgradesWith "35016" `shouldReturn` [2]

    it "boosts only the chosen attack of a two-attack event" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35016"
        event <- putInHand p "35009"
        payment <- traverse (putInHand p) ["01090", "01089"]
        playWith event payment
        choose "first attack" isVillainChoice
        choose "Warrior Skill" (responseOf c)
        pass
        choose "second attack" isVillainChoice
        pass
        villainDamageNow `shouldReturn` 7
        map (.upgradeUses) <$> upgradesWith "35016" `shouldReturn` [2]

    it "cannot play a second copy under the same player's control" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putIntoPlay p "35016"
        c <- putInHand p "35016"
        _ <- putInHand p "01090"
        offers (isPlay c) `shouldReturn` False

  describe "Outta My Way!" do
    it "deals three damage to an ordinary enemy" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- payOne p "35017"
        choose "villain" isVillainChoice
        pass
        villainDamageNow `shouldReturn` 3

    it "deals five damage to guard and patrol enemies" do
      for_ [(Minions.hydraSoldier, Nothing), (Minions.badoonLieutenant, Just 5)] \(def, expected) -> do
        tg <- testGame ["wolverine"]
        inGame tg do
          let p = player1 tg
          asHero p
          mid <- addMinion p def
          _ <- payOne p "35017"
          choose "minion" (isTargetChoice mid)
          pass
          minionDamageOf mid `shouldReturn` expected

  describe "Precision Strike" do
    it "heals two damage when its attack defeats an enemy" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        setDamage p 4
        mid <- addMinion p Minions.hydraMercenary
        resolve [MinionMessage mid (MinionDamaged (toSource p) (toDamage 1 FromAbility))]
        _ <- payOne p "35018"
        choose "minion" (isTargetChoice mid)
        pass
        minionDamageOf mid `shouldReturn` Nothing
        damageOf p `shouldReturn` 2

    it "does not heal when the target survives" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        setDamage p 4
        _ <- payOne p "35018"
        choose "villain" isVillainChoice
        pass
        damageOf p `shouldReturn` 4
        villainDamageNow `shouldReturn` 2

    it "does not heal when tough prevents the attack's damage" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        setDamage p 4
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) ToughStatus]
        _ <- payOne p "35018"
        choose "villain" isVillainChoice
        pass
        damageOf p `shouldReturn` 4
        villainDamageNow `shouldReturn` 0

  describe "Weapon X" do
    it "takes one damage and exhausts to discard until an identity card, adding it to hand" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        clearHand p
        c <- putIntoPlay p "35022"
        [ordinary, signature] <- stackDeck p ["01084", "35008"]
        before <- damageOf p
        choose "Weapon X" (responseOf c)
        pass
        damageOf p `shouldReturn` (before + 1)
        handOf p `shouldReturn` [signature]
        discardOf p >>= (`shouldSatisfy` elem ordinary)
        supportsWith "35022" >>= \as -> map (.supportExhausted) as `shouldBe` [True]

    it "cannot play without the Mutant trait in the current form" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putInHand p "35022"
        _ <- putInHand p "01090"
        offers (isPlay c) `shouldReturn` False

    it "cannot activate in hero form" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        c <- putIntoPlay p "35022"
        asHero p
        offers (responseOf c) `shouldReturn` False

    it "cannot pay damage while tough" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        clearHand p
        c <- putIntoPlay p "35022"
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        offers (responseOf c) `shouldReturn` False

  describe "Fastball Special" do
    it "requires both named characters" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putInHand p "35023"
        _ <- putInHand p "01090"
        offers (isPlay c) `shouldReturn` False

    it "sums hero and ally ATK, pierces tough, and overkills" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- putIntoPlay p "35021"
        _ <- putIntoPlay p "01057"
        mid <- addMinion p Minions.hydraMercenary
        resolve [GiveStatus (MinionRef mid) ToughStatus]
        _ <- payOne p "35023"
        choose "minion" (isTargetChoice mid)
        pass
        minionDamageOf mid `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 3

  describe "Longshot" do
    it "defeats a surviving non-Elite minion when the discarded encounter card has a star" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35033"
        mid <- addMinion p Minions.hydraMercenary
        -- A toughness status keeps the minion alive through Longshot's attack.
        resolve [GiveStatus (MinionRef mid) ToughStatus]
        _ <- stackEncounterDeck [Treacheries.tentacleStrike]
        attackWith c
        choose "minion" (isTargetChoice mid)
        choose "Longshot response" (responseOf c)
        pass
        minionDamageOf mid `shouldReturn` Nothing
        encounterDiscardCodes >>= (`shouldSatisfy` elem "35031")

    it "leaves a minion alive when the discarded card has no star" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35033"
        mid <- addMinion p Minions.hydraMercenary
        _ <- stackEncounterDeck [Treacheries.assault]
        attackWith c
        choose "minion" (isTargetChoice mid)
        choose "Longshot response" (responseOf c)
        pass
        minionDamageOf mid `shouldReturn` Just 2

    it "cannot respond after attacking the villain or an Elite minion" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "35033"
        mid <- addMinion p Minions.omegaRed
        attackWith c
        choose "Elite minion" (isTargetChoice mid)
        offers (responseOf c) `shouldReturn` False
        pass
        resolve [AllyMessage (allyIdOf c) ReadiedAlly]
        attackWith c
        choose "villain" isVillainChoice
        offers (responseOf c) `shouldReturn` False
        pass

    it "cannot be played without X-Men in the current form" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        clearHand p
        c <- putInHand p "35033"
        _ <- putInHand p "01090"
        _ <- putInHand p "01089"
        offers (isPlay c) `shouldReturn` False

  describe "Command Center" do
    it "exhausts after an ally defeats a side scheme to deal two damage" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        center <- putIntoPlay p "35032"
        _ <- putIntoPlay p "35013"
        side <- revealNow p SideSchemes.crowdControl
        pass
        a <- allyIn "35013"
        choose "ally thwart" (isAllyThwart a)
        choose "side scheme" (isTargetChoice $ sideSchemeOf side)
        choose "Command Center" (responseOf center)
        choose "villain" isVillainChoice
        pass
        villainDamageNow `shouldReturn` 2
        supportsWith "35032" >>= \as -> map (.supportExhausted) as `shouldBe` [True]

    it "does not trigger when Nick Fury removes threat without thwarting" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        center <- putIntoPlay p "35032"
        side <- revealNow p SideSchemes.crowdControl
        pass
        fury <- putInHand p "01084"
        payment <- traverse (putInHand p) ["01090", "01089"]
        playWith fury payment
        choose "Nick Fury's response" (responseOf fury)
        choose "remove threat" (isLabel $ countLabel "removeThreatFromAScheme" 2)
        choose "side scheme" (isTargetChoice $ sideSchemeOf side)
        offers (responseOf center) `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 0
        supportsWith "35032" >>= \as -> map (.supportExhausted) as `shouldBe` [False]

    it "does not trigger after a hero defeats a side scheme" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        center <- putIntoPlay p "35032"
        side <- revealNow p SideSchemes.crowdControl
        pass
        choose "hero thwart" (isHeroThwart p)
        choose "side scheme" (isTargetChoice $ sideSchemeOf side)
        offers (responseOf center) `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 0

    it "cannot respond a second time while exhausted" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        asHero p
        center <- putIntoPlay p "35032"
        c <- putIntoPlay p "35013"
        side <- revealNow p SideSchemes.crowdControl
        pass
        a <- allyIn "35013"
        choose "ally thwart" (isAllyThwart a)
        choose "side scheme" (isTargetChoice $ sideSchemeOf side)
        choose "Command Center" (responseOf center)
        choose "villain" isVillainChoice
        pass
        resolve [AllyMessage (allyIdOf c) ReadiedAlly]
        second <- revealNow p SideSchemes.crowdControl
        pass
        choose "ally thwart again" (isAllyThwart a)
        choose "second side scheme" (isTargetChoice $ sideSchemeOf second)
        offers (responseOf center) `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 2
