module Marvel.Aspect.ThorPackSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Marvel.Game.Public (publicGame)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

thor :: IO TestGame
thor = testGame ["thor"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

inPlay :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
inPlay p code codes = do
  card <- putInHand p code
  playPaying p card codes
  pure card

-- | A card from hand played on the target, paying with new copies of the cards given.
_inPlayOn :: (IsRef a) => IdentityId -> CardCode -> a -> [CardCode] -> GameAppT PlayerCard
_inPlayOn p code target codes' = do
  card <- putInHand p code
  payment <- traverse (putInHand p) codes'
  playOn card target payment
  pure card

codes :: [PlayerCard] -> [CardCode]
codes = map toCardCode

engagedWith :: IdentityId -> GameAppT [MinionId]
engagedWith p = toList . (.playerIdentityMinions) <$> playerAttrs p

spec :: Spec
spec = describe "The Thor pack's aspect and basic cards" do
  describe "Hercules (06011)" do
    it "costs 1 less for each minion engaged with you" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- addMinion p Minions.hydraMercenary
        _ <- addMinion p Minions.hydraMercenary
        hercules <- putInHand p "06011"
        -- each Invulnerability pays 1 resource
        _ <- traverse (putInHand p) ["06021", "06021", "06021"]
        offers (isPlay hercules) `shouldReturn` False
        _ <- putInHand p "06021"
        offers (isPlay hercules) `shouldReturn` True

  describe "Valkyrie (06012)" do
    it "deals 2 damage to a minion as she enters play" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        mercenary <- addMinion p Minions.loki
        valkyrie <- putInHand p "06012"
        playPaying p valkyrie ["01089", "01090"]
        choose "valkyrie" (isAbilityOfCard valkyrie)
        choose "the minion" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Just 2

    it "deals 3 instead when paid for with an energy resource" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        loki <- addMinion p Minions.loki
        valkyrie <- putInHand p "06012"
        playPaying p valkyrie ["01088", "01089"]
        choose "valkyrie" (isAbilityOfCard valkyrie)
        choose "the minion" (isTargetChoice loki)
        minionDamageOf loki `shouldReturn` Just 3

    it "deals 3 when a wild resource is declared as energy" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        loki <- addMinion p Minions.loki
        valkyrie <- putInHand p "06012"
        -- The Power of Aggression doubles to two wild resources for an Aggression card
        payment <- traverse (putInHand p) ["01055", "01089"]
        playDeclaring valkyrie payment Energy
        choose "valkyrie" (isAbilityOfCard valkyrie)
        choose "the minion" (isTargetChoice loki)
        minionDamageOf loki `shouldReturn` Just 3

    it "deals 2 when the wild resource is declared as another type" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        loki <- addMinion p Minions.loki
        valkyrie <- putInHand p "06012"
        payment <- traverse (putInHand p) ["01055", "01089"]
        choose "play the card" (isPlay valkyrie)
        for_ payment \c -> choose "pay" (isPayWith c)
        -- declared as energy, then again as physical: one wild is one type
        declareWildAs Energy
        declareWildAs Physical
        declareWildAs Physical
        choose "finish paying" isFinishPayment
        choose "valkyrie" (isAbilityOfCard valkyrie)
        choose "the minion" (isTargetChoice loki)
        minionDamageOf loki `shouldReturn` Just 2

  describe "Get Over Here! (06014)" do
    it "deals 1 damage to a minion, and an Aerial hero engages it" do
      tg <- testGame ["thor", "she-hulk"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        changeForm p
        clearHand p
        _ <- inPlay p "06009" ["01088"]
        loki <- addMinion other Minions.loki
        pass
        getOver <- putInHand p "06014"
        choose "play" (isPlay getOver)
        choose "Loki" (isTargetChoice (EnemyMinionId loki))
        chooseIf (isNamedAbility "\"Have at thee!\"")
        pass
        minionDamageOf loki `shouldReturn` Just 1
        engagedWith p `shouldReturn` [loki]
        engagedWith other `shouldReturn` []

    it "without Aerial, the minion stays where it is" do
      tg <- testGame ["thor", "she-hulk"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        changeForm p
        clearHand p
        loki <- addMinion other Minions.loki
        pass
        getOver <- putInHand p "06014"
        choose "play" (isPlay getOver)
        choose "Loki" (isTargetChoice (EnemyMinionId loki))
        pass
        engagedWith other `shouldReturn` [loki]

    it "a stunned hero may play it with no minion to attack, and the stun is removed" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        resolve [IdentityMessage p IdentityStunned]
        getOver <- putInHand p "06014"
        choose "play" (isPlay getOver)
        pass
        isStunned p `shouldReturn` False

  describe "Mean Swing (06015)" do
    it "exhausts a Weapon for +3 ATK on the basic attack" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        mjolnir <- inPlay p "06009" ["01088"]
        swing <- putInHand p "06015"
        _ <- putInHand p "01090"
        choose "attack" (isHeroAttack p)
        choose "mean swing" (isPlay swing)
        whenM (offers isFinishPayment) $ choose "pay" isFinishPayment
        pass
        vid <- villainIdNow
        choose "attack Rhino" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 6
        upgradeIsExhausted (upgradeOf mjolnir) `shouldReturn` True

    it "needs a ready Weapon" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        swing <- putInHand p "06015"
        _ <- putInHand p "01090"
        choose "attack" (isHeroAttack p)
        offers (isPlay swing) `shouldReturn` False

  describe "Hall of Heroes (06017)" do
    it "gains a glory counter as you defeat a minion; at 3, an alter-ego action draws 3" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        hall <- inPlay p "06017" ["01088", "01089"]
        for_ [1 :: Int .. 3] \_ -> do
          m <- addMinion p Minions.hydraMercenary
          runNow [MinionMessage m (MinionDamaged (IdentityRef p) (toDamage 3 $ FromPlayerAttack p))]
          choose "glory" (isAbilityOfCard hall)
          pass
        (map (.supportUses) <$> supportsWith "06017") `shouldReturn` [3]
        choose "draw 3" (isAbilityOfCard hall)
        length <$> handOf p `shouldReturn` 3
        (map (.supportUses) <$> supportsWith "06017") `shouldReturn` [0]

    it "gains no glory counter when an ally defeats the minion" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        hall <- inPlay p "06017" ["01088", "01089"]
        _ <- putIntoPlay p "06002"
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        resolve [MinionMessage mercenary (MinionDamaged GameRef (toDamage 1 FromAbility))]
        [sifAttrs] <- alliesWith "06002"
        choose "Lady Sif attacks" (isAllyAttack sifAttrs)
        choose "the target" (isTargetChoice mercenary)
        offers (isAbilityOfCard hall) `shouldReturn` False
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        (map (.supportUses) <$> supportsWith "06017") `shouldReturn` [0]

  describe "Battle Fury (06018)" do
    it "after your hero attacks and defeats a minion, take 1 damage and discard it to ready" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        fury <- inPlay p "06018" ["01088"]
        mercenary <- addMinion p Minions.hydraMercenary
        pass
        resolve [MinionMessage mercenary (MinionDamaged GameRef (toDamage 1 FromAbility))]
        choose "attack" (isHeroAttack p)
        choose "attack the mercenary" (isTargetChoice mercenary)
        choose "battle fury" (isAbilityOfCard fury)
        pass
        exhaustedNow p `shouldReturn` False
        damageOf p `shouldReturn` 1
        filter (== "06018") . codes <$> discardOf p `shouldReturn` ["06018"]

  describe "Jarnbjorn (06019)" do
    it "after the hero attacks, spend a physical resource for 2 damage to an enemy" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        jarnbjorn <- inPlay p "06019" ["01090"]
        strength <- putInHand p "01090"
        choose "attack" (isHeroAttack p)
        vid <- villainIdNow
        choose "attack Rhino" (isTargetChoice vid)
        choose "jarnbjorn" (isAbilityOfCard jarnbjorn)
        choose "pay" (isPayWith strength)
        whenM (offers isFinishPayment) $ choose "pay" isFinishPayment
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4

    it "may be triggered after an attack event, as the hero attacked (FAQ)" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        _ <- inPlay p "06009" ["01088"]
        jarnbjorn <- inPlay p "06019" ["01090"]
        throw <- putInHand p "06005"
        payment <- traverse (putInHand p) ["01088", "01089", "01090"]
        strength <- putInHand p "01090"
        playWith throw payment
        vid <- villainIdNow
        choose "throw at Rhino" (isTargetChoice vid)
        choose "jarnbjorn" (isAbilityOfCard jarnbjorn)
        choose "pay" (isPayWith strength)
        whenM (offers isFinishPayment) $ choose "pay" isFinishPayment
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 10

  describe "Heimdall (06020)" do
    it "looks at the top 3 encounter cards: 1 is discarded and the others go back in the order chosen" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        [a, b, c] <- stackEncounterDeck [Treacheries.advance, Minions.hydraMercenary, Minions.loki]
        heimdall <- putInHand p "06020"
        playPaying p heimdall ["01088", "01089", "01090"]
        _ <- putInHand p "01088"
        choose "heimdall" (isAbilityOfCard heimdall)
        choose "discard Advance" (isTargetChoice a)
        choose "Loki on top" (isTargetChoice c)
        take 2 <$> scenarioEncounterDeckNow `shouldReturn` [c, b]
        map ecCardId <$> encounterDiscardNow >>= (`shouldSatisfy` elem (ecCardId a))

  describe "Invulnerability (06021)" do
    it "gives the hero a tough status card, which prevents the next damage" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        invulnerable <- putInHand p "06021"
        playPaying p invulnerable ["01088", "01089"]
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
        resolve [IdentityMessage p (IdentityDamaged GameRef (toDamage 3 FromAbility))]
        damageOf p `shouldReturn` 0
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False

  describe "Under Surveillance (06031)" do
    it "increases the main scheme's target threat by 4, one per scheme" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        m <- mainSchemeId
        -- attached to the main scheme without asking: there is only the one
        _ <- inPlay p "06031" ["01088", "01089"]
        -- Rhino's first main scheme advances at 7 threat
        resolve [MainSchemeMessage m (MainSchemeThwarted GameRef 99), MainSchemeMessage m (MainSchemePlaceThreat 10)]
        mainSchemeThreat `shouldReturn` 10
        another <- putInHand p "06031"
        _ <- traverse (putInHand p) ["01088", "01089"]
        offers (isPlay another) `shouldReturn` False

    it "is attached to the main scheme, and discarded as that scheme leaves play" do
      -- Klaw's first main scheme advances to a second
      tg <- testGameAgainst "01113" ["thor"]
      let p = player1 tg
      inGame tg do
        clearHand p
        m <- mainSchemeId
        surveillance <- inPlay p "06031" ["01088", "01089"]
        map (.upgradeAttachedMainScheme) <$> upgradesWith "06031" `shouldReturn` [Just m]
        -- shown on the main scheme, not in the player's area
        view <- liftIO . publicGame =<< getGame
        let onMainScheme =
              [ c
              | Object o <- [view]
              , Just (Array schemes) <- [KeyMap.lookup "mainSchemes" o]
              , Object scheme <- toList schemes
              , Just (Array upgrades) <- [KeyMap.lookup "upgrades" scheme]
              , Object u <- toList upgrades
              , Just (String c) <- [KeyMap.lookup "code" u]
              ]
        onMainScheme `shouldBe` ["06031"]
        resolve [MainSchemeMessage m (MainSchemePlaceThreat 99)]
        mainSchemeId >>= (`shouldSatisfy` (/= m))
        upgradesWith "06031" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem surveillance)

  describe "Teamwork (06032)" do
    it "exhausts an ally to add its ATK to the hero's basic attack" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        sif <- putIntoPlay p "06002"
        teamwork <- putInHand p "06032"
        _ <- putInHand p "01088"
        choose "attack" (isHeroAttack p)
        choose "teamwork" (isPlay teamwork)
        whenM (offers isFinishPayment) $ choose "pay" isFinishPayment
        choose "the target" (isTargetChoice (allyIdOf sif))
        pass
        vid <- villainIdNow
        choose "attack Rhino" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 4

    it "exhausts an ally to add its THW to the hero's basic thwart" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        placeMainThreat 5
        before <- mainSchemeThreat
        sif <- putIntoPlay p "06002"
        teamwork <- putInHand p "06032"
        _ <- putInHand p "01088"
        choose "thwart" (isHeroThwart p)
        choose "teamwork" (isPlay teamwork)
        whenM (offers isFinishPayment) $ choose "pay" isFinishPayment
        choose "the target" (isTargetChoice (allyIdOf sif))
        pass
        m <- mainSchemeId
        choose "the target" (isTargetChoice m)
        mainSchemeThreat `shouldReturn` (before - 3)

  describe "Second Wind (06033)" do
    it "heals 4 damage, 5 if paid for with a mental resource" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        setDamage p 10
        wind <- putInHand p "06033"
        playPaying p wind ["01088", "01090"]
        damageOf p `shouldReturn` 6
        wind' <- putInHand p "06033"
        playPaying p wind' ["01089", "01090"]
        damageOf p `shouldReturn` 1

  describe "Enhanced Physique (06034)" do
    it "pays a physical resource 3 times, then is discarded" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        physique <- inPlay p "06034" ["01088", "01089"]
        (map (.upgradeUses) <$> upgradesWith "06034") `shouldReturn` [3]
        resolve [UpgradeMessage (upgradeOf physique) SpendUpgradeUse, UpgradeMessage (upgradeOf physique) SpendUpgradeUse]
        jarnbjorn <- putInHand p "06019"
        choose "play Jarnbjorn" (isPlay jarnbjorn)
        choose "pay with Enhanced Physique" (isAbilityOfCard physique)
        upgradesWith "06034" `shouldReturn` []
        upgradeCodes >>= (`shouldSatisfy` elem "06019")
        getsGame (HashMap.size . gameUpgrades) `shouldReturn` 1
