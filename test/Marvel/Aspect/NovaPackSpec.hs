module Marvel.Aspect.NovaPackSpec (spec) where

import Marvel.Attachment.Cards qualified as Attachments
import Marvel.ActiveCost (allocationLabel)
import Marvel.Ability.Types (AbilityType (HeroResponse))
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity, PlayerIdentitySide (..))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Villain.Types (villainStage)
import Marvel.Window qualified as W
import Marvel.Phase
import TestImport

asNova :: IdentityId -> GameAppT ()
asNova p = changeForm p >> pass >> clearHand p

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code resources = do
  card <- putInHand p code
  payment <- traverse (putInHand p) resources
  playWith card payment
  pure card

cardResponse :: PlayerCard -> Choice -> Bool
cardResponse card choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

heroAttack :: IdentityId -> GameAppT Natural
heroAttack p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> Hero.getModifiedAttack (toAttrs h)
  _ -> pure 0

novaAttack :: IdentityId -> GameAppT ()
novaAttack p = do
  choose "basic attack" (isHeroAttack p)
  chooseVillain
  chooseIf isNoResponse

spec :: Spec
spec = describe "Nova pack" do
  describe "The Locust" do
    it "requires Champion and returns a chosen Aggression event when entering play" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        clearHand p
        locust <- putInHand p "28010"
        money <- putInHand p "01088"
        offers (isPlay locust) `shouldReturn` False
        asNova p
        locust' <- putInHand p "28010"
        money' <- putInHand p "01088"
        attack <- putInDiscard p "28014"
        other <- putInDiscard p "28004"
        playWith locust' [money']
        choose "Locust response" (cardResponse locust')
        offers (isTargetChoice other) `shouldReturn` False
        choose "recover aggression event" (isTargetChoice attack)
        handOf p >>= (`shouldSatisfy` elem attack)
        discardOf p >>= (`shouldSatisfy` elem other)

    it "does not offer the Hero response when entering during alter ego" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        _ <- putInDiscard p "28014"
        locust <- putIntoPlay p "28010"
        offers (cardResponse locust) `shouldReturn` False

  describe "Pitchback" do
    it "requires Aerial and responds to a hero attack with a separate four-damage attack" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        card <- putInHand p "28012"
        money <- putInHand p "28007"
        choose "basic attack" (isHeroAttack p)
        chooseVillain
        offers (isPlay card) `shouldReturn` False
        pass
        resolve [IdentityMessage p ReadiedIdentity]
        _ <- putIntoPlay p "28009"
        choose "basic attack" (isHeroAttack p)
        chooseVillain
        playWith card [money]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 6

    it "responds after an attack event, while not being available without an attack window" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        _ <- putIntoPlay p "28009"
        response <- putInHand p "28012"
        money <- putInHand p "28007"
        offers (isPlay response) `shouldReturn` False
        _ <- playPaying p "28005" ["28007"]
        chooseVillain
        playWith response [money]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 8

  describe "No Quarter" do
    it "requires physical and retrieves every Aggression card among excess-damage discards" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        target <- addMinion p Minions.hydraBomber
        resolve [MinionMessage target $ MinionDamaged GameRef $ toDamage 1 FromAbility]
        card <- putInHand p "28013"
        _ <- putInHand p "01089"
        offers (isPlay card) `shouldReturn` False
        physical <- putInHand p "01090"
        top <- stackDeck p ["01052", "01089", "01057"]
        playWith card [physical]
        choose "Hydra Bomber" (isTargetChoice target)
        pass
        minionDamageOf target `shouldReturn` Nothing
        hand <- handOf p
        hand `shouldSatisfy` elem (head top)
        hand `shouldSatisfy` elem (top !! 2)
        hand `shouldSatisfy` notElem (top !! 1)
        discardOf p >>= (`shouldSatisfy` elem (top !! 1))

    it "discards nothing when tough prevents the attack or when it deals no excess" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        tough <- addMinion p Minions.sandman
        top <- stackDeck p ["01052"]
        _ <- playPaying p "28013" ["01090"]
        choose "Sandman" (isTargetChoice tough)
        pass
        playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` (elem (head top) . unDeck))
        _ <- playPaying p "28013" ["01090"]
        chooseVillain
        pass
        playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` (elem (head top) . unDeck))

    it "retrieves for damage already dealt even when an interrupt replaces the enemy's defeat" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        target <- addMinion p Minions.hydraBomber
        resolve [MinionMessage target $ MinionDamaged GameRef $ toDamage 1 FromAbility]
        armor <- encounterCard Attachments.biomechanicalUpgrades
        resolve [PutEncounterCardIntoPlay p armor]
        top <- stackDeck p ["01052", "01089", "01057"]
        _ <- playPaying p "28013" ["01090"]
        choose "hit upgraded Bomber" (isTargetChoice target)
        choose "Biomechanical Upgrades saves it" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId armor)
        pass
        minionDamageOf target `shouldReturn` Just 0
        hand <- handOf p
        hand `shouldSatisfy` elem (head top)
        hand `shouldSatisfy` elem (top !! 2)
        discardOf p >>= (`shouldSatisfy` elem (top !! 1))

    it "counts excess against a defeated villain stage" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 13 FromAbility]
        top <- stackDeck p ["01052", "01089", "01057"]
        _ <- playPaying p "28013" ["01090"]
        chooseVillain
        choose "Rhino II reveal" (isAbilityOf vid)
        pass
        villainStage . toAttrs <$> getVillain `shouldReturn` 2
        hand <- handOf p
        hand `shouldSatisfy` elem (head top)
        hand `shouldSatisfy` elem (top !! 2)

  describe "One by One" do
    it "makes its second attack only after the first defeats its chosen enemy" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        target <- addMinion p Minions.hydraBomber
        _ <- playPaying p "28014" ["28007"]
        choose "first attack" (isTargetChoice target)
        chooseVillain
        pass
        minionDamageOf target `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 2

    it "opens its attack response only once, after both damage instances" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        _ <- putIntoPlay p "28009"
        first <- addMinion p Minions.hydraBomber
        pitch <- putInHand p "28012"
        money <- putInHand p "28007"
        _ <- playPaying p "28014" ["28007"]
        choose "first damage defeats Bomber" (isTargetChoice first)
        offers (isPlay pitch) `shouldReturn` False
        chooseVillain
        playWith pitch [money]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 6

    it "does not make a second attack when the first enemy survives or tough prevents its damage" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        _ <- playPaying p "28014" ["28007"]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        tough <- addMinion p Minions.sandman
        _ <- playPaying p "28014" ["28007"]
        choose "tough enemy" (isTargetChoice tough)
        pass
        minionDamageOf tough `shouldReturn` Just 0
        villainDamageNow `shouldReturn` 2

    it "does not deal its second damage when an interrupt saves the first enemy from defeat" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        target <- addMinion p Minions.hydraBomber
        armor <- encounterCard Attachments.biomechanicalUpgrades
        resolve [PutEncounterCardIntoPlay p armor]
        _ <- playPaying p "28014" ["28007"]
        choose "hit upgraded Bomber" (isTargetChoice target)
        choose "Biomechanical Upgrades saves it" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId armor)
        pass
        minionDamageOf target `shouldReturn` Just 0
        villainDamageNow `shouldReturn` 0

    it "allows its second attack after defeating a villain stage" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 13 FromAbility]
        _ <- playPaying p "28014" ["28007"]
        chooseVillain
        choose "Rhino II reveal" (isAbilityOf vid)
        pass
        chooseVillain
        pass
        villainStage . toAttrs <$> getVillain `shouldReturn` 2
        villainDamageNow `shouldReturn` 2

  describe "Fluid Motion" do
    it "exhausts at most one copy for each Attack event and stacks until phase end" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        first <- putIntoPlay p "28016"
        second <- putIntoPlay p "28016"
        _ <- playPaying p "28005" ["28007"]
        chooseVillain
        choose "first Fluid Motion" (cardResponse first)
        offers (cardResponse second) `shouldReturn` False
        pass
        heroAttack p `shouldReturn` 2
        upgradeIsExhausted (upgradeIdOf first) `shouldReturn` True
        _ <- playPaying p "28014" ["28007"]
        chooseVillain
        choose "second Fluid Motion for the next event" (cardResponse second)
        pass
        heroAttack p `shouldReturn` 3
        runNow [EndPhase PlayerPhase]
        heroAttack p `shouldReturn` 1

    it "does not respond to an event without the Attack trait" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        motion <- putIntoPlay p "28016"
        placeMainThreat 4
        _ <- playPaying p "28004" ["28007"]
        chooseMainScheme
        offers (cardResponse motion) `shouldReturn` False

  describe "Honed Technique" do
    it "requires two mental resources and enforces max one per player" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        technique <- putInHand p "28017"
        _ <- putInHand p "01088"
        _ <- putInHand p "28004"
        offers (isPlay technique) `shouldReturn` False
        _ <- putInHand p "28004"
        offers (isPlay technique) `shouldReturn` True
        _ <- putIntoPlay p "28017"
        offers (isPlay technique) `shouldReturn` False

    it "adds printed cost to each damage instance when the Aggression event was paid with mental" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        technique <- putIntoPlay p "28017"
        target <- addMinion p Minions.hydraMercenary
        _ <- playPaying p "28014" ["28004"]
        choose "Honed Technique" (cardResponse technique)
        choose "first attack boosted to three" (isTargetChoice target)
        chooseVillain
        pass
        minionDamageOf target `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 3
        _ <- playPaying p "28014" ["28007"]
        offers (cardResponse technique) `shouldReturn` False
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5

    it "does not trigger when generated mental resources were surplus and not actually spent" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        technique <- putIntoPlay p "28017"
        event <- putInHand p "28014"
        energy <- putInHand p "01088"
        mental <- putInHand p "01089"
        choose "play One by One" (isPlay event)
        choose "generate energy" (isPayWith energy)
        choose "generate surplus mental" (isPayWith mental)
        whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
        choose "spend energy only" (isLabel $ allocationLabel [Energy])
        offers (cardResponse technique) `shouldReturn` False
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2

    it "accepts a declared mental wild but ignores hero-specific attacks and undeclared wild" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        technique <- putIntoPlay p "28017"
        event <- putInHand p "28014"
        wild <- putInHand p "28007"
        playDeclaring event [wild] Mental
        choose "Honed Technique" (cardResponse technique)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        _ <- playPaying p "28005" ["28007"]
        offers (cardResponse technique) `shouldReturn` False
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 7

  describe "Moon Girl" do
    it "requires Champion or Genius and draws one card per mental resource actually spent" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        clearHand p
        moon <- putInHand p "28018"
        _ <- putInHand p "01089"
        _ <- putInHand p "28004"
        offers (isPlay moon) `shouldReturn` False
        asNova p
        moon' <- putInHand p "28018"
        mental <- putInHand p "01089"
        energy <- putInHand p "01088"
        choose "play Moon Girl" (isPlay moon')
        choose "pay mental" (isPayWith mental)
        choose "pay energy" (isPayWith energy)
        whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
        choose "spend only one mental" (isLabel $ allocationLabel [Mental, Energy, Energy])
        before <- length <$> handOf p
        choose "Moon Girl response" (cardResponse moon')
        pass
        (length <$> handOf p) `shouldReturn` before + 1

    it "counts a wild declared mental among the resources paid" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        moon <- putInHand p "28018"
        genius <- putInHand p "01089"
        wild <- putInHand p "28007"
        playDeclaring moon [genius, wild] Mental
        before <- length <$> handOf p
        choose "Moon Girl response" (cardResponse moon)
        pass
        (length <$> handOf p) `shouldReturn` before + 3

    it "does not draw when put into play rather than played from hand" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        before <- length <$> handOf p
        moon <- putIntoPlay p "28018"
        offers (cardResponse moon) `shouldReturn` False
        (length <$> handOf p) `shouldReturn` before

  describe "Everyday Hero" do
    it "lets a Civilian contribute only Everyday Hero and optionally heals the receiving identity" do
      tg <- testGame ["spider-man", "nova"]
      let p = player1 tg
          helper = player2 tg
      inGame tg do
        asNova p
        clearHand helper
        setDamage p 2
        setDamage helper 2
        card <- putInHand p "28027"
        everyday <- putInHand helper "28019"
        ordinary <- putInHand helper "01089"
        choose "play Height Advantage" (isPlay card)
        choose "invite Sam" (isTargetChoice helper)
        fmap fst <$> asked `shouldReturn` Just helper
        offers (isPayWith ordinary) `shouldReturn` False
        choose "contribute Everyday Hero" (isPayWith everyday)
        chooseIf (isLabel "$label.doneContributing")
        declareWildAs Wild
        choose "finish paying" isFinishPayment
        choose "heal recipient" (isLabel "$label.everydayHeroHeal")
        pass
        damageOf p `shouldReturn` 1
        damageOf helper `shouldReturn` 2
        discardOf helper >>= (`shouldSatisfy` elem everyday)

    it "doubles a Civilian helper's wild for Pot Shot and lets that helper declare both resources" do
      tg <- testGame ["nova", "ghost-spider"]
      let p = player1 tg
          helper = player2 tg
      inGame tg do
        asNova p
        clearHand helper
        setDamage p 3
        setDamage helper 3
        shot <- putInHand p "28005"
        everyday <- putInHand helper "28019"
        offers (isPlay shot) `shouldReturn` True
        choose "play Pot Shot" (isPlay shot)
        choose "invite Gwen" (isTargetChoice helper)
        choose "contribute Everyday Hero" (isPayWith everyday)
        chooseIf (isLabel "$label.doneContributing")
        fmap fst <$> asked `shouldReturn` Just helper
        declareWildAs Physical
        fmap fst <$> asked `shouldReturn` Just helper
        declareWildAs Mental
        choose "finish paying" isFinishPayment
        choose "heal recipient" (isLabel "$label.everydayHeroHeal")
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4
        damageOf p `shouldReturn` 2
        damageOf helper `shouldReturn` 3
        discardOf helper >>= (`shouldSatisfy` elem everyday)

    it "offers no healing while the spender is a hero" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        setDamage p 2
        _ <- playPaying p "28027" ["28019"]
        offers (isLabel "$label.everydayHeroHeal") `shouldReturn` False
        damageOf p `shouldReturn` 2

    it "lets the Civilian decline the healing response when spending for themself" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        clearHand p
        setDamage p 2
        _ <- playPaying p "28027" ["28019"]
        choose "decline healing" isNoResponse
        pass
        damageOf p `shouldReturn` 2

  describe "Champions Mobile Bunker" do
    it "exhausts to offer a Champion two draws followed by two discards" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        bunker <- putIntoPlay p "28020"
        top <- stackDeck p ["01089", "01088"]
        choose "use bunker" (cardResponse bunker)
        choose "choose Nova" (isTargetChoice p)
        choose "draw" (isLabel "$label.championsMobileBunkerDraw")
        choose "discard first drawn card" (isTargetChoice $ head top)
        choose "discard second drawn card" (isTargetChoice $ top !! 1)
        pass
        handOf p `shouldReturn` []
        (supportExhausted . head) <$> supportsWith "28020" `shouldReturn` True

    it "allows the selected Champion to decline and cannot be used in alter ego" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        bunker <- putIntoPlay p "28020"
        offers (cardResponse bunker) `shouldReturn` False
        asNova p
        choose "bunker" (cardResponse bunker)
        choose "choose Nova" (isTargetChoice p)
        before <- length <$> handOf p
        choose "decline" isNoResponse
        (length <$> handOf p) `shouldReturn` before

  describe "Yaw and Roll" do
    it "requires Aerial and removes three threat after a labeled hero thwart" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        _ <- putIntoPlay p "28009"
        placeMainThreat 6
        yaw <- putInHand p "28026"
        money <- putInHand p "28007"
        offers (isPlay yaw) `shouldReturn` False
        _ <- playPaying p "28004" ["28007"]
        chooseMainScheme
        playWith yaw [money]
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` 0

    it "responds to basic thwart but is unavailable without Aerial" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        placeMainThreat 6
        yaw <- putInHand p "28026"
        money <- putInHand p "28007"
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        offers (isPlay yaw) `shouldReturn` False
        pass
        resolve [IdentityMessage p ReadiedIdentity]
        _ <- putIntoPlay p "28009"
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        playWith yaw [money]
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` 1

  describe "Height Advantage" do
    it "reduces each enemy attack while Aerial, but not ability damage or attacks without Aerial" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        helmet <- putIntoPlay p "28009"
        _ <- putIntoPlay p "28027"
        enemy <- addMinion p Minions.hydraMercenary
        runNow [MinionMessage enemy $ MinionAttacks p]
        choose "take attack" isNoDefender
        pass
        damageOf p `shouldReturn` 0
        resolve [IdentityMessage p $ IdentityDamaged GameRef $ toDamage 1 FromAbility]
        damageOf p `shouldReturn` 1
        resolve [RemoveFromPlay $ toRef $ upgradeIdOf helmet]
        runNow [MinionMessage enemy $ MinionAttacks p]
        choose "take attack without Aerial" isNoDefender
        pass
        damageOf p `shouldReturn` 2

    it "is forcibly discarded at the start of its controller's turn" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        height <- putIntoPlay p "28027"
        runNow [IdentityMessage p BeginTurn]
        choose "forced discard" (cardResponse height)
        pass
        discardOf p >>= (`shouldSatisfy` elem height)
