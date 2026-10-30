module Marvel.Hero.NovaSpec (spec, nova, asNova) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity, PlayerIdentitySide (..))
import Marvel.Ally.Types qualified as Ally
import Marvel.Card.Def (CardDef (..))
import Marvel.Card.Side (Side (..))
import Marvel.Matchers (identityMatches, IdentityMatcher (IdentityWithTrait))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Hp (unHp)
import Marvel.Villain.Types (villainHp)
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Trait (Trait (Aerial))
import TestImport

nova :: IO TestGame
nova = testGame ["nova"]

asNova :: IdentityId -> GameAppT ()
asNova p = clearHand p >> changeForm p >> pass

isIdentityAbility :: IdentityId -> Natural -> Choice -> Bool
isIdentityAbility p n = \case
  UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == n
  _ -> False

isCardAbility :: PlayerCard -> Natural -> Choice -> Bool
isCardAbility card n choice = case choice of
  UseAbility ab -> isAbilityOfCard card choice && abilityIndex ab == n
  _ -> False

attackVillain :: IdentityId -> GameAppT ()
attackVillain p = choose "basic attack" (isHeroAttack p) >> chooseVillain

paying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
paying p card codes = traverse (putInHand p) codes >>= playWith card

removeHelmetFromZones :: IdentityId -> GameAppT ()
removeHelmetFromZones p = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  let keep = filter ((/= "28009") . toCardCode . getCardDef)
   in PlayerIdentity a
        { playerIdentityHand = Hand $ keep $ unHand $ playerIdentityHand a
        , playerIdentityDeck = Deck $ keep $ unDeck $ playerIdentityDeck a
        , playerIdentityDiscard = Discard $ keep $ unDiscard $ playerIdentityDiscard a
        }

useForce :: IdentityId -> GameAppT ()
useForce p = do
  event <- putInHand p "28006"
  paying p event ["28007"]
  pass

spec :: Spec
spec = describe "Nova" do
  describe "Nova and Supernova Helmet" do
    it "gives only Nova's hero form Aerial and generates a wild resource only in hero form" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        clearHand p
        helmet <- putIntoPlay p "28009"
        identityMatches (IdentityWithTrait Aerial) p `shouldReturn` False
        shot <- putInHand p "28005"
        offers (isPlay shot) `shouldReturn` False
        changeForm p
        pass
        identityMatches (IdentityWithTrait Aerial) p `shouldReturn` True
        offers (isPlay shot) `shouldReturn` True
        choose "play Pot Shot" (isPlay shot)
        choose "generate wild with the Helmet" (isCardAbility helmet 1)
        offers isFinishPayment `shouldReturn` True
        choose "declare the first doubled wild mental" (== DeclareWild 0 Mental)
        choose "declare the second doubled wild physical" (== DeclareWild 1 Physical)
        choose "declare only the second resource wild again" (== DeclareWild 1 Wild)
        offers (== DeclareWild 0 Mental) `shouldReturn` False
        offers (== DeclareWild 1 Physical) `shouldReturn` True
        payWith []
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4
        upgradeIsExhausted (upgradeOf helmet) `shouldReturn` True
        resolve [IdentityMessage p (ChangedToForm B)]
        identityMatches (IdentityWithTrait Aerial) p `shouldReturn` False

    it "readies the Helmet after each basic attack and basic thwart without a phase limit" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        helmet <- putIntoPlay p "28009"
        resolve [UpgradeMessage (upgradeOf helmet) ExhaustedUpgrade]
        attackVillain p
        choose "Nova readies the Helmet" (isIdentityAbility p 1)
        upgradeIsExhausted (upgradeOf helmet) `shouldReturn` False
        exhaustedNow p `shouldReturn` True
        resolve [IdentityMessage p ReadiedIdentity, UpgradeMessage (upgradeOf helmet) ExhaustedUpgrade]
        placeMainThreat 2
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        choose "Nova readies the Helmet again" (isIdentityAbility p 1)
        upgradeIsExhausted (upgradeOf helmet) `shouldReturn` False

    it "readies the Helmet after Nova defends" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        helmet <- putIntoPlay p "28009"
        resolve [UpgradeMessage (upgradeOf helmet) ExhaustedUpgrade]
        _ <- stackEncounterDeck [Treacheries.imTough]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "defend with Nova" isDefend
        choose "ready the Helmet after defense" (isIdentityAbility p 1)
        upgradeIsExhausted (upgradeOf helmet) `shouldReturn` False
        exhaustedNow p `shouldReturn` True

    it "does not trigger for a stunned attack, a confused thwart, or an event attack" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        helmet <- putIntoPlay p "28009"
        resolve [UpgradeMessage (upgradeOf helmet) ExhaustedUpgrade, IdentityMessage p IdentityStunned]
        choose "stunned attack" (isHeroAttack p)
        offers (isIdentityAbility p 1) `shouldReturn` False
        resolve [IdentityMessage p ReadiedIdentity, IdentityMessage p IdentityConfused]
        placeMainThreat 2
        choose "confused thwart" (isHeroThwart p)
        offers (isIdentityAbility p 1) `shouldReturn` False
        shot <- putInHand p "28005"
        paying p shot ["28007"]
        chooseVillain
        offers (isIdentityAbility p 1) `shouldReturn` False
        upgradeIsExhausted (upgradeOf helmet) `shouldReturn` True

    it "does not offer to ready a ready Helmet or one blocked by Weight of the World" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        helmet <- putIntoPlay p "28009"
        attackVillain p
        offers (isIdentityAbility p 1) `shouldReturn` False
        pass
        resolve [IdentityMessage p ReadiedIdentity, UpgradeMessage (upgradeOf helmet) ExhaustedUpgrade]
        _ <- revealNow p Obligations.weightOfTheWorld
        pass
        attackVillain p
        offers (isIdentityAbility p 1) `shouldReturn` False
        upgradeIsExhausted (upgradeOf helmet) `shouldReturn` True

  describe "Sam Alexander" do
    it "searches the discard pile and adds the Helmet to hand with a nonwild payment" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        clearHand p
        removeHelmetFromZones p
        helmet <- putInDiscard p "28009"
        payment <- putInHand p "28003"
        choose "Sam searches for the Helmet" (isIdentityAbility p 1)
        payWith [payment]
        choose "take the Helmet" (isTargetChoice helmet)
        pass
        handOf p >>= (`shouldSatisfy` elem helmet)
        upgradeCodes `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` notElem helmet)

    it "puts the searched Helmet directly into play when a wild is spent as wild" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        clearHand p
        removeHelmetFromZones p
        helmet <- putInDiscard p "28009"
        payment <- putInHand p "28007"
        choose "Sam searches for the Helmet" (isIdentityAbility p 1)
        payWith [payment]
        pass
        upgradeCodes `shouldReturn` ["28009"]
        handOf p >>= (`shouldSatisfy` notElem helmet)
        discardOf p >>= (`shouldSatisfy` notElem helmet)

    it "searches the deck and treats a wild declared physical as a nonwild payment" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        clearHand p
        removeHelmetFromZones p
        [helmet] <- stackDeck p ["28009"]
        payment <- putInHand p "28007"
        choose "Sam searches for the Helmet" (isIdentityAbility p 1)
        choose "spend Worldmind" (isPayWith payment)
        declareWildAs Physical
        payWith []
        choose "take the Helmet into hand" (isTargetChoice helmet)
        handOf p >>= (`shouldSatisfy` elem helmet)
        upgradeCodes `shouldReturn` []

    it "is an alter-ego action and may be used again after returning the Helmet to the discard pile" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        clearHand p
        removeHelmetFromZones p
        helmet <- putInDiscard p "28009"
        first <- putInHand p "28003"
        second <- putInHand p "28003"
        choose "Sam's first search" (isIdentityAbility p 1)
        payWith [first]
        choose "take Helmet" (isTargetChoice helmet)
        resolve [DiscardedCard (PlayerCard helmet)]
        offers (isIdentityAbility p 1) `shouldReturn` True
        choose "Sam's second search" (isIdentityAbility p 1)
        payWith [second]
        choose "take Helmet again" (isTargetChoice helmet)
        changeForm p
        pass
        offers (isIdentityAbility p 1) `shouldReturn` False

  describe "Ms. Marvel" do
    it "exhausts and takes one damage to return the exact played event copy from discard" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        msMarvel <- putIntoPlay p "28002"
        other <- putInDiscard p "28005"
        shot <- putInHand p "28005"
        paying p shot ["28007"]
        chooseVillain
        choose "Ms. Marvel returns the event" (isCardAbility msMarvel 1)
        handOf p >>= (`shouldSatisfy` elem shot)
        discardOf p >>= (`shouldSatisfy` elem other)
        Ally.allyDamage <$> allyIn "28002" `shouldReturn` 1
        Ally.allyExhausted <$> allyIn "28002" `shouldReturn` True
        second <- putInHand p "28005"
        paying p second ["28007"]
        chooseVillain
        offers (isCardAbility msMarvel 1) `shouldReturn` False

    it "can pay with her last hit point and still return the event" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        msMarvel <- putIntoPlay p "28002"
        resolve [AllyMessage (allyIdOf msMarvel) (AllyDamaged GameRef $ toDamage 2 FromAbility)]
        shot <- putInHand p "28005"
        paying p shot ["28007"]
        chooseVillain
        choose "Ms. Marvel pays her last hit point" (isCardAbility msMarvel 1)
        handOf p >>= (`shouldSatisfy` elem shot)
        alliesWith "28002" `shouldReturn` []

    it "cannot pay her damage cost with tough, and cannot return an event that returned itself to hand" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        msMarvel <- putIntoPlay p "28002"
        resolve [GiveStatus (toRef $ allyIdOf msMarvel) ToughStatus]
        shot <- putInHand p "28005"
        paying p shot ["28007"]
        chooseVillain
        offers (isCardAbility msMarvel 1) `shouldReturn` False
        pass
        resolve [RemoveStatus (toRef $ allyIdOf msMarvel) ToughStatus, EndRound]
        clobber <- putInHand p "18012"
        paying p clobber ["01090"]
        chooseVillain
        handOf p >>= (`shouldSatisfy` elem clobber)
        offers (isCardAbility msMarvel 1) `shouldReturn` False

  describe "Forcefield Projection" do
    it "prevents three attack damage and deals three ability damage when paid with wild" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        event <- putInHand p "28003"
        payment <- putInHand p "28007"
        runNow $ damageIdentity p GameRef (toDamage 5 FromAttack)
        playWith event [payment]
        chooseVillain
        pass
        damageOf p `shouldReturn` 2
        villainDamageNow `shouldReturn` 3

    it "prevents attack damage to a friendly ally and has no damage bonus for nonwild payment" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        ally <- putIntoPlay p "01084"
        event <- putInHand p "28003"
        payment <- putInHand p "28004"
        runNow $ damageAlly (allyIdOf ally) GameRef (toDamage 4 FromAttack)
        playWith event [payment]
        pass
        Ally.allyDamage <$> allyIn "01084" `shouldReturn` 1
        villainDamageNow `shouldReturn` 0

    it "does not respond to nonattack damage or treat a wild declared mental as wild" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        event <- putInHand p "28003"
        payment <- putInHand p "28007"
        runNow $ damageIdentity p GameRef (toDamage 1 FromAbility)
        offers (isPlay event) `shouldReturn` False
        pass
        runNow $ damageIdentity p GameRef (toDamage 2 FromAttack)
        playDeclaring event [payment] Mental
        pass
        damageOf p `shouldReturn` 1
        villainDamageNow `shouldReturn` 0

  describe "Lightspeed Flight and Pot Shot" do
    it "removes three threat using a single printed wild doubled for Lightspeed Flight" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        placeMainThreat 5
        before <- mainSchemeThreatNow
        flight <- putInHand p "28004"
        paying p flight ["28007"]
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` (before - 3)

    it "pays for Lightspeed Flight with only the Helmet's generated wild, doubled and independently declared" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        helmet <- putIntoPlay p "28009"
        placeMainThreat 5
        before <- mainSchemeThreatNow
        flight <- putInHand p "28004"
        offers (isPlay flight) `shouldReturn` True
        choose "play Lightspeed Flight" (isPlay flight)
        choose "exhaust the sole resource source" (isCardAbility helmet 1)
        upgradeIsExhausted (upgradeOf helmet) `shouldReturn` True
        offers isFinishPayment `shouldReturn` True
        choose "declare the first generated wild mental" (== DeclareWild 0 Mental)
        choose "declare the second generated wild energy" (== DeclareWild 1 Energy)
        choose "return only the second resource to wild" (== DeclareWild 1 Wild)
        offers (== DeclareWild 0 Mental) `shouldReturn` False
        offers (== DeclareWild 1 Energy) `shouldReturn` True
        payWith []
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` (before - 3)
        upgradeIsExhausted (upgradeOf helmet) `shouldReturn` True

    it "deals four damage using one printed wild doubled for Pot Shot" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        shot <- putInHand p "28005"
        paying p shot ["28007"]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4

    it "requires two ordinary resources, and a stun replaces Pot Shot's attack after payment" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        shot <- putInHand p "28005"
        a <- putInHand p "28003"
        offers (isPlay shot) `shouldReturn` False
        b <- putInHand p "28004"
        offers (isPlay shot) `shouldReturn` True
        resolve [IdentityMessage p IdentityStunned]
        playWith shot [a, b]
        pass
        villainDamageNow `shouldReturn` 0
        isStunned p `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` elem shot)

    it "a confusion replaces Lightspeed Flight's thwart after payment" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        placeMainThreat 4
        before <- mainSchemeThreatNow
        resolve [IdentityMessage p IdentityConfused]
        flight <- putInHand p "28004"
        paying p flight ["28007"]
        pass
        mainSchemeThreatNow `shouldReturn` before
        isConfused p `shouldReturn` False

  describe "Unleash Nova Force" do
    it "readies Nova and draws on his enemy defeats, and has a shared maximum once per round" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        minion <- addMinion p Minions.hydraBomber
        resolve [MinionMessage minion (MinionDamaged GameRef $ toDamage 1 FromAbility)]
        useForce p
        clearHand p
        second <- putInHand p "28006"
        _ <- putInHand p "28007"
        offers (isPlay second) `shouldReturn` False
        clearHand p
        choose "attack the minion" (isHeroAttack p)
        choose "Hydra Bomber" (isTargetChoice minion)
        pass
        exhaustedNow p `shouldReturn` False
        length <$> handOf p `shouldReturn` 1

    it "readies and draws on removing the last main-scheme threat, but not partial removal" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        useForce p
        clearHand p
        placeMainThreat 2
        choose "first basic thwart" (isHeroThwart p)
        chooseMainScheme
        pass
        exhaustedNow p `shouldReturn` True
        length <$> handOf p `shouldReturn` 0
        resolve [IdentityMessage p ReadiedIdentity]
        choose "remove the last threat" (isHeroThwart p)
        chooseMainScheme
        pass
        exhaustedNow p `shouldReturn` False
        length <$> handOf p `shouldReturn` 1

    it "also triggers for a villain stage defeated by Nova's attack" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        vid <- villainIdNow
        hp <- villainHp . toAttrs <$> getVillain
        resolve [VillainMessage vid (VillainDamaged GameRef $ toDamage (fromIntegral (unHp hp) - 1) FromAbility)]
        useForce p
        clearHand p
        unHp . villainHp . toAttrs <$> getVillain `shouldReturn` 1
        currentIdentity <$> getPlayer p >>= \case
          HeroSide hero -> Hero.getModifiedAttack (toAttrs hero) `shouldReturn` 1
          _ -> fail "Nova must be in hero form before attacking"
        attackVillain p
        -- Finish the next stage's reveal without refreshing a stale question.
        toFirstTurn
        pass
        exhaustedNow p `shouldReturn` False
        length <$> handOf p `shouldReturn` 1

    it "triggers for an event clearing a side scheme" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        scheme <- revealNow p SideSchemes.bombScare
        pass
        useForce p
        resolve [IdentityMessage p ExhaustedIdentity]
        clearHand p
        flight <- putInHand p "28004"
        paying p flight ["28007"]
        choose "clear Bomb Scare" (isTargetChoice $ sideSchemeOf scheme)
        pass
        exhaustedNow p `shouldReturn` False
        length <$> handOf p `shouldReturn` 1

    it "counts a defeat from Nova's nonattack event damage" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        minion <- addMinion p Minions.hydraBomber
        useForce p
        resolve [IdentityMessage p ExhaustedIdentity]
        clearHand p
        field <- putInHand p "28003"
        payment <- putInHand p "28007"
        runNow $ damageIdentity p GameRef (toDamage 1 FromAttack)
        playWith field [payment]
        choose "defeat Hydra Bomber" (isTargetChoice minion)
        pass
        exhaustedNow p `shouldReturn` False
        length <$> handOf p `shouldReturn` 1

    it "pauses while Sam is faceup and resumes when Nova returns before the round ends" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        useForce p
        clearHand p
        resolve [IdentityMessage p (ChangedToForm B), IdentityMessage p ExhaustedIdentity]
        placeMainThreat 1
        main <- mainSchemeIdNow
        runNow [MainSchemeMessage main (MainSchemeThreatRemoved (IdentityRef p) 1)]
        pass
        exhaustedNow p `shouldReturn` True
        length <$> handOf p `shouldReturn` 0
        resolve [IdentityMessage p (ChangedToForm A)]
        placeMainThreat 1
        runNow [MainSchemeMessage main (MainSchemeThreatRemoved (IdentityRef p) 1)]
        pass
        exhaustedNow p `shouldReturn` False
        length <$> handOf p `shouldReturn` 1

    it "triggers for hero ability threat removal, never for an ally defeat, and ends with the round" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        ally <- putIntoPlay p "01084"
        minion <- addMinion p Minions.hydraBomber
        resolve [MinionMessage minion (MinionDamaged GameRef $ toDamage 1 FromAbility)]
        useForce p
        clearHand p
        resolve [IdentityMessage p ExhaustedIdentity]
        a <- allyIn "01084"
        choose "ally attacks" (isAllyAttack a)
        choose "ally defeats the minion" (isTargetChoice minion)
        pass
        exhaustedNow p `shouldReturn` True
        length <$> handOf p `shouldReturn` 0
        placeMainThreat 1
        main <- mainSchemeIdNow
        runNow [MainSchemeMessage main (MainSchemeThreatRemoved (IdentityRef p) 1)]
        pass
        exhaustedNow p `shouldReturn` False
        length <$> handOf p `shouldReturn` 1
        -- Leave the new round's turn question open; resolve would auto-play it
        -- looking for the preceding round's stale question and could lose the game.
        runNow [EndRound]
        toFirstTurn
        resolve [IdentityMessage p ExhaustedIdentity]
        clearHand p
        placeMainThreat 1
        runNow [MainSchemeMessage main (MainSchemeThreatRemoved (IdentityRef p) 1)]
        pass
        exhaustedNow p `shouldReturn` True
        length <$> handOf p `shouldReturn` 0

  describe "Connection to the Worldmind" do
    it "does not count toward hand size, including copies drawn during replenishment" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        asNova p
        held <- putInHand p "28007"
        _ <- stackDeck p ["28007", "28003", "28003", "28004", "28004", "28005"]
        resolve [IdentityMessage p DrawToHandLimit]
        cards <- handOf p
        cards `shouldSatisfy` elem held
        length cards `shouldBe` 7
        length (filter ((/= "28007") . toCardCode . getCardDef) cards) `shouldBe` 5

  describe "Jesse Alexander" do
    it "shuffles one Worldmind into the deck and draws a card in alter-ego" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        clearHand p
        jesse <- putIntoPlay p "28008"
        connection <- putInDiscard p "28007"
        choose "Jesse helps Sam" (isCardAbility jesse 1)
        choose "shuffle Worldmind" (isTargetChoice connection)
        pass
        length <$> handOf p `shouldReturn` 1
        discardOf p >>= (`shouldSatisfy` notElem connection)
        offers (isCardAbility jesse 1) `shouldReturn` False

    it "draws without a Worldmind in discard and cannot be used in hero form" do
      tg <- nova
      let p = player1 tg
      inGame tg do
        clearHand p
        jesse <- putIntoPlay p "28008"
        choose "Jesse draws a card" (isCardAbility jesse 1)
        pass
        length <$> handOf p `shouldReturn` 1
        resolve [SupportMessage (supportIdOf jesse) ReadiedSupport]
        changeForm p
        pass
        offers (isCardAbility jesse 1) `shouldReturn` False
