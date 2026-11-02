module Marvel.Aspect.WonderManPackSpec (spec) where

import Marvel.Hero.Runner qualified as Hero
import Marvel.Hercules.Cards qualified as HerculesCards
import Marvel.Identity (currentIdentity)
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Query
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Modifier
import Marvel.Matchers
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero you = changeForm you >> pass >> clearHand you

spec :: Spec
spec = describe "Wonder Man aspect cards" do
  it "Heroic Conditioning grants three hit points and one THW under its chosen player's control" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putInHand you "58021"
      payment <- traverse (putInHand you) ["01088", "58003"]
      playOn card you payment
      hitPointsOf you `shouldReturn` 15
      currentIdentity <$> getPlayer you >>= \case
        HeroSide hero -> Hero.getModifiedThwart (toAttrs hero) `shouldReturn` 3
        _ -> error "Expected hero form"

  it "Battlefield Benevolence heals its enemy as a cost then confuses that same enemy" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      villain <- villainIdNow
      Lifted.runQueueT (Lifted.capture $ Lifted.dealDamage villain GameRef $ toDamage 2 FromAbility) >>= resolve
      card <- putInHand you "58016"
      payment <- putInHand you "58003"
      playWith card [payment]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 0
      enemyMatches ConfusedEnemy (EnemyVillainId villain) `shouldReturn` True

  it "Battlefield Benevolence cannot pay its healing cost with only one damage" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      villain <- villainIdNow
      Lifted.runQueueT (Lifted.capture $ Lifted.dealDamage villain GameRef $ toDamage 1 FromAbility) >>= resolve
      card <- putInHand you "58016"
      _ <- putInHand you "58003"
      offers (isPlay card) `shouldReturn` False

  it "Firebird enters with a rebirth counter only when overpaid" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- putInHand you "58012"
      payment <- putInHand you "01088"
      playWith card [payment]
      pass
      ally <- allyIn "58012"
      allyCounters ally `shouldBe` 0

  it "Hawkeye enters with four arrows and shoots after another ally thwarts" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      hawkeye <- putIntoPlay you "58013"
      _ <- putIntoPlay you "58012"
      ally <- allyIn "58012"
      minion <- addMinion you Minions.hydraRegular
      placeMainThreat 3
      choose "Firebird thwarts" (isAllyThwart ally)
      chooseMainScheme
      choose "Hawkeye arrow" (isAbilityOfCard hawkeye)
      choose "the minion" (isTargetChoice minion)
      pass
      allyCounters <$> allyIn "58013" `shouldReturn` 3

  it "Swordsman's basic attack has piercing" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "58022"
      swordsman <- allyIn "58022"
      villain <- villainIdNow
      Lifted.runQueueT (Lifted.capture $ Lifted.giveStatus villain ToughStatus) >>= resolve
      before <- villainDamageNow
      choose "Swordsman attacks" (isAllyAttack swordsman)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` (before + 2)

  it "Avengers Compound tucks an ally from hand without putting it into play" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      compound <- putIntoPlay you "58034"
      ally <- putInHand you "58022"
      choose "Avengers Compound" (isAbilityOfCard compound)
      choose "tuck Swordsman" (isTargetChoice ally)
      Lifted.heldCardsAt (supportIdOf compound) `shouldReturn` [ally]
      alliesWith "58022" `shouldReturn` []

  it "Caught in the Crossfire damages its attached minion when a friendly ally attacks" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      mid <- addMinion you Minions.hydraMercenary
      caught <- putInHand you "58030"
      playOn caught mid []
      _ <- putIntoPlay you "58013"
      hawkeye <- allyIn "58013"
      choose "Hawkeye attacks" (isAllyAttack hawkeye)
      choose "attached minion" (isTargetChoice mid)
      choose "Crossfire" (isAbilityOfCard caught)
      pass
      getMinionDamageInPlay mid `shouldReturn` 2

  it "Stronger Together reduces damage to another Avenger by Wonder Man's DEF" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      ally <- putIntoPlay you "58022"
      event <- putInHand you "58019"
      Lifted.runQueueT (Lifted.capture $ Lifted.damageCharacter (allyIdOf ally) GameRef $ toDamage 3 FromAbility) >>= runNow
      playWith event []
      pass
      allyDamage <$> allyIn "58022" `shouldReturn` 1

  it "Stronger Together does not protect Wonder Man himself" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      event <- putInHand you "58019"
      runNow [IdentityMessage you $ IdentityDamaged GameRef $ toDamage 2 FromAbility]
      offers (isPlay event) `shouldReturn` False
      pass
      damageOf you `shouldReturn` 2

  it "Unified Strike adds the hero's matching power and prevents consequential damage for that use" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- putIntoPlay you "58012"
      firebird <- allyIn "58012"
      event <- putInHand you "58020"
      placeMainThreat 6
      before <- mainSchemeThreatNow
      choose "Firebird thwarts" (isAllyThwart firebird)
      chooseMainScheme
      playWith event []
      pass
      mainSchemeThreatNow `shouldReturn` before - 4
      allyDamage <$> allyIn "58012" `shouldReturn` 0
      isExhaustedNow you `shouldReturn` True

  it "Scarlet Witch resolves the discarded treachery's When Revealed without revealing another card" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- stackEncounterDeck [Treacheries.advance]
      before <- mainSchemeThreatNow
      _ <- putIntoPlay you "58014"
      mainSchemeThreatNow >>= (\n -> n `shouldSatisfy` (> before))
      cards <- encounterDiscard
      any ((== "01186") . toCardCode) cards `shouldBe` True

  it "Sentry places six threat when his encounter-deck search finds no side scheme" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- setEncounterDeck [Treacheries.advance] []
      before <- mainSchemeThreatNow
      _ <- putIntoPlay you "58015"
      mainSchemeThreatNow `shouldReturn` before + 6

  it "Jarvis can discard a status after an Avenger hero changes to alter ego" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      jarvis <- putIntoPlay you "58024"
      resolve [IdentityMessage you IdentityConfused]
      runNow [IdentityMessage you $ ChangedToForm B]
      choose "Jarvis" (isAbilityOfCard jarvis)
      choose "discard confused" (isLabel "$label.discardConfusedStatus")
      isConfused you `shouldReturn` False

  it "Coordinated Effort attaches to an encounter card and grants contribution permission there" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      villain <- villainIdNow
      card <- putInHand you "58032"
      playOn card villain []
      modifiers <- getModifiers $ VillainRef villain
      (AllowsCostContributions `elem` modifiers) `shouldBe` True

  it "Cameo adds an absent identity's signature ally during setup and removes itself" do
    let base = fromMaybe (error "No Hercules starter") $ lookupStarterDeck "hercules"
        deck = base {cards = ("58031", 1) : filter ((/= "59034") . fst) base.cards}
        setup = fromMaybe (error "No Rhino setup") $ recommendedSetup "01094"
    (app, ran, logged) <- dealtGameLogged setup [deck] 1
    seats <- runGameApp app $ getsGame gameSeats
    let tg = TestGame app ran logged seats
    inGame tg do
      toFirstTurn
      let you = player1 tg
      owned <- getCardsOwnedBy you
      let imported = filter (\def -> cardMatch (HeroSpecificCard <> CardWithType AllyType) def && toCardCode def /= "59008") owned
      length imported `shouldBe` 1
      any ((== "58031") . toCardCode) owned `shouldBe` False

  it "Everywhere All at Once removes two threat from exactly X different schemes" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      side <- sideSchemeOf <$> revealNow you Schemes.bombScare
      pass
      placeMainThreat 4
      before <- mainSchemeThreatNow
      card <- putInHand you "58018"
      payment <- putInHand you "01088"
      choose "play Everywhere" (isPlay card)
      choose "X is two" (isLabel $ countLabel "chooseXAmount" 2)
      payWith [payment]
      chooseMainScheme
      choose "the distinct side scheme" (isTargetChoice side)
      pass
      mainSchemeThreatNow `shouldReturn` before - 2

  it "Disarming Defense adds DEF and discards an actionable attachment only after a damage-free defense" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      horn <- revealNow you Attachments.enhancedIvoryHorn
      pass
      card <- putInHand you "58033"
      payment <- putInHand you "58003"
      villain <- villainIdNow
      runNow [VillainMessage villain $ VillainAttacksWithoutBoost you]
      choose "Wonder Man defends" isDefend
      playWith card [payment]
      pass
      choose "discard the Horn" (isTargetChoice $ AttachmentId $ unCardId $ ecCardId horn)
      pass
      damageOf you `shouldReturn` 0
      selectAny (AttachmentIs Attachments.enhancedIvoryHorn) `shouldReturn` False

  it "Coordinated Effort lets another player contribute resources to the attached encounter card's cost" do
    tg <- testGame ["wonder-man", "hercules"]
    inGame tg do
      let you = player1 tg
          other = player2 tg
      asHero you
      clearHand other
      horn <- revealNow you Attachments.enhancedIvoryHorn
      pass
      let aid = AttachmentId $ unCardId $ ecCardId horn
      coordinated <- putInHand you "58032"
      playOn coordinated aid []
      ownPhysical <- putInHand you "58010"
      contribution <- putInHand other "01090"
      choose "remove the Horn" (isAbilityOf aid)
      choose "pay own physical" (isPayWith ownPhysical)
      choose "invite Hercules" (isTargetChoice other)
      choose "contribute Strength" (isPayWith contribution)
      whenM (offers isFinishPayment) $ choose "finish" isFinishPayment
      pass
      selectAny (AttachmentIs Attachments.enhancedIvoryHorn) `shouldReturn` False
      discard <- discardOf other
      (contribution `elem` discard) `shouldBe` True

  it "Disarming Defense adds exactly two DEF and does not discard an attachment when one damage gets through" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      _ <- revealNow you Attachments.enhancedIvoryHorn
      pass
      _ <- revealNow you HerculesCards.hc59039
      pass
      card <- putInHand you "58033"
      payment <- putInHand you "58003"
      villain <- villainIdNow
      getVillainAttackValue villain `shouldReturn` 5
      runNow [VillainMessage villain $ VillainAttacksWithoutBoost you]
      choose "Wonder Man defends" isDefend
      playWith card [payment]
      pass
      damageOf you `shouldReturn` 1
      selectAny (AttachmentIs Attachments.enhancedIvoryHorn) `shouldReturn` True
