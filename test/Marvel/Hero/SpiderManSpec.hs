module Marvel.Hero.SpiderManSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types (AbilityType (..))
import Marvel.Damage
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.SideScheme.Types (sideSchemeHeldCards)
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport

spec :: Spec
spec = describe "Spider-Man" do
  it "Swinging Web Kick deals 8 damage to an enemy" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      kick <- putInHand p "01005"
      strength <- putInHand p "01090"
      energy <- putInHand p "01088"
      changeForm p
      playWith kick [strength, energy]
      villain <- getVillain
      choose "kick Rhino" (isTargetChoice (toId villain))
      villainDamageNow `shouldReturn` 8

  it "Swinging Web Kick is a hero action" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      kick <- putInHand p "01005"
      _ <- putInHand p "01090"
      _ <- putInHand p "01088"
      offers (isPlay kick) `shouldReturn` False

  it "Peter Parker generates a mental resource once a round" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      shooter <- putInHand p "01008"
      choose "play Web-Shooter" (isPlay shooter)
      choose "pay with Scientist" (isAbilityOf p)
      upgradeCodes `shouldReturn` ["01008"]
      -- once a round: another card cannot be paid for with it
      kick <- putInHand p "01005"
      offers (isPlay kick) `shouldReturn` False

  it "Spider-Sense draws a card when the villain attacks him" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      endTurn
      -- the end of the player phase draws up to hand size first
      drawnUp <- length <$> handOf p
      choose "use Spider-Sense" (isAbilityOf p)
      after <- length <$> handOf p
      after `shouldBe` drawnUp + 1

  it "Black Cat adds the cards with a printed mental resource of the top 2 of the deck to hand" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      cat <- putInHand p "01002"
      energy <- putInHand p "01088"
      [kick, shooter] <- stackDeck p ["01005", "01008"]
      playWith cat [energy]
      choose "resolve Black Cat" (isAbilityOfCard cat)
      hand <- handOf p
      hand `shouldBe` [kick]
      discard <- discardOf p
      discard `shouldSatisfy` elem shooter
      discard `shouldSatisfy` notElem kick

  it "Backflip prevents all the damage of an attack" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      backflip <- putInHand p "01003"
      _ <- stackEncounterDeck [Minions.hydraMercenary]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "decline Spider-Sense" isNoResponse
      choose "do not defend" isNoDefender
      choose "play Backflip" (isPlay backflip)
      pass
      damageOf p `shouldReturn` 0
      discardOf p `shouldReturn` [backflip]

  it "Backflip is not offered against damage that is not from an attack" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      backflip <- putInHand p "01003"
      runNow [IdentityMessage p (IdentityDamaged GameRef (toDamage 2 FromAbility))]
      offers (isPlay backflip) `shouldReturn` False
      damageOf p `shouldReturn` 2

  it "Enhanced Spider-Sense cancels a treachery's When Revealed" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      sense <- putInHand p "01004"
      energy <- putInHand p "01088"
      before <- mainSchemeThreat
      _ <- revealNow p Treacheries.advance
      playWith sense [energy]
      pass
      mainSchemeThreat `shouldReturn` before
      discardOf p `shouldReturn` [sense, energy]

  it "a treachery cancelled by one Enhanced Spider-Sense is not offered to another" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      sense <- putInHand p "01004"
      other <- putInHand p "01004"
      energy <- putInHand p "01088"
      _ <- putInHand p "01088"
      _ <- revealNow p Treacheries.advance
      playWith sense [energy]
      offers (isPlay other) `shouldReturn` False

  it "Enhanced Spider-Sense cannot be played in alter-ego form" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      sense <- putInHand p "01004"
      _ <- putInHand p "01088"
      _ <- revealNow p Treacheries.advance
      offers (isPlay sense) `shouldReturn` False

  it "Aunt May heals Peter Parker 4" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      aunt <- putInHand p "01006"
      energy <- putInHand p "01088"
      playWith aunt [energy]
      setDamage p 6
      choose "use Aunt May" (isAbilityOfCard aunt)
      damageOf p `shouldReturn` 2
      -- exhausted, and only in alter-ego form
      offers (isAbilityOfCard aunt) `shouldReturn` False

  it "Aunt May is an alter-ego action" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      aunt <- putInHand p "01006"
      energy <- putInHand p "01088"
      playWith aunt [energy]
      setDamage p 6
      changeForm p
      offers (isAbilityOfCard aunt) `shouldReturn` False

  it "Spider-Tracer removes 3 threat from a scheme when the minion it is on is defeated" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      tracer <- putInHand p "01007"
      energy <- putInHand p "01088"
      offers (isPlay tracer) `shouldReturn` False -- no minion yet
      mercenary <- revealNow p Minions.hydraMercenary
      pass
      -- the one minion is what it is played on, still picked
      playOn tracer (minionOf mercenary) [energy]
      mainScheme <- getsGame (HashMap.keys . gameMainSchemes)
      runNow [MainSchemeMessage m (MainSchemePlaceThreat 4) | m <- mainScheme]
      before <- mainSchemeThreat
      runNow [MinionMessage (minionOf mercenary) (MinionDamaged GameRef (toDamage 10 FromAbility))]
      choose "resolve Spider-Tracer" (isAbilityOfCard tracer)
      chooseMainScheme
      pass
      mainSchemeThreat `shouldReturn` (before - 3)
      upgradeCodes `shouldReturn` []

  it "Web-Shooter generates a wild resource per web counter, then is discarded" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      shooter <- putInHand p "01008"
      strength <- putInHand p "01090"
      playWith shooter [strength]
      for_ [1 :: Int, 2, 3] \_ -> do
        -- another Web-Shooter, paid for by the first
        another <- putInHand p "01008"
        choose "play another Web-Shooter" (isPlay another)
        -- named as printed: a Hero Resource
        offers (\case UseAbility ab -> isAbilityOfCard shooter (UseAbility ab) && abilityType ab == HeroResource; _ -> False)
          `shouldReturn` True
        choose "pay with Web-Shooter" (isAbilityOfCard shooter)
        whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
        offers (isAbilityOfCard shooter) `shouldReturn` False -- exhausted
        runNow [UpgradeMessage (upgradeOf shooter) ReadiedUpgrade]
      getsGame (HashMap.member (upgradeOf shooter) . gameUpgrades) `shouldReturn` False
      discardOf p >>= (`shouldSatisfy` elem shooter)

  it "Web-Shooter is a hero resource" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      shooter <- putInHand p "01008"
      strength <- putInHand p "01090"
      playWith shooter [strength]
      -- Black Cat costs 2: Scientist pays 1, and Web-Shooter cannot pay the other
      runNow [IdentityMessage p (ChangedToForm B)]
      cat <- putInHand p "01002"
      offers (isPlay cat) `shouldReturn` False

  it "Webbed Up stuns the villain instead of letting it attack" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      webbed <- putInHand p "01009"
      payment <- traverse (putInHand p) ["01088", "01090"]
      -- Rhino is the one enemy to play it on, still picked
      vid <- villainIdNow
      playOn webbed vid payment
      upgradeCodes `shouldReturn` ["01009"]
      -- max 1 per enemy
      webbed' <- putInHand p "01009"
      _ <- traverse (putInHand p) ["01088", "01090"]
      offers (isPlay webbed') `shouldReturn` False
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "resolve Webbed Up" (isAbilityOfCard webbed)
      pass
      damageOf p `shouldReturn` 0
      upgradeCodes `shouldReturn` []
      (V.villainStunned . toAttrs <$> getVillain) `shouldReturn` True

  it "Webbed Up stuns a minion instead of letting it attack" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      mercenary <- revealNow p Minions.hydraMercenary
      pass
      webbed <- putInHand p "01009"
      payment <- traverse (putInHand p) ["01088", "01090"]
      playOn webbed (EnemyMinionId $ minionOf mercenary) payment
      runNow [MinionMessage (minionOf mercenary) (MinionAttacks p)]
      choose "resolve Webbed Up" (isAbilityOfCard webbed)
      pass
      damageOf p `shouldReturn` 0
      upgradeCodes `shouldReturn` []
      stunned <- getsGame (fmap (minionStunned . toAttrs) . HashMap.lookup (minionOf mercenary) . gameMinions)
      stunned `shouldBe` Just True

  it "Webbed Up is hero form only" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      webbed <- putInHand p "01009"
      _ <- traverse (putInHand p) ["01088", "01090"]
      offers (isPlay webbed) `shouldReturn` False

  it "Eviction Notice: flipping to Peter Parker and exhausting him removes it from the game" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      eviction <- revealNow p Obligations.evictionNotice
      choose "flip to alter-ego" (isLabel "$label.changeToB")
      choose "exhaust Peter Parker" (isLabelStarting "$label.exhaust")
      isExhaustedNow p `shouldReturn` True
      a <- playerAttrs p
      a.playerIdentitySide `shouldBe` B
      (map ecCardId <$> encounterDiscard) >>= (`shouldSatisfy` notElem (ecCardId eviction))
      getsGame (HashMap.null . gameObligations) `shouldReturn` True

  it "Eviction Notice: otherwise discard a card at random, and it surges" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      card <- putInHand p "01088"
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      eviction <- revealNow p Obligations.evictionNotice
      choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
      pass
      handOf p `shouldReturn` []
      discardOf p `shouldReturn` [card]
      (map ecCardId <$> encounterDiscard) >>= (`shouldSatisfy` elem (ecCardId eviction))
      -- surge: the next card is dealt facedown
      dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)

  it "Highway Robbery holds a random card from each hand, facedown, until it is defeated" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      card <- putInHand p "01088"
      robbery <- revealNow p SideSchemes.highwayRobbery
      handOf p `shouldReturn` []
      discardOf p `shouldReturn` []
      held <- getsGame (fmap (sideSchemeHeldCards . toAttrs) . HashMap.lookup (sideSchemeOf robbery) . gameSideSchemes)
      held `shouldBe` Just [card]
      runNow [SideSchemeMessage (sideSchemeOf robbery) (SideSchemeThwarted GameRef 20)]
      pass
      handOf p `shouldReturn` [card]
      getsGame (HashMap.member (sideSchemeOf robbery) . gameSideSchemes) `shouldReturn` False

  it "Vulture attacks the hero as he engages (quickstrike)" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      _ <- revealNow p Minions.vulture
      pass
      damageOf p `shouldReturn` 3

  it "Vulture does not attack an alter-ego as he engages" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p Minions.vulture
      pass
      damageOf p `shouldReturn` 0

  it "Sweeping Swoop stuns the hero, and surges while Vulture is in play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      swoop <- revealNow p Treacheries.sweepingSwoop
      pass
      isStunned p `shouldReturn` True
      dealtTo p >>= (`shouldSatisfy` not . (ecCardId next `elem`) . map ecCardId)
      (map ecCardId <$> encounterDiscard) >>= (`shouldSatisfy` elem (ecCardId swoop))
      _ <- revealNow p Minions.vulture
      pass
      [next'] <- stackEncounterDeck [Minions.hydraMercenary]
      _ <- revealNow p Treacheries.sweepingSwoop
      pass
      dealtTo p >>= (`shouldSatisfy` (ecCardId next' `elem`) . map ecCardId)

  it "Sweeping Swoop as a boost stuns the character the attack damages" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      _ <- stackEncounterDeck [Treacheries.sweepingSwoop]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "decline Spider-Sense" isNoResponse
      choose "do not defend" isNoDefender
      pass
      damageOf p `shouldReturn` 2
      isStunned p `shouldReturn` True

  it "The Vulture's Plans discards a random card from each hand and places threat per resource type" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      card <- putInHand p "01088"
      before <- mainSchemeThreat
      _ <- revealNow p Treacheries.theVulturesPlans
      pass
      discardOf p `shouldReturn` [card]
      mainSchemeThreat `shouldReturn` (before + 1)
      -- nothing discarded, no threat
      _ <- revealNow p Treacheries.theVulturesPlans
      pass
      mainSchemeThreat `shouldReturn` (before + 1)

  it "The Vulture's Plans counts each different resource type once" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      ps <- players
      for_ ps clearHand
      for_ ps \q -> putInHand q "01088"
      before <- mainSchemeThreat
      _ <- revealNow (player1 tg) Treacheries.theVulturesPlans
      pass
      mainSchemeThreat `shouldReturn` (before + 1)
      for_ (zip ps ["01088", "01089"]) \(q, code) -> putInHand q code
      _ <- revealNow (player1 tg) Treacheries.theVulturesPlans
      pass
      mainSchemeThreat `shouldReturn` (before + 3)

  it "Spider-Sense does not answer a minion's attack" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      mercenary <- revealNow p Minions.hydraMercenary
      pass
      runNow [MinionMessage (minionOf mercenary) (MinionAttacks p)]
      offers (isAbilityOf p) `shouldReturn` False

  it "a minion's attack is over once it resolves" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      mercenary <- revealNow p Minions.hydraMercenary
      pass
      runNow [MinionMessage (minionOf mercenary) (MinionAttacks p)]
      choose "take it undefended" isNoDefender
      damageOf p `shouldReturn` 1
      attacking <- getsGame (fmap (minionAttacking . toAttrs) . HashMap.lookup (minionOf mercenary) . gameMinions)
      attacking `shouldBe` Just Nothing

  it "Eviction Notice is given to the Peter Parker player, whoever reveals it" do
    tg <- testGame ["captain-marvel", "spider-man"]
    inGame tg do
      [carol, peter] <- players
      _ <- revealNow carol Obligations.evictionNotice
      fmap fst <$> asked `shouldReturn` Just peter
      choose "exhaust Peter Parker" (isLabelStarting "$label.exhaust")
      isExhaustedNow peter `shouldReturn` True
      isExhaustedNow carol `shouldReturn` False

  it "Eviction Notice without Peter Parker in the game is removed, and another card is revealed" do
    tg <- testGame ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      eviction <- revealNow p Obligations.evictionNotice
      pass
      getsGame (HashMap.null . gameObligations) `shouldReturn` True
      (map ecCardId <$> encounterDiscard) >>= (`shouldSatisfy` notElem (ecCardId eviction))
      getsGame (HashMap.member (minionOf next) . gameMinions) `shouldReturn` True
