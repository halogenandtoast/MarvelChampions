module Marvel.Hero.GambitSpec (spec) where

import Control.Monad (replicateM)
import Data.HashMap.Strict qualified as HashMap
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Boost (BoostIcon (..))
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.AlterEgo.Types (alterEgoBaseRecovery)
import Marvel.Stats (unRec)
import Marvel.Identity ( currentIdentity )
import Marvel.Minion.Cards qualified as Minions
import Marvel.Status
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport hiding (Boost)

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

charges :: IdentityId -> GameAppT Natural
charges p = playerIdentityCounters <$> playerAttrs p

addCharges :: IdentityId -> Natural -> GameAppT ()
addCharges p n = resolve [PlaceCounters (toTarget p) n]

throwCards :: Natural -> GameAppT ()
throwCards 0 = pass
throwCards n = do
  choose "Throw de Card" (isNamedAbility "$label.throwDeCard")
  choose "charge counters" (isLabel $ countLabel "discardCounters" n)

playCharged :: IdentityId -> Natural -> GameAppT ()
playCharged p n = do
  card <- putInHand p "37006"
  pay <- putInHand p "01088"
  playWith card [pay]
  throwCards n

attackNow :: IdentityId -> GameAppT ()
attackNow p = do
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

spec :: Spec
spec = describe "Gambit" do
  it "starts as Remy with 9 hit points, REC 3, six cards and no charge counters" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 9
      handSizeOf p `shouldReturn` 6
      charges p `shouldReturn` 0
      currentIdentity <$> getPlayer p >>= \case
        AlterEgoSide ae -> unRec (alterEgoBaseRecovery $ toAttrs ae) `shouldBe` 3
        _ -> error "expected Remy"

  it "Charge de Card is hero-only, places one charge and resets each round" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      offers (isNamedAbility "$label.chargeDeCard") `shouldReturn` False
      asHero p
      choose "Charge de Card" (isNamedAbility "$label.chargeDeCard")
      charges p `shouldReturn` 1
      offers (isNamedAbility "$label.chargeDeCard") `shouldReturn` False
      runNow [IdentityEndedTurn p, EndRound]
      pass
      offers (isNamedAbility "$label.chargeDeCard") `shouldReturn` True
      choose "charge in the new round" (isNamedAbility "$label.chargeDeCard")
      charges p `shouldReturn` 2

  it "charge counters survive changing to Remy and back" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      addCharges p 3
      runNow [IdentityEndedTurn p, EndRound]
      pass
      changeForm p
      pass
      charges p `shouldReturn` 3
      runNow [IdentityEndedTurn p, EndRound]
      pass
      asHero p
      charges p `shouldReturn` 3

  it "Throw de Card cannot trigger with zero counters; an unboosted event deals printed damage" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "37006"
      pay <- putInHand p "01088"
      playWith card [pay]
      offers (isNamedAbility "$label.throwDeCard") `shouldReturn` False
      pass
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4
      charges p `shouldReturn` 0

  it "Throw de Card spends at most three counters and enhances just that event" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      addCharges p 5
      card <- putInHand p "37006"
      pay <- putInHand p "01088"
      playWith card [pay]
      choose "Throw" (isNamedAbility "$label.throwDeCard")
      offers (isLabel $ countLabel "discardCounters" 4) `shouldReturn` False
      choose "three counters" (isLabel $ countLabel "discardCounters" 3)
      chooseVillain
      pass
      charges p `shouldReturn` 2
      villainDamageNow `shouldReturn` 7
      next <- putInHand p "01087"
      resource <- putInHand p "01088"
      playWith next [resource]
      pass
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 10

  it "Throw de Card is not offered for a non-Attack event" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      addCharges p 2
      card <- putInHand p "01086"
      resource <- putInHand p "01088"
      setDamage p 2
      playWith card [resource]
      offers (isNamedAbility "$label.throwDeCard") `shouldReturn` False
      choose "heal Gambit" (isTargetChoice p)
      pass
      charges p `shouldReturn` 2

  it "Charged Card has ranged with one counter and avoids retaliate" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.omegaRed
      addCharges p 1
      playCharged p 1
      choose "Omega Red" (isTargetChoice mid)
      pass
      damageOf p `shouldReturn` 0
      minionDamageOf mid `shouldReturn` Just 5

  it "Charged Card without counters still suffers retaliate" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.omegaRed
      playCharged p 0
      choose "Omega Red" (isTargetChoice mid)
      pass
      damageOf p `shouldReturn` 1

  it "Charged Card with only one counter does not pierce tough" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      vid <- villainIdNow
      resolve [VillainMessage vid VillainBecomeTough]
      addCharges p 1
      playCharged p 1
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 0

  it "Charged Card with two counters pierces tough" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      vid <- villainIdNow
      resolve [VillainMessage vid VillainBecomeTough]
      addCharges p 2
      playCharged p 2
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` False

  it "Charged Card with three counters also deals excess damage to the villain" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.hydraMercenary
      addCharges p 3
      playCharged p 3
      choose "mercenary" (isTargetChoice mid)
      pass
      minionDamageOf mid `shouldReturn` Nothing
      villainDamageNow `shouldReturn` 4

  it "Royal Flush adds its counter after Throw and boosts all three damage instances" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      addCharges p 3
      card <- putInHand p "37007"
      pay <- putInHand p "01088"
      extra <- putInHand p "01089"
      playWith card [pay, extra]
      throwCards 3
      charges p `shouldReturn` 1
      chooseVillain
      chooseVillain
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 9

  it "Royal Flush chooses each target after the previous minion is defeated" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.hydraMercenary
      addCharges p 3
      card <- putInHand p "37007"
      pay <- putInHand p "01088"
      extra <- putInHand p "01089"
      playWith card [pay, extra]
      throwCards 3
      choose "first hit defeats guard" (isTargetChoice mid)
      offers (isTargetChoice mid) `shouldReturn` False
      chooseVillain
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6

  it "Royal Flush cannot spend the counter it has yet to place" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "37007"
      pay <- putInHand p "01088"
      extra <- putInHand p "01089"
      playWith card [pay, extra]
      offers (isNamedAbility "$label.throwDeCard") `shouldReturn` False
      pass
      chooseVillain
      chooseVillain
      chooseVillain
      pass
      charges p `shouldReturn` 1
      villainDamageNow `shouldReturn` 0

  it "Royal Flush is one attack and triggers retaliate once even when all three hits deal zero" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.modok
      card <- putInHand p "37007"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playWith card [energy, genius]
      pass
      choose "first zero-damage hit" (isTargetChoice mid)
      choose "second zero-damage hit" (isTargetChoice mid)
      choose "third zero-damage hit" (isTargetChoice mid)
      pass
      minionDamageOf mid `shouldReturn` Just 0
      damageOf p `shouldReturn` 2

  it "Rogue costs one less for each charge, has tough, and does not consume charges" do
    for_ [0 :: Natural, 2, 4, 6] \n -> do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        addCharges p n
        clearHand p
        rogue <- putInHand p "37002"
        payment <- replicateM (fromIntegral $ subtractNatural n 4) (putInHand p "37006")
        playWith rogue payment
        pass
        ally <- allyIn "37002"
        allyTough ally `shouldBe` True
        charges p `shouldReturn` n

  it "Remy and Creole Charmer require a legal thwart target unless confused" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- addMinion p Minions.kreeCommando
      placeMainThreat 3
      charmer <- putInHand p "37009"
      _ <- putInHand p "01088"
      offers (isNamedAbility "$label.thiefExtraordinaire") `shouldReturn` False
      offers (isPlay charmer) `shouldReturn` False
      resolve [GiveStatus (IdentityRef p) ConfusedStatus]
      offers (isNamedAbility "$label.thiefExtraordinaire") `shouldReturn` True
      offers (isPlay charmer) `shouldReturn` True

  it "Remy looks privately at two cards, discards the chosen second card and preserves the first" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      placeMainThreat 5
      (cards, _) <- setEncounterDeck [Treacheries.advance, Minions.hydraMercenary, Treacheries.assault] []
      before <- mainSchemeThreatNow
      choose "Thief Extraordinaire" (isNamedAbility "$label.thiefExtraordinaire")
      offers (isTargetChoice $ CardIdRef $ toCardId $ cards !! 0) `shouldReturn` True
      offers (isTargetChoice $ CardIdRef $ toCardId $ cards !! 2) `shouldReturn` False
      n <- getBoostIcons (cards !! 1)
      choose "discard the second" (isTargetChoice $ CardIdRef $ toCardId $ cards !! 1)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - n
      map toCardId <$> getEncounterDeck `shouldReturn` map toCardId [cards !! 0, cards !! 2]
      encounterDiscardCodes `shouldReturn` ["01101"]
      isExhaustedPlayer p `shouldReturn` True

  it "Remy counts boost icons but not boost stars" do
    tg <- testGame ["gambit"]
    inGame tg do
      let _p = player1 tg
      placeMainThreat 5
      (cards, _) <- setEncounterDeck [Minions.hydraMercenary {cdBoostIcons = [Boost, Star]}, Treacheries.advance] []
      before <- mainSchemeThreatNow
      choose "Thief" (isNamedAbility "$label.thiefExtraordinaire")
      choose "one icon and a star" (isTargetChoice $ CardIdRef $ toCardId $ testHead cards)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 1

  it "a confused Remy still discards the encounter card as a cost but removes no threat" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      placeMainThreat 5
      resolve [IdentityMessage p IdentityConfused]
      (cards, _) <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
      before <- mainSchemeThreatNow
      choose "confused Thief" (isNamedAbility "$label.thiefExtraordinaire")
      choose "pay the encounter discard" (isTargetChoice $ CardIdRef $ toCardId $ testHead cards)
      pass
      mainSchemeThreatNow `shouldReturn` before
      playerIdentityConfused <$> playerAttrs p `shouldReturn` False
      encounterDiscardCodes `shouldReturn` [toCardCode $ testHead cards]
      isExhaustedPlayer p `shouldReturn` True

  it "Remy works with one card remaining and resets the exhausted encounter deck" do
    tg <- testGame ["gambit"]
    inGame tg do
      let _p = player1 tg
      placeMainThreat 5
      (cards, _) <- setEncounterDeck [Minions.hydraMercenary] [Treacheries.advance]
      choose "Thief" (isNamedAbility "$label.thiefExtraordinaire")
      choose "only card" (isTargetChoice $ CardIdRef $ toCardId $ testHead cards)
      chooseMainScheme
      pass
      accelerationTokensNow `shouldReturn` 1
      encounterDeckSize `shouldReturn` 2

  it "The Thieves Guild responds only after Remy's ability and draws when its point clears a scheme" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      guild <- putIntoPlay p "37003"
      clearHand p
      placeMainThreat 2
      (cards, _) <- setEncounterDeck [Minions.hydraMercenary {cdBoostIcons = [Boost]}, Treacheries.advance] []
      choose "Thief" (isNamedAbility "$label.thiefExtraordinaire")
      choose "one boost" (isTargetChoice $ CardIdRef $ toCardId $ testHead cards)
      offers (isAbilityOfCard guild) `shouldReturn` False
      chooseMainScheme
      choose "The Thieves Guild" (isAbilityOfCard guild)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 0
      length <$> handOf p `shouldReturn` 1
      supportsWith "37003" >>= \case
        [attrs] -> supportExhausted attrs `shouldBe` True
        _ -> error "expected Guild"

  it "The Thieves Guild does not draw if threat remains" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      guild <- putIntoPlay p "37003"
      clearHand p
      placeMainThreat 6
      (cards, _) <- setEncounterDeck [Minions.hydraMercenary {cdBoostIcons = [Boost]}, Treacheries.advance] []
      choose "Thief" (isNamedAbility "$label.thiefExtraordinaire")
      choose "one boost" (isTargetChoice $ CardIdRef $ toCardId $ testHead cards)
      chooseMainScheme
      choose "Guild" (isAbilityOfCard guild)
      chooseMainScheme
      pass
      length <$> handOf p `shouldReturn` 0


  it "The Thieves Guild respects crisis while removing threat from a side scheme" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      guild <- putIntoPlay p "37003"
      _ <- revealNow p Schemes.crowdControl
      pass
      sid <- getsGame $ testHead . HashMap.keys . gameSideSchemes
      placeMainThreat 1
      (cards, _) <- setEncounterDeck [Minions.hydraMercenary {cdBoostIcons = [Boost]}, Treacheries.advance] []
      choose "Thief" (isNamedAbility "$label.thiefExtraordinaire")
      choose "one boost" (isTargetChoice $ CardIdRef $ toCardId $ testHead cards)
      mid <- mainSchemeIdNow
      offers (isTargetChoice mid) `shouldReturn` False
      choose "thwart Crowd Control" (isTargetChoice sid)
      choose "Guild" (isAbilityOfCard guild)
      offers (isTargetChoice mid) `shouldReturn` False
      choose "Guild removes side-scheme threat" (isTargetChoice sid)
      pass
      mainSchemeThreatNow `shouldReturn` 1

  it "The Thieves Guild does not respond to Gambit's Charge de Card" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      guild <- putIntoPlay p "37003"
      placeMainThreat 5
      asHero p
      choose "Charge" (isNamedAbility "$label.chargeDeCard")
      offers (isAbilityOfCard guild) `shouldReturn` False

  it "Gambit's Staff deals one damage when an enemy attacks and exhausts" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      staff <- putIntoPlay p "37004"
      asHero p
      attackNow p
      choose "staff" (isAbilityOfCard staff)
      villainDamageNow `shouldReturn` 1
      upgradeIsExhausted (upgradeIdOf staff) `shouldReturn` True
      pass

  it "Gambit's Staff can interrupt an enemy attacking another player" do
    tg <- testGame ["gambit", "spider-man"]
    inGame tg do
      let p = player1 tg
      ps <- players
      let other = ps !! 1
      staff <- putIntoPlay p "37004"
      asHero p
      attackNow other
      choose "staff on the other player's attack" (isAbilityOfCard staff)
      villainDamageNow `shouldReturn` 1
      pass

  it "Gambit's Staff defeats a one-hit-point attacker before it can deal damage" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      staff <- putIntoPlay p "37004"
      asHero p
      mid <- addMinion p Minions.hydraMercenary
      resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 2 FromAbility)]
      runNow [MinionMessage mid (MinionAttacks p)]
      choose "staff defeats attacker" (isAbilityOfCard staff)
      pass
      minionDamageOf mid `shouldReturn` Nothing
      damageOf p `shouldReturn` 0

  it "Gambit's Staff is unavailable in alter ego" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      staff <- putIntoPlay p "37004"
      attackNow p
      offers (isAbilityOfCard staff) `shouldReturn` False
      pass

  it "Guild Armor readies Gambit after he defends without damage" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      armor <- putIntoPlay p "37005"
      asHero p
      _ <- setEncounterDeck [Treacheries.assault {cdBoostIcons = []}, Treacheries.advance] []
      attackNow p
      choose "defend" isDefend
      choose "Guild Armor" (isAbilityOfCard armor)
      pass
      isExhaustedPlayer p `shouldReturn` False
      damageOf p `shouldReturn` 0
      upgradeIsExhausted (upgradeIdOf armor) `shouldReturn` True

  it "Guild Armor is not offered after taking attack damage or leaving an attack undefended" do
    for_ [True, False] \defends -> do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        armor <- putIntoPlay p "37005"
        asHero p
        _ <- setEncounterDeck [Treacheries.advance {cdBoostIcons = replicate 4 Boost}, Treacheries.assault] []
        attackNow p
        if defends then choose "defend" isDefend else choose "no defender" isNoDefender
        offers (isAbilityOfCard armor) `shouldReturn` False
        pass

  it "Natural Agility places a counter first and counts it toward this defense only" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      addCharges p 1
      card <- putInHand p "37008"
      _ <- setEncounterDeck [Treacheries.advance {cdBoostIcons = replicate 3 Boost}, Treacheries.assault] []
      attackNow p
      choose "defend" isDefend
      choose "Natural Agility" (isPlay card)
      pass
      charges p `shouldReturn` 2
      damageOf p `shouldReturn` 0
      resolve [IdentityMessage p ReadiedIdentity]
      _ <- setEncounterDeck [Treacheries.advance {cdBoostIcons = replicate 3 Boost}, Treacheries.assault] []
      attackNow p
      choose "defend next attack" isDefend
      pass
      damageOf p `shouldReturn` 2

  it "Natural Agility keeps its DEF bonus when a defense event precedes basic defense" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      clearHand p
      addCharges p 1
      punch <- putInHand p "32014"
      energy <- putInHand p "01088"
      agility <- putInHand p "37008"
      _ <- setEncounterDeck [Treacheries.advance {cdBoostIcons = replicate 3 Boost}, Treacheries.assault] []
      attackNow p
      playWith punch [energy]
      let untilOffered match = do
            ready <- offers match
            unless ready $ choose "decline other interrupt" (isLabel "$label.noResponse") >> untilOffered match
      untilOffered (isPlay agility)
      playWith agility []
      untilOffered isDefend
      choose "make a basic defense" isDefend
      pass
      charges p `shouldReturn` 2
      damageOf p `shouldReturn` 0

  it "Natural Agility still pays its charge counter when Counterspell cancels its DEF effect" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- revealNow p Attachments.counterspell
      pass
      card <- putInHand p "37008"
      _ <- setEncounterDeck [Treacheries.advance {cdBoostIcons = replicate 3 Boost}, Treacheries.assault] []
      attackNow p
      choose "defend" isDefend
      choose "Natural Agility" (isPlay card)
      chooseIf isAnyAbility
      pass
      charges p `shouldReturn` 1
      damageOf p `shouldReturn` 2

  it "Creole Charmer confuses the villain when its thwart removes the last threat" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      placeMainThreat 3
      card <- putInHand p "37009"
      pay <- putInHand p "01088"
      playWith card [pay]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 0
      Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` True

  it "Creole Charmer does not confuse when threat remains and cannot be played in hero form" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      placeMainThreat 5
      card <- putInHand p "37009"
      pay <- putInHand p "01088"
      playWith card [pay]
      chooseMainScheme
      pass
      Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` False
      asHero p
      other <- putInHand p "37009"
      _ <- putInHand p "01088"
      offers (isPlay other) `shouldReturn` False

  it "Molecular Acceleration's spending interrupt adds a charge before Throw de Card" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      clearHand p
      card <- putInHand p "37006"
      resource <- putInHand p "37010"
      playWith card [resource]
      choose "Molecular Acceleration" (isLabel "$label.molecularAcceleration")
      payWith []
      charges p `shouldReturn` 1
      throwCards 1
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5
      charges p `shouldReturn` 0

  it "Molecular Acceleration does not add charges when spent in alter ego" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      card <- putInHand p "37009"
      resource <- putInHand p "37010"
      placeMainThreat 4
      playWith card [resource]
      offers (isLabel "$label.molecularAcceleration") `shouldReturn` False
      chooseMainScheme
      pass
      charges p `shouldReturn` 0

  it "Molecular Acceleration also triggers when spent on an ability" do
    tg <- testGame ["gambit"]
    inGame tg do
      let p = player1 tg
      asHero p
      axe <- putIntoPlay p "06019"
      resource <- putInHand p "37010"
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      choose "Jarnbjorn" (isAbilityOfCard axe)
      payWith [resource]
      choose "Molecular Acceleration" (isLabel "$label.molecularAcceleration")
      payWith []
      chooseVillain
      pass
      charges p `shouldReturn` 1
