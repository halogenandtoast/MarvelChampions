module Marvel.Hero.CaptainMarvelSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Upgrade.Types (getUpgradeUses)
import Marvel.Villain.Types qualified as V
import Marvel.Damage (DamageSource (..), toDamage)
import TestImport

captainMarvel :: IO TestGame
captainMarvel = testGame ["captain-marvel"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

-- | A card put into play from hand, paid for with new resource cards.
inPlay :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
inPlay p code codes = do
  card <- putInHand p code
  playPaying p card codes
  pure card

spec :: Spec
spec = describe "Captain Marvel" do
  it "Rechannel: spend an energy resource and heal 1 to draw a card, once a round" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      energy <- putInHand p "01088"
      setDamage p 2
      choose "rechannel" (isNamedAbility "Rechannel")
      choose "pay with energy" (isPayWith energy)
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      damageOf p `shouldReturn` 1
      hand <- handOf p
      length hand `shouldBe` 1
      _ <- putInHand p "01088"
      offers (isNamedAbility "Rechannel") `shouldReturn` False

  it "Rechannel needs damage to heal and an energy resource" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      _ <- putInHand p "01089"
      setDamage p 2
      offers (isNamedAbility "Rechannel") `shouldReturn` False
      _ <- putInHand p "01088"
      setDamage p 0
      offers (isNamedAbility "Rechannel") `shouldReturn` False

  it "Commander: a player draws a card, once a round" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      clearHand p
      choose "commander" (isNamedAbility "Commander")
      choose "the player" (isTargetChoice p)
      length <$> handOf p `shouldReturn` 1
      offers (isNamedAbility "Commander") `shouldReturn` False

  it "Spider-Woman confuses the villain as she enters play" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      clearHand p
      woman <- putInHand p "01011"
      playPaying p woman ["01014"]
      choose "confuse the villain" (isAbilityOfCard woman)
      (V.villainConfused . toAttrs <$> getVillain) `shouldReturn` True

  it "Crisis Interdiction removes 2 threat from a scheme, and from a second one only when aerial" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      mainScheme <- mainSchemeIdNow
      runNow [MainSchemeMessage mainScheme (MainSchemePlaceThreat 4)]
      magnitron <- revealNow p SideSchemes.thePsycheMagnitron
      pass
      before <- mainSchemeThreat
      sideBefore <- sideSchemeThreat (sideSchemeOf magnitron)
      interdiction <- putInHand p "01012"
      playPaying p interdiction ["01088"]
      choose "the main scheme" (isTargetChoice (SchemeMainSchemeId mainScheme))
      mainSchemeThreat `shouldReturn` (before - 2)
      sideSchemeThreat (sideSchemeOf magnitron) `shouldReturn` sideBefore
      offers isEndTurn `shouldReturn` True
      -- with Cosmic Flight she is aerial
      _ <- inPlay p "01017" ["01088"]
      interdiction' <- putInHand p "01012"
      playPaying p interdiction' ["01088"]
      choose "the main scheme" (isTargetChoice (SchemeMainSchemeId mainScheme))
      offers (isTargetChoice (SchemeMainSchemeId mainScheme)) `shouldReturn` False
      choose "the side scheme" (isTargetChoice (SchemeSideSchemeId $ sideSchemeOf magnitron))
      mainSchemeThreat `shouldReturn` (before - 4)
      sideSchemeThreat (sideSchemeOf magnitron) `shouldReturn` (sideBefore - 2)

  it "Photonic Blast deals 5 damage, and draws a card when paid with an energy resource" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      blast <- putInHand p "01013"
      -- Energy Absorption alone pays for it
      playPaying p blast ["01014"]
      vid <- villainIdNow
      choose "blast Rhino" (isTargetChoice vid)
      villainDamageNow `shouldReturn` 5
      length <$> handOf p `shouldReturn` 1
      clearHand p
      blast' <- putInHand p "01013"
      playPaying p blast' ["01089", "01090"]
      choose "blast Rhino" (isTargetChoice vid)
      villainDamageNow `shouldReturn` 10
      length <$> handOf p `shouldReturn` 0

  it "Alpha Flight Station: discard a card to draw 1, or 2 as Carol Danvers" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      clearHand p
      station <- inPlay p "01015" ["01089"]
      junk <- putInHand p "01088"
      choose "use the station" (isAbilityOfCard station)
      choose "discard" (isTargetChoice junk)
      discardOf p >>= (`shouldSatisfy` elem junk)
      length <$> handOf p `shouldReturn` 2
      offers (isAbilityOfCard station) `shouldReturn` False
      runNow [SupportMessage (supportOf station) ReadiedSupport]
      changeForm p
      clearHand p
      junk' <- putInHand p "01088"
      choose "use the station" (isAbilityOfCard station)
      choose "discard" (isTargetChoice junk')
      length <$> handOf p `shouldReturn` 1

  it "Captain Marvel's Helmet gives +1 DEF, +2 when aerial" do
    let defended withFlight = do
          tg <- captainMarvel
          let p = player1 tg
          inGame tg do
            changeForm p
            clearHand p
            _ <- inPlay p "01016" ["01090"]
            when withFlight $ void $ inPlay p "01017" ["01088"]
            -- a boost of 2: Rhino attacks for 4
            _ <- stackEncounterDeck [Treacheries.theVulturesPlans]
            vid <- villainIdNow
            runNow [VillainMessage vid (VillainAttacks p)]
            choose "defend" isDefend
            pass
            damageOf p
    defended False `shouldReturn` 2
    defended True `shouldReturn` 1

  it "Cosmic Flight answers damage that is not from an attack" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      flight <- inPlay p "01017" ["01088"]
      runNow (damageIdentity p GameRef (toDamage 2 FromAbility))
      choose "use Cosmic Flight" (isAbilityOfCard flight)
      pass
      damageOf p `shouldReturn` 0
      upgradeCodes `shouldReturn` []

  it "Cosmic Flight is discarded to prevent 3 of the damage she would take" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      flight <- inPlay p "01017" ["01088"]
      _ <- stackEncounterDeck [Treacheries.theVulturesPlans]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "no defender" isNoDefender
      choose "use Cosmic Flight" (isAbilityOfCard flight)
      pass
      damageOf p `shouldReturn` 1
      upgradeCodes `shouldReturn` []
      discardOf p >>= (`shouldSatisfy` elem flight)

  it "Cosmic Flight is a hero interrupt" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      flight <- inPlay p "01017" ["01088"]
      runNow [IdentityMessage p (ChangedToForm B)]
      _ <- stackEncounterDeck [Treacheries.theVulturesPlans]
      mercenary <- revealNow p Minions.hydraMercenary
      pass
      runNow [MinionMessage (minionOf mercenary) (MinionAttacks p)]
      offers (isAbilityOfCard flight) `shouldReturn` False

  it "Energy Channel stores energy and deals 2 damage per counter, to at most 10" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      channel <- inPlay p "01018" []
      -- max 1 per player
      channel' <- putInHand p "01018"
      offers (isPlay channel') `shouldReturn` False
      clearHand p
      energy <- putInHand p "01088"
      choose "store energy" (isAbilityOfCard channel)
      choose "pay with energy" (isPayWith energy)
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      uses <- getsGame (fmap getUpgradeUses . HashMap.lookup (upgradeOf channel) . gameUpgrades)
      uses `shouldBe` Just 2
      for_ ["01014", "01014"] \code -> do
        absorption <- putInHand p code
        choose "store energy" (isAbilityOfCard channel)
        choose "pay with absorption" (isPayWith absorption)
        whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      choose "discharge" \case
        UseAbility ab -> abilitySource ab == UpgradeRef (upgradeOf channel) && abilityIndex ab == 2
        _ -> False
      vid <- villainIdNow
      whenM (offers (isTargetChoice (EnemyVillainId vid))) $ choose "Rhino" (isTargetChoice (EnemyVillainId vid))
      villainDamageNow `shouldReturn` 10
      upgradeCodes `shouldReturn` []

  it "Family Emergency: flipping to Carol Danvers and exhausting her removes it from the game" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      _ <- revealNow p Obligations.familyEmergency
      choose "flip to alter-ego" (isLabel "$label.changeToB")
      choose "exhaust Carol Danvers" (isLabelStarting "$label.exhaust")
      isExhaustedNow p `shouldReturn` True
      getsGame (HashMap.null . gameObligations) `shouldReturn` True
      isStunned p `shouldReturn` False

  it "Family Emergency: otherwise she is stunned and it surges" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      emergency <- revealNow p Obligations.familyEmergency
      choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
      pass
      isStunned p `shouldReturn` True
      (map ecCardId <$> encounterDiscard) >>= (`shouldSatisfy` elem (ecCardId emergency))
      dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)

  it "The Psyche-Magnitron enters with 3 threat and 1 more per hero, and is a hazard" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      magnitron <- revealNow p SideSchemes.thePsycheMagnitron
      pass
      sideSchemeThreat (sideSchemeOf magnitron) `shouldReturn` 4
      getHazardCount `shouldReturn` 1

  it "Yon-Rogg places 1 threat on The Psyche-Magnitron after he attacks" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      magnitron <- revealNow p SideSchemes.thePsycheMagnitron
      pass
      yonRogg <- revealNow p Minions.yonRogg
      pass
      runNow [MinionMessage (minionOf yonRogg) (MinionAttacks p)]
      choose "no defender" isNoDefender
      choose "Yon-Rogg's forced response" isAnyAbility
      pass
      damageOf p `shouldReturn` 3
      sideSchemeThreat (sideSchemeOf magnitron) `shouldReturn` 5

  it "Kree Manipulator places 1 threat on the main scheme and surges" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      before <- mainSchemeThreat
      _ <- revealNow p Treacheries.kreeManipulator
      pass
      mainSchemeThreat `shouldReturn` (before + 1)
      dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)

  it "Kree Manipulator as a boost places 1 threat when the villain's attack is undefended" do
    let attackedWith defend = do
          tg <- captainMarvel
          let p = player1 tg
          inGame tg do
            changeForm p
            _ <- stackEncounterDeck [Treacheries.kreeManipulator]
            before <- mainSchemeThreat
            vid <- villainIdNow
            runNow [VillainMessage vid (VillainAttacks p)]
            choose "defend or not" (if defend then isDefend else isNoDefender)
            pass
            subtract before <$> mainSchemeThreat
    attackedWith False `shouldReturn` 1
    attackedWith True `shouldReturn` 0

  it "Yon-Rogg's Treason discards each energy resource from hand, and surges when there is none" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      clearHand p
      energy <- putInHand p "01088"
      absorption <- putInHand p "01014"
      genius <- putInHand p "01089"
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      _ <- revealNow p Treacheries.yonRoggsTreason
      pass
      handOf p `shouldReturn` [genius]
      discardOf p >>= (`shouldSatisfy` \d -> energy `elem` d && absorption `elem` d)
      dealtTo p >>= (`shouldSatisfy` not . (ecCardId next `elem`) . map ecCardId)
      [next'] <- stackEncounterDeck [Minions.hydraMercenary]
      _ <- revealNow p Treacheries.yonRoggsTreason
      pass
      handOf p `shouldReturn` [genius]
      dealtTo p >>= (`shouldSatisfy` (ecCardId next' `elem`) . map ecCardId)

  it "Energy Channel's discharge is an attack: what answers an attack answers it" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      channel <- inPlay p "01018" []
      jarnbjorn <- putIntoPlay p "06019"
      energy <- putInHand p "01088"
      choose "store energy" (isAbilityOfCard channel)
      choose "pay with energy" (isPayWith energy)
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      _ <- putInHand p "01090"
      choose "discharge" \case
        UseAbility ab -> abilitySource ab == UpgradeRef (upgradeOf channel) && abilityIndex ab == 2
        _ -> False
      vid <- villainIdNow
      choose "the target" (isTargetChoice (EnemyVillainId vid))
      offers (isAbilityOfCard jarnbjorn) `shouldReturn` True

  it "Energy Channel's discharge is an attack: a stunned hero only loses the stun" do
    tg <- captainMarvel
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      channel <- inPlay p "01018" []
      energy <- putInHand p "01088"
      choose "store energy" (isAbilityOfCard channel)
      choose "pay with energy" (isPayWith energy)
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      runNow [IdentityMessage p IdentityStunned]
      choose "discharge" \case
        UseAbility ab -> abilitySource ab == UpgradeRef (upgradeOf channel) && abilityIndex ab == 2
        _ -> False
      isStunned p `shouldReturn` False
      villainDamageNow `shouldReturn` 0
      upgradeCodes `shouldReturn` []
