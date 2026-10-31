module Marvel.Aspect.PhoenixPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Minion.Cards qualified as Minions
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Discard (Discard (..))
import Marvel.Phase (Phase (..))
import Marvel.Status
import Marvel.Villain.Types (villainConfused)
import TestImport hiding (hero)

hero :: TestGame -> GameAppT ()
hero tg = changeForm (player1 tg) >> pass >> clearHand (player1 tg)

allyNow :: CardCode -> GameAppT (Attrs Ally)
allyNow = allyIn

powerNow :: GameAppT Natural
powerNow = getsGame $ sum . map (upgradeUses . toAttrs) . filter ((`elem` ["34002a", "34002b"]) . toCardCode . getCardDef) . HashMap.elems . gameUpgrades

spec :: Spec
spec = describe "Phoenix pack" do
  it "Banshee confuses a minion after thwarting and takes consequential damage" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putIntoPlay p "34014"
      a <- allyNow "34014"
      mid <- addMinion p Minions.hydraSoldier
      placeMainThreat 5
      choose "Banshee thwart" (isAllyThwart a)
      sid <- mainSchemeIdNow
      choose "main" (isTargetChoice sid)
      choose "Banshee response" (isAbilityOfCard card)
      choose "minion" (isTargetChoice mid)
      m <- getsGame $ toAttrs . fromMaybe (error "minion") . HashMap.lookup mid . gameMinions
      Minion.minionConfused m `shouldBe` True
      allyDamage <$> allyNow "34014" `shouldReturn` 1

  it "Banshee has no response after attacking" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putIntoPlay p "34014"
      _ <- addMinion p Minions.weaponsRunner
      a <- allyNow "34014"
      choose "Banshee attack" (isAllyAttack a)
      chooseVillain
      offers (isAbilityOfCard card) `shouldReturn` False

  it "Marvel Girl removes the attacked minion's printed SCH from the main scheme" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putIntoPlay p "34015"
      mid <- addMinion p Minions.hydraSoldier
      placeMainThreat 5
      before <- mainSchemeThreatNow
      a <- allyNow "34015"
      choose "Marvel Girl attack" (isAllyAttack a)
      choose "minion" (isTargetChoice mid)
      choose "Marvel Girl interrupt" (isAbilityOfCard card)
      sid <- mainSchemeIdNow
      choose "main" (isTargetChoice sid)
      mainSchemeThreatNow `shouldReturn` (before - 1)
      allyDamage <$> allyNow "34015" `shouldReturn` 1

  it "Marvel Girl does not respond when she attacks the villain" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putIntoPlay p "34015"
      placeMainThreat 4
      a <- allyNow "34015"
      choose "attack" (isAllyAttack a)
      chooseVillain
      offers (isAbilityOfCard card) `shouldReturn` False

  it "Mission Training gives an X-Men ally one THW and two hit points" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34014"
      a <- allyNow "34014"
      card <- putInHand p "34016"
      pay <- putInHand p "01090"
      playOn card (allyId a) [pay]
      modified <- allyNow "34014"
      Ally.getModifiedHitPoints modified `shouldReturn` 5
      Ally.getModifiedThwart modified `shouldReturn` 3
      another <- putInHand p "34016"
      _ <- putInHand p "01088"
      offers (isPlay another) `shouldReturn` False

  it "Mission Training cannot be attached to a non-X-Men ally" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "01083"
      card <- putInHand p "34016"
      _ <- putInHand p "01090"
      offers (isPlay card) `shouldReturn` False

  it "Storm costs one less for a Mutant and has two THW and ATK" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "34021"
      a <- putInHand p "01090"
      b <- putInHand p "01089"
      playWith card [a,b]
      storm <- allyNow "34021"
      Ally.getModifiedAttack storm `shouldReturn` 2
      Ally.getModifiedThwart storm `shouldReturn` 2
      length <$> handOf p `shouldReturn` 0

  it "Storm moves two threat to another scheme before removing her thwart amount" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putIntoPlay p "34021"
      sc <- revealNow p Schemes.bombScare
      pass
      let dest = sideSchemeOf sc
      priorSide <- sideSchemeThreat dest
      placeMainThreat 6
      before <- mainSchemeThreatNow
      a <- allyNow "34021"
      choose "Storm thwart" (isAllyThwart a)
      sid <- mainSchemeIdNow
      choose "main" (isTargetChoice sid)
      choose "Storm interrupt" (isAbilityOfCard card)
      choose "destination" (isTargetChoice dest)
      mainSchemeThreatNow `shouldReturn` (before - 4)
      sideSchemeThreat dest `shouldReturn` (priorSide + 2)

  it "Storm cannot move threat when there is only one scheme" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putIntoPlay p "34021"
      placeMainThreat 4
      a <- allyNow "34021"
      choose "thwart" (isAllyThwart a)
      sid <- mainSchemeIdNow
      choose "main" (isTargetChoice sid)
      offers (isAbilityOfCard card) `shouldReturn` False

  it "Cerebro searches the full deck when Jean controls a Psionic character" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let p = player1 tg
      clearHand p
      cerebro <- putIntoPlay p "34022"
      stacked <- stackDeck p ["01090", "01089", "01088", "34010", "34011", "34014"]
      choose "Cerebro" (isAbilityOfCard cerebro)
      choose "Banshee beyond top five" (isTargetChoice $ last stacked)
      map toCardCode <$> handOf p `shouldReturn` ["34014"]
      offers (isAbilityOfCard cerebro) `shouldReturn` False

  it "Cerebro can only be played by a Mutant identity" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "34022"
      _ <- putInHand p "01090"
      offers (isPlay card) `shouldReturn` False

  it "Mutant Peacekeepers may exhaust only Phoenix and removes her total THW" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      placeMainThreat 6
      before <- mainSchemeThreatNow
      card <- putInHand p "34018"
      one <- putInHand p "34011"
      playWith card [one]
      -- With only one legal scheme, the divided-thwart pipeline allocates the
      -- entire pool automatically.
      mainSchemeThreatNow `shouldReturn` (before - 3)
      playerIdentityExhausted <$> playerAttrs p `shouldReturn` True

  it "Psychic Assault deals three attack damage and confuses its target" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putInHand p "34032"
      two <- putInHand p "01090"
      playWith card [two]
      chooseVillain
      villainDamageNow `shouldReturn` 3
      villainConfused . toAttrs <$> getVillain `shouldReturn` True

  it "Psionic events cannot be played without the Psionic trait" do
    tg <- testGame ["spider-man"]
    inGame tg do
      hero tg
      let p = player1 tg
      assault <- putInHand p "34032"
      kicker <- putInHand p "34034"
      _ <- putInHand p "01090"
      _ <- putIntoPlay p "01083"
      offers (isPlay assault) `shouldReturn` False
      offers (isPlay kicker) `shouldReturn` False

  it "Psychic Kicker readies an ally and boosts only its next basic attack" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34014"
      a <- allyNow "34014"
      resolve [AllyMessage (allyId a) ExhaustedAlly]
      card <- putInHand p "34034"
      playWith card []
      choose "Banshee" (isTargetChoice $ allyId a)
      allyExhausted <$> allyNow "34014" `shouldReturn` False
      choose "attack" (isAllyAttack a)
      chooseVillain
      villainDamageNow `shouldReturn` 4
      after <- allyNow "34014"
      Ally.getModifiedAttack after `shouldReturn` 2
      Ally.getModifiedThwart after `shouldReturn` 2

  it "Psychic Kicker boosts a thwart once" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34014"
      a <- allyNow "34014"
      placeMainThreat 6
      before <- mainSchemeThreatNow
      card <- putInHand p "34034"
      playWith card []
      choose "Banshee" (isTargetChoice $ allyId a)
      choose "thwart" (isAllyThwart a)
      sid <- mainSchemeIdNow
      choose "main" (isTargetChoice sid)
      pass
      mainSchemeThreatNow `shouldReturn` (before - 4)
      after <- allyNow "34014"
      Ally.getModifiedThwart after `shouldReturn` 2

  it "Soul Sisters readies Phoenix and Storm and heals two damage from both" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34021"
      a <- allyNow "34021"
      setDamage p 4
      resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyId a) ExhaustedAlly, AllyMessage (allyId a) (AllyDamaged (toSource p) $ toDamage 2 FromAbility)]
      card <- putInHand p "34035"
      one <- putInHand p "34011"
      playWith card [one]
      damageOf p `shouldReturn` 2
      playerIdentityExhausted <$> playerAttrs p `shouldReturn` False
      allyDamage <$> allyNow "34021" `shouldReturn` 0
      allyExhausted <$> allyNow "34021" `shouldReturn` False

  it "Soul Sisters requires both named characters" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putInHand p "34035"
      _ <- putInHand p "01090"
      offers (isPlay card) `shouldReturn` False

  it "Psychic Rapport readies Phoenix and Cyclops and can replenish Phoenix Force" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34003"
      a <- allyNow "34003"
      before <- powerNow
      resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyId a) ExhaustedAlly]
      card <- putInHand p "34023"
      two <- putInHand p "01090"
      playWith card [two]
      choose "power" (isLabel "$label.psychicRapportPower")
      powerNow `shouldReturn` (before + 2)
      playerIdentityExhausted <$> playerAttrs p `shouldReturn` False
      allyExhausted <$> allyNow "34003" `shouldReturn` False

  it "Passion for Justice adds one threat removal when paying for a thwart event" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      placeMainThreat 6
      before <- mainSchemeThreatNow
      card <- putInHand p "34012"
      passion <- putInHand p "34020"
      one <- putInHand p "34011"
      playWith card [passion,one]
      choose "Passion" (isLabel "$label.passionForJustice")
      sid <- mainSchemeIdNow
      choose "main" (isTargetChoice sid)
      mainSchemeThreatNow `shouldReturn` (before - 5)

  it "Passion for Justice has no interrupt when paying for an attack event" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putInHand p "34032"
      passion <- putInHand p "34020"
      one <- putInHand p "34011"
      playWith card [passion,one]
      offers (isLabel "$label.passionForJustice") `shouldReturn` False
      chooseVillain
      villainDamageNow `shouldReturn` 3

  it "Psychic Misdirection requires another enemy" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putInHand p "34033"
      _ <- putInHand p "01090"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      offers (isPlay card) `shouldReturn` False

  it "Psychic Misdirection sends attack damage to another enemy while defending Phoenix" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      mid <- addMinion p Minions.weaponsRunner
      card <- putInHand p "34033"
      two <- putInHand p "01090"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      playWith card [two]
      choose "redirect to minion" (isTargetChoice mid)
      pass
      damageOf p `shouldReturn` 0
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False

  it "Psychic Manipulation removes boosted scheme threat instead of placing it" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      placeMainThreat 6
      before <- mainSchemeThreatNow
      -- Keep an undrawn card in the deck: drawing its only card with no
      -- discard pile would end the game before the boost can finish resolving.
      _ <- setEncounterDeck [Minions.hydraMercenary, Minions.hydraMercenary] []
      card <- putInHand p "34017"
      two <- putInHand p "01090"
      one <- putInHand p "34011"
      vid <- villainIdNow
      start [VillainMessage vid VillainSchemes]
      playWith card [two,one]
      pass
      mainSchemeThreatNow `shouldReturn` (before - 2)

  it "Psychic Manipulation cannot respond to a scheme replaced by confusion" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putInHand p "34017"
      _ <- putInHand p "01090"
      _ <- putInHand p "34011"
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainConfused (toSource p))]
      start [VillainMessage vid VillainSchemes]
      offers (isPlay card) `shouldReturn` False

  it "Psychic Manipulation cannot thwart the main scheme under a crisis icon" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- revealNow p Schemes.crowdControl
      pass
      card <- putInHand p "34017"
      _ <- putInHand p "01090"
      _ <- putInHand p "34011"
      vid <- villainIdNow
      start [VillainMessage vid VillainSchemes]
      offers (isPlay card) `shouldReturn` False

  it "Cerebro searches only five cards without a controlled Psionic character" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      cerebro <- putIntoPlay p "34022"
      _ <- stackDeck p ["01090", "01089", "01088", "34010", "34011", "34014"]
      choose "Cerebro" (isAbilityOfCard cerebro)
      length <$> handOf p `shouldReturn` 0

  it "Cerebro's action is unavailable in hero form" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      cerebro <- putIntoPlay p "34022"
      offers (isAbilityOfCard cerebro) `shouldReturn` False

  it "Storm also costs one less for an X-Men hero" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putInHand p "34021"
      a <- putInHand p "01090"
      b <- putInHand p "01089"
      playWith card [a,b]
      length <$> handOf p `shouldReturn` 0
      length <$> alliesWith "34021" `shouldReturn` 1

  it "Storm does not discount herself for an unrelated identity" do
    tg <- testGame ["she-hulk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "34021"
      two <- putInHand p "01090"
      anotherTwo <- putInHand p "01089"
      offers (isPlay card) `shouldReturn` False
      one <- putInHand p "34011"
      playWith card [two,anotherTwo,one]
      length <$> handOf p `shouldReturn` 0
      length <$> alliesWith "34021" `shouldReturn` 1

  it "Psychic Kicker's unused boost expires at the end of the phase" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34014"
      a <- allyNow "34014"
      card <- putInHand p "34034"
      playWith card []
      choose "Banshee" (isTargetChoice $ allyId a)
      start [EndPhase PlayerPhase]
      after <- allyNow "34014"
      Ally.getModifiedAttack after `shouldReturn` 2
      Ally.getModifiedThwart after `shouldReturn` 2

  it "Psychic Kicker is consumed by a confused ally's thwart attempt" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34014"
      a <- allyNow "34014"
      resolve [GiveStatus (AllyRef $ allyId a) ConfusedStatus]
      card <- putInHand p "34034"
      playWith card []
      choose "Banshee" (isTargetChoice $ allyId a)
      choose "attempt thwart" (isAllyThwart a)
      after <- allyNow "34014"
      Ally.allyConfused after `shouldBe` False
      Ally.getModifiedThwart after `shouldReturn` 2

  it "Storm moves only the available threat when her scheme has less than two" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putIntoPlay p "34021"
      sc <- revealNow p Schemes.bombScare
      pass
      let dest = sideSchemeOf sc
      priorSide <- sideSchemeThreat dest
      before <- mainSchemeThreatNow
      main <- mainSchemeIdNow
      when (before > 0) $ resolve [MainSchemeMessage main (MainSchemeThreatRemoved (toSource p) before)]
      placeMainThreat 1
      a <- allyNow "34021"
      choose "Storm thwart" (isAllyThwart a)
      sid <- mainSchemeIdNow
      choose "main" (isTargetChoice sid)
      choose "Storm interrupt" (isAbilityOfCard card)
      choose "destination" (isTargetChoice dest)
      mainSchemeThreatNow `shouldReturn` 0
      sideSchemeThreat dest `shouldReturn` (priorSide + 1)

  it "Two copies of Passion for Justice add two total threat removal" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      placeMainThreat 6
      before <- mainSchemeThreatNow
      card <- putInHand p "34012"
      first <- putInHand p "34020"
      second <- putInHand p "34020"
      playWith card [first,second]
      choose "first Passion" (isLabel "$label.passionForJustice")
      choose "second Passion" (isLabel "$label.passionForJustice")
      sid <- mainSchemeIdNow
      choose "main" (isTargetChoice sid)
      mainSchemeThreatNow `shouldReturn` (before - 6)

  it "Psychic Manipulation changes only the current scheme activation" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      placeMainThreat 5
      before <- mainSchemeThreatNow
      _ <- setEncounterDeck [Minions.hydraMercenary, Minions.hydraMercenary] []
      card <- putInHand p "34017"
      two <- putInHand p "01090"
      one <- putInHand p "34011"
      vid <- villainIdNow
      start [VillainMessage vid VillainSchemes]
      playWith card [two,one]
      pass
      mainSchemeThreatNow `shouldReturn` (before - 2)
      start [VillainMessage vid VillainSchemes]
      pass
      mainSchemeThreatNow `shouldReturn` before

  it "Psychic Misdirection cannot redirect to the attacking enemy" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      mid <- addMinion p Minions.weaponsRunner
      card <- putInHand p "34033"
      two <- putInHand p "01090"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      playWith card [two]
      offers (isTargetChoice vid) `shouldReturn` False
      choose "other enemy" (isTargetChoice mid)
      pass
      damageOf p `shouldReturn` 0

  it "Psychic Misdirection sends overkill from the defeated minion to the villain" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      mid <- addMinion p Minions.weaponsRunner
      charge <- revealNow p Attachments.charge
      pass
      card <- putInHand p "34033"
      two <- putInHand p "01090"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      choose "Charge forced interrupt" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId charge)
      playWith card [two]
      choose "other enemy" (isTargetChoice mid)
      pass
      damageOf p `shouldReturn` 0
      villainDamageNow `shouldReturn` 3

  it "Psychic Rapport can return a Cyclops signature card from the payer's discard" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34003"
      fixture <- putInDiscard p "34005"
      let cyclopsFixture = fixture {pcCardDef = (getCardDef fixture) {cdCardCode = "33003", cdAlternateCardCodes = []}}
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
        PlayerIdentity a {playerIdentityDiscard = Discard [cyclopsFixture]}
      card <- putInHand p "34023"
      two <- putInHand p "01090"
      playWith card [two]
      choose "return card" (isLabel "$label.psychicRapportReturnCard")
      choose "Cyclops card" (isTargetChoice cyclopsFixture)
      cards <- handOf p
      map toCardCode cards `shouldSatisfy` elem "33003"

  it "Psychic Rapport cannot return Phoenix's own signature cards through its Cyclops branch" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34003"
      _ <- putInDiscard p "34005"
      card <- putInHand p "34023"
      two <- putInHand p "01090"
      playWith card [two]
      offers (isLabel "$label.psychicRapportReturnCard") `shouldReturn` False
      choose "power" (isLabel "$label.psychicRapportPower")

  it "Psychic Misdirection lets a redirected target's tough status prevent its damage" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      mid <- addMinion p Minions.weaponsRunner
      resolve [MinionMessage mid MinionBecomeTough]
      card <- putInHand p "34033"
      two <- putInHand p "01090"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      playWith card [two]
      choose "tough minion" (isTargetChoice mid)
      pass
      damageOf p `shouldReturn` 0
      m <- getsGame $ toAttrs . fromMaybe (error "minion") . HashMap.lookup mid . gameMinions
      Minion.minionDamage m `shouldBe` 0
      Minion.minionTough m `shouldBe` False

  it "Psychic Misdirection can redirect a minion's attack to the villain" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      mid <- addMinion p Minions.weaponsRunner
      card <- putInHand p "34033"
      two <- putInHand p "01090"
      start [MinionMessage mid (MinionAttacks p)]
      playWith card [two]
      chooseVillain
      pass
      damageOf p `shouldReturn` 0
      villainDamageNow `shouldReturn` 1

  it "Passion for Justice increases Psychic Manipulation's eventual boosted removal" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      placeMainThreat 6
      before <- mainSchemeThreatNow
      -- Keep an undrawn card in the deck: drawing its only card with no
      -- discard pile would end the game before the boost can finish resolving.
      _ <- setEncounterDeck [Minions.hydraMercenary, Minions.hydraMercenary] []
      card <- putInHand p "34017"
      passion <- putInHand p "34020"
      two <- putInHand p "01090"
      vid <- villainIdNow
      start [VillainMessage vid VillainSchemes]
      playWith card [passion,two]
      choose "Passion" (isLabel "$label.passionForJustice")
      pass
      mainSchemeThreatNow `shouldReturn` (before - 3)

  it "Storm's moved threat is placement and can trigger Consume the World's loss" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putIntoPlay p "34021"
      consumed <- revealNow p Schemes.consumeTheWorld
      pass
      let dest = sideSchemeOf consumed
      resolve [SideSchemeMessage dest (SideSchemePlaceThreat 5)]
      placeMainThreat 4
      a <- allyNow "34021"
      choose "Storm thwart" (isAllyThwart a)
      main <- mainSchemeIdNow
      choose "main" (isTargetChoice main)
      choose "Storm interrupt" (isAbilityOfCard card)
      choose "move into Consume the World" (isTargetChoice dest)
      getsGame gameState `shouldReturn` Finished Lost

  it "Storm cannot move threat off a restricted main scheme while crisis is in play" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34021"
      blocked <- revealNow p Schemes.crowdControl
      pass
      let dest = sideSchemeOf blocked
      placeMainThreat 4
      before <- mainSchemeThreatNow
      beforeSide <- sideSchemeThreat dest
      a <- allyNow "34021"
      main <- mainSchemeIdNow
      movement <- Lifted.runQueueT $ Lifted.capture $
        Lifted.moveSchemeThreat a (SchemeMainSchemeId main) (SchemeSideSchemeId dest) 2
      resolve movement
      mainSchemeThreatNow `shouldReturn` before
      sideSchemeThreat dest `shouldReturn` beforeSide

  it "Banshee does not offer a confusion response when every minion is stalwart" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putIntoPlay p "34014"
      _ <- addMinion p Minions.kreeLieutenant
      placeMainThreat 5
      a <- allyNow "34014"
      choose "Banshee thwart" (isAllyThwart a)
      main <- mainSchemeIdNow
      choose "main" (isTargetChoice main)
      offers (isAbilityOfCard card) `shouldReturn` False

  it "Banshee cannot choose an already confused minion" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      card <- putIntoPlay p "34014"
      confused <- addMinion p Minions.weaponsRunner
      eligible <- addMinion p Minions.hydraSoldier
      resolve [MinionMessage confused (MinionConfused $ toSource p)]
      placeMainThreat 5
      a <- allyNow "34014"
      choose "Banshee thwart" (isAllyThwart a)
      main <- mainSchemeIdNow
      choose "main" (isTargetChoice main)
      choose "Banshee response" (isAbilityOfCard card)
      offers (isTargetChoice confused) `shouldReturn` False
      choose "eligible minion" (isTargetChoice eligible)
      m <- getsGame $ toAttrs . fromMaybe (error "minion") . HashMap.lookup eligible . gameMinions
      Minion.minionConfused m `shouldBe` True

  it "Mutant Peacekeepers adds exhausted X-Men allies' THW without consequential damage" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34014"
      _ <- putIntoPlay p "01083"
      clearHand p
      banshee <- allyNow "34014"
      nonXmen <- allyNow "01083"
      placeMainThreat 6
      before <- mainSchemeThreatNow
      card <- putInHand p "34018"
      one <- putInHand p "34011"
      playWith card [one]
      offers (isTargetChoice $ allyId nonXmen) `shouldReturn` False
      choose "exhaust Banshee" (isTargetChoice $ allyId banshee)
      mainSchemeThreatNow `shouldReturn` (before - 5)
      playerIdentityExhausted <$> playerAttrs p `shouldReturn` True
      allyExhausted <$> allyNow "34014" `shouldReturn` True
      allyExhausted <$> allyNow "01083" `shouldReturn` False
      allyDamage <$> allyNow "34014" `shouldReturn` 0

  it "Mutant Peacekeepers' threat removal opens the normal thwart interrupt window" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      sc <- revealNow p Schemes.consumeTheWorld
      pass
      let side = sideSchemeOf sc
      placeMainThreat 6
      main <- mainSchemeIdNow
      overwatch <- putInHand p "30019"
      playOn overwatch main []
      card <- putInHand p "34018"
      one <- putInHand p "34011"
      playWith card [one]
      choose "assign first threat to main" (isTargetChoice main)
      choose "assign second threat to main" (isTargetChoice main)
      choose "assign third threat to main" (isTargetChoice main)
      choose "Overwatch thwart interrupt" (isAbilityOfCard overwatch)
      choose "remove matching threat elsewhere" (isTargetChoice side)
      mainSchemeThreatNow `shouldReturn` 3
      sideSchemeThreat side `shouldReturn` 3

  it "Mutant Peacekeepers cannot be played with only a main scheme blocked by Patrol" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      mid <- addMinion p Minions.weaponsRunner
      gate <- putInHand p "32044"
      playOn gate mid []
      placeMainThreat 5
      card <- putInHand p "34018"
      _ <- putInHand p "34011"
      offers (isPlay card) `shouldReturn` False

  it "Mutant Peacekeepers cannot remove threat from the main scheme through crisis" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      sc <- revealNow p Schemes.crowdControl
      pass
      placeMainThreat 5
      before <- mainSchemeThreatNow
      card <- putInHand p "34018"
      one <- putInHand p "34011"
      playWith card [one]
      mainSchemeThreatNow `shouldReturn` before
      sideSchemeThreat (sideSchemeOf sc) `shouldReturn` 0

  it "Mutant Peacekeepers can thwart a side scheme while Patrol protects the main scheme" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      sc <- revealNow p Schemes.consumeTheWorld
      pass
      mid <- addMinion p Minions.weaponsRunner
      gate <- putInHand p "32044"
      playOn gate mid []
      placeMainThreat 5
      before <- mainSchemeThreatNow
      card <- putInHand p "34018"
      one <- putInHand p "34011"
      playWith card [one]
      mainSchemeThreatNow `shouldReturn` before
      sideSchemeThreat (sideSchemeOf sc) `shouldReturn` 3

  it "Passion for Justice adds one to Mutant Peacekeepers' total pool split across two schemes" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      sc <- revealNow p Schemes.consumeTheWorld
      pass
      let side = sideSchemeOf sc
      placeMainThreat 6
      main <- mainSchemeIdNow
      card <- putInHand p "34018"
      passion <- putInHand p "34020"
      playWith card [passion]
      choose "Passion for Justice" (isLabel "$label.passionForJustice")
      choose "first pool point to main" (isTargetChoice main)
      choose "second pool point to main" (isTargetChoice main)
      choose "third pool point to side" (isTargetChoice side)
      choose "fourth pool point to side" (isTargetChoice side)
      mainSchemeThreatNow `shouldReturn` 4
      sideSchemeThreat side `shouldReturn` 4
      playerIdentityExhausted <$> playerAttrs p `shouldReturn` True

  it "Confused Phoenix can pay for Mutant Peacekeepers with no threat to clear confusion" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      mainSchemeThreatNow `shouldReturn` 0
      resolve [GiveStatus (IdentityRef p) ConfusedStatus]
      card <- putInHand p "34018"
      one <- putInHand p "34011"
      playWith card [one]
      playerIdentityConfused <$> playerAttrs p `shouldReturn` False
      playerIdentityExhausted <$> playerAttrs p `shouldReturn` True
      mainSchemeThreatNow `shouldReturn` 0
      length <$> handOf p `shouldReturn` 0

  it "Confused Phoenix can play Psychic Manipulation through crisis and Patrol without replacing the scheme" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- revealNow p Schemes.crowdControl
      pass
      mid <- addMinion p Minions.weaponsRunner
      gate <- putInHand p "32044"
      playOn gate mid []
      placeMainThreat 3
      before <- mainSchemeThreatNow
      resolve [GiveStatus (IdentityRef p) ConfusedStatus]
      _ <- setEncounterDeck [Minions.hydraMercenary, Minions.hydraMercenary, Minions.hydraMercenary] []
      card <- putInHand p "34017"
      two <- putInHand p "01090"
      one <- putInHand p "34011"
      vid <- villainIdNow
      start [VillainMessage vid VillainSchemes]
      playWith card [two,one]
      pass
      playerIdentityConfused <$> playerAttrs p `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` (before + 2)

  it "Soul Sisters cannot be played when Phoenix and Storm are both ready and undamaged" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34021"
      card <- putInHand p "34035"
      _ <- putInHand p "34011"
      offers (isPlay card) `shouldReturn` False

  it "Soul Sisters can be played for healing even when both named characters are ready" do
    tg <- testGame ["phoenix"]
    inGame tg do
      hero tg
      let p = player1 tg
      _ <- putIntoPlay p "34021"
      setDamage p 1
      card <- putInHand p "34035"
      one <- putInHand p "34011"
      playWith card [one]
      damageOf p `shouldReturn` 0
      playerIdentityExhausted <$> playerAttrs p `shouldReturn` False
      allyExhausted <$> allyNow "34021" `shouldReturn` False
