module Marvel.Encounter.NovaNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types qualified as Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Attack
import Marvel.EncounterSet (EncounterSet (NovaNemesis), getNemesisSet)
import Marvel.Game.Source
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Status
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

asNova :: IdentityId -> GameAppT ()
asNova p = changeForm p >> pass >> clearHand p

-- Forced encounter abilities are explicit client choices, even if only one exists.
settle :: GameAppT ()
settle = do
  pressWhenRevealed
  forced <- offers isForced
  if forced then choose "resolve the forced encounter ability" isForced >> settle
    else do
      pass
      whenM (offers isForced) settle
 where
  isForced = \case
    UseAbility a -> abilityType a `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse]
    _ -> False

minionAttrs :: MinionId -> GameAppT (Attrs Minion)
minionAttrs mid = getsGame (maybe (error "missing minion") toAttrs . HashMap.lookup mid . gameMinions)

toughCount :: MinionId -> GameAppT Natural
toughCount mid = do
  a <- minionAttrs mid
  pure $ (if minionTough a then 1 else 0) + minionAdditionalTough a

attackOf :: MinionId -> GameAppT Natural
attackOf mid = Minion.getModifiedAttack =<< minionAttrs mid

obligationOf :: EncounterCard -> ObligationId
obligationOf = ObligationId . unCardId . ecCardId

attachmentTarget :: CardCode -> GameAppT (Maybe EnemyId)
attachmentTarget code = getsGame $ headMay . mapMaybe (Attachment.attachmentEnemy . toAttrs)
  . filter ((== code) . toCardCode . getCardDef) . HashMap.elems . gameAttachments

prepareSurge :: GameAppT EncounterCard
prepareSurge = do
  [marker] <- stackEncounterDeck [Minions.hydraMercenary]
  pure marker

wasDealt :: IdentityId -> EncounterCard -> GameAppT Bool
wasDealt p card = elem (ecCardId card) . map ecCardId <$> dealtTo p

-- A card with two printed wild icons distinguishes counting cards from counting icons.
putDoubleWild :: IdentityId -> GameAppT PlayerCard
putDoubleWild p = do
  cid <- getRandom
  let card = MkPlayerCard cid ((lookupPlayerCard "28007") {cdResources = [(PrintedResource, Wild), (PrintedResource, Wild)]}) (Just p) (Just p)
  withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
    PlayerIdentity a {playerIdentityHand = Hand (card : unHand (playerIdentityHand a))}
  pure card

spec :: Spec
spec = describe "Nova nemesis and Armadillo modular" do
  it "registers Nova's five-card nemesis set" do
    getNemesisSet "28001a" `shouldBe` NovaNemesis :: IO ()

  describe "Weight of the World" do
    it "stays in play and stops the Helmet from readying until Sam exhausts to remove it" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        helmet <- putIntoPlay p "28009"
        resolve [UpgradeMessage (upgradeOf helmet) ExhaustedUpgrade]
        obligation <- revealNow p Obligations.weightOfTheWorld
        settle
        obligationsInPlay `shouldReturn` 1
        resolve [UpgradeMessage (upgradeOf helmet) ReadiedUpgrade]
        upgradeIsExhausted (upgradeOf helmet) `shouldReturn` True
        choose "remove the obligation" (isAbilityOf $ ObligationRef $ obligationOf obligation)
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        resolve [UpgradeMessage (upgradeOf helmet) ReadiedUpgrade]
        upgradeIsExhausted (upgradeOf helmet) `shouldReturn` False

    it "cannot be removed by its alter-ego action while Nova is in hero form" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        obligation <- revealNow p Obligations.weightOfTheWorld
        settle
        offers (isAbilityOf $ ObligationRef $ obligationOf obligation) `shouldReturn` False
        obligationsInPlay `shouldReturn` 1

  describe "Bring the War!" do
    it "starts with two fixed threat for two players and adds a hazard icon" do
      tg <- testGame ["nova", "spider-man"]
      inGame tg do
        card <- revealNow (player1 tg) Schemes.bringTheWar
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 2
        hazards <- getsGame $ maybe 0 (cdHazards . getCardDef) . HashMap.lookup (sideSchemeOf card) . gameSideSchemes
        hazards `shouldBe` 1

  describe "Warbringer" do
    it "counts cards with printed wild resources, gives overkill to that attack, and loses the bonus afterward" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        mid <- addMinion p Minions.warbringer
        _ <- putDoubleWild p
        _ <- putInHand p "01089"
        runNow [MinionMessage mid (MinionAttacks p)]
        choose "resolve Warbringer" (isAbilityOf mid)
        a <- minionAttacking <$> minionAttrs mid
        attackValue <$> a `shouldBe` Just 4
        attackOverkill <$> a `shouldBe` Just True
        choose "take the attack" isNoDefender
        settle
        damageOf p `shouldReturn` 4
        attackOf mid `shouldReturn` 3
        clearHand p
        runNow [MinionMessage mid (MinionAttacks p)]
        settle
        damageOf p `shouldReturn` 7

    it "deals overkill past a defending ally" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        mid <- addMinion p Minions.warbringer
        ally <- putIntoPlay p "01084"
        clearHand p
        _ <- putInHand p "28007"
        _ <- putInHand p "28019"
        runNow [MinionMessage mid (MinionAttacks p)]
        choose "resolve Warbringer" (isAbilityOf mid)
        choose "Mockingbird defends" \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
        settle
        damageOf p `shouldReturn` 2

    it "does not activate or apply a bonus while stunned" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        mid <- addMinion p Minions.warbringer
        _ <- putInHand p "28007"
        resolve [MinionMessage mid (MinionStunned GameRef), MinionMessage mid (MinionAttacks p)]
        damageOf p `shouldReturn` 0
        minionStunned <$> minionAttrs mid `shouldReturn` False
        attackOf mid `shouldReturn` 3

  describe "War Delivery" do
    it "allows a wild payment to prevent both attacks" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- addMinion p Minions.warbringer
        wild <- putInHand p "28007"
        _ <- revealNow p Treacheries.warDelivery
        choose "spend a wild" (isLabel "$label.spendWildResource")
        payWith [wild]
        settle
        damageOf p `shouldReturn` 0
        handOf p >>= (`shouldSatisfy` notElem wild)

    it "offers no payment with only nonwild resources and both enemies attack even in alter-ego" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- addMinion p Minions.warbringer
        _ <- putInHand p "01089"
        _ <- stackEncounterDeck [Treacheries.toughItOut]
        _ <- revealNow p Treacheries.warDelivery
        settle
        damageOf p `shouldReturn` 5

    it "allows declining an affordable payment and attacks only with enemies in play" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "28007"
        _ <- stackEncounterDeck [Treacheries.toughItOut]
        _ <- revealNow p Treacheries.warDelivery
        choose "decline the payment" (isLabel "$label.declineWarDeliveryPayment")
        settle
        damageOf p `shouldReturn` 2

    it "its boost places threat for each wild-bearing card, counting a double-wild card once" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        _ <- putDoubleWild p
        _ <- putInHand p "28019"
        _ <- putInHand p "01089"
        _ <- stackEncounterDeck [Treacheries.warDelivery]
        vid <- villainIdNow
        before <- mainSchemeThreatNow
        runNow [VillainMessage vid (VillainAttacks p)]
        settle
        mainSchemeThreatNow `shouldReturn` before + 2

  describe "The War's Been Brought" do
    it "counts every printed wild in hand, discard, and controlled cards, then surges" do
      tg <- testGame ["nova", "spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putDoubleWild p
        _ <- stackDiscard p ["28007"]
        _ <- putIntoPlay p "27035"
        _ <- putIntoPlay (player2 tg) "27048"
        _ <- setEncounterDeck (replicate 8 Minions.hydraMercenary) []
        before <- encounterDeckSize
        _ <- revealNow p Treacheries.theWarsBeenBrought
        settle
        encounterDeckSize `shouldReturn` before - 5
        length <$> dealtTo p `shouldReturn` 1
        encounterDiscardCodes >>= (`shouldSatisfy` (>= 4) . length . filter (== "01101"))

    it "still surges when the total is zero" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        clearHand p
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDiscard = Discard []}
        marker <- prepareSurge
        _ <- revealNow p Treacheries.theWarsBeenBrought
        settle
        wasDealt p marker `shouldReturn` True

  describe "Armored Assault" do
    it "starts with three threat per player and increases only tough enemies' ATK" do
      tg <- testGame ["nova", "spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.hydraSoldier
        _ <- addMinion p Minions.hydraRegular
        card <- revealNow p Schemes.armoredAssault
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 6
        attackOf mid `shouldReturn` 2
        vid <- villainIdNow
        resolve [GiveStatus (toRef mid) ToughStatus, GiveStatus (toRef vid) ToughStatus]
        attackOf mid `shouldReturn` 5
        getVillainAttackValue vid `shouldReturn` 5
        resolve [RemoveStatus (toRef mid) ToughStatus, RemoveStatus (toRef vid) ToughStatus]
        attackOf mid `shouldReturn` 2
        getVillainAttackValue vid `shouldReturn` 2

  describe "Armadillo" do
    it "enters tough, stacks one tough after scheming, and consumes one per damage instance" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.armadillo
        toughCount mid `shouldReturn` 1
        resolve [MinionMessage mid MinionSchemes]
        toughCount mid `shouldReturn` 2
        resolve [MinionMessage mid $ MinionDamaged GameRef (toDamage 1 FromAbility)]
        toughCount mid `shouldReturn` 1
        minionDamageOf mid `shouldReturn` Just 0
        resolve [MinionMessage mid $ MinionDamaged GameRef (withPiercing $ toDamage 1 FromAbility)]
        toughCount mid `shouldReturn` 0
        minionDamageOf mid `shouldReturn` Just 1

    it "gains tough after attacking but gains none when stun or confusion replaces activation" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        mid <- addMinion p Minions.armadillo
        runNow [MinionMessage mid (MinionAttacks p)]
        settle
        toughCount mid `shouldReturn` 2
        resolve [MinionMessage mid (MinionStunned GameRef), MinionMessage mid (MinionAttacks p)]
        toughCount mid `shouldReturn` 2
        resolve [MinionMessage mid (MinionConfused GameRef), MinionMessage mid MinionSchemes]
        toughCount mid `shouldReturn` 2

  describe "Rollin', Rollin'" do
    it "attaches to an existing Armadillo, adds two ATK and prohibits every defender only while tough" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        mid <- addMinion p Minions.armadillo
        ally <- putIntoPlay p "28002"
        _ <- revealNow p Attachments.rollinRollin
        settle
        attachmentTarget "28030" `shouldReturn` Just (EnemyMinionId mid)
        attackOf mid `shouldReturn` 4
        runNow [MinionMessage mid (MinionAttacks p)]
        offers isDefend `shouldReturn` False
        offers (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False) `shouldReturn` False
        settle
        resolve [RemoveStatus (toRef mid) ToughStatus, RemoveStatus (toRef mid) ToughStatus]
        runNow [MinionMessage mid (MinionAttacks p)]
        offers isDefend `shouldReturn` True
        offers (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False) `shouldReturn` True
        choose "defend as Nova" isDefend
        settle

    it "finds Armadillo in the encounter discard, puts him into play engaged with the revealer, and attaches" do
      tg <- testGame ["nova", "spider-man"]
      let p = player2 tg
      inGame tg do
        _ <- setEncounterDeck [Minions.hydraMercenary] [Minions.armadillo]
        _ <- revealNow p Attachments.rollinRollin
        settle
        mid <- selectJust (MinionIs Minions.armadillo)
        minionEngagedIdentity <$> minionAttrs mid `shouldReturn` p
        attachmentTarget "28030" `shouldReturn` Just (EnemyMinionId mid)
        toughCount mid `shouldReturn` 1
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "28029")

    it "finds Armadillo in the encounter deck, and discards itself when he cannot be found" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Minions.armadillo, Minions.hydraMercenary] []
        _ <- revealNow p Attachments.rollinRollin
        settle
        mid <- selectJust (MinionIs Minions.armadillo)
        attachmentTarget "28030" `shouldReturn` Just (EnemyMinionId mid)
        resolve [RemoveFromPlay (toRef mid)]
        _ <- setEncounterDeck [Minions.hydraMercenary] []
        _ <- revealNow p Attachments.rollinRollin
        settle
        attachmentTarget "28030" `shouldReturn` Nothing
        encounterDiscardCodes >>= (`shouldSatisfy` elem "28030")

  describe "Tough and Tumble" do
    it "makes tough villain and minion scheme in alter-ego without surge" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.armadillo
        vid <- villainIdNow
        resolve [GiveStatus (toRef vid) ToughStatus]
        _ <- stackEncounterDeck [Treacheries.toughItOut]
        before <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.toughAndTumble
        settle
        mainSchemeThreatNow `shouldReturn` before + 2
        length <$> dealtTo p `shouldReturn` 0

    it "makes only tough enemies attack in hero form" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        mid <- addMinion p Minions.armadillo
        _ <- addMinion p Minions.hydraSoldier
        _ <- revealNow p Treacheries.toughAndTumble
        settle
        damageOf p `shouldReturn` 2
        toughCount mid `shouldReturn` 2
        length <$> dealtTo p `shouldReturn` 0

    it "does not surge when one eligible enemy activates and another is stunned" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        _ <- addMinion p Minions.armadillo
        vid <- villainIdNow
        resolve [GiveStatus (toRef vid) ToughStatus, VillainMessage vid (VillainStunned GameRef)]
        marker <- prepareSurge
        _ <- revealNow p Treacheries.toughAndTumble
        settle
        damageOf p `shouldReturn` 2
        wasDealt p marker `shouldReturn` False

    it "lets the revealing player order multiple eligible minions" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        first <- addMinion p Minions.hydraSoldier
        second <- addMinion p Minions.hydraSoldier
        resolve [GiveStatus (toRef first) ToughStatus, GiveStatus (toRef second) ToughStatus]
        _ <- revealNow p Treacheries.toughAndTumble
        offers (isTargetChoice first) `shouldReturn` True
        offers (isTargetChoice second) `shouldReturn` True
        choose "the second minion activates first" (isTargetChoice second)
        settle
        damageOf p `shouldReturn` 4
        length <$> dealtTo p `shouldReturn` 0

    it "surges when no enemy is tough" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        marker <- prepareSurge
        _ <- revealNow p Treacheries.toughAndTumble
        settle
        wasDealt p marker `shouldReturn` True

    it "surges when all eligible attacks were replaced by stun" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        asNova p
        mid <- addMinion p Minions.armadillo
        resolve [MinionMessage mid (MinionStunned GameRef)]
        marker <- prepareSurge
        _ <- revealNow p Treacheries.toughAndTumble
        settle
        wasDealt p marker `shouldReturn` True
        damageOf p `shouldReturn` 0
        toughCount mid `shouldReturn` 1

    it "surges when all eligible schemes were replaced by confusion" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.armadillo
        resolve [MinionMessage mid (MinionConfused GameRef)]
        marker <- prepareSurge
        _ <- revealNow p Treacheries.toughAndTumble
        settle
        wasDealt p marker `shouldReturn` True
        toughCount mid `shouldReturn` 1

  describe "Tough It Out" do
    it "adds another tough to Armadillo and gives the villain tough without surge" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.armadillo
        marker <- prepareSurge
        _ <- revealNow p Treacheries.toughItOut
        settle
        toughCount mid `shouldReturn` 2
        Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True
        wasDealt p marker `shouldReturn` False

    it "surges when the villain already has tough, while still stacking Armadillo's tough" do
      tg <- testGame ["nova"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.armadillo
        vid <- villainIdNow
        resolve [GiveStatus (toRef vid) ToughStatus]
        marker <- prepareSurge
        _ <- revealNow p Treacheries.toughItOut
        settle
        toughCount mid `shouldReturn` 2
        wasDealt p marker `shouldReturn` True

    it "surges when Armadillo is absent, whether or not the villain can gain tough" do
      for_ [False, True] \alreadyTough -> do
        tg <- testGame ["nova"]
        let p = player1 tg
        inGame tg do
          vid <- villainIdNow
          when alreadyTough $ resolve [GiveStatus (toRef vid) ToughStatus]
          marker <- prepareSurge
          _ <- revealNow p Treacheries.toughItOut
          settle
          Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True
          wasDealt p marker `shouldReturn` True
