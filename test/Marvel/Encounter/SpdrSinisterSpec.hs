module Marvel.Encounter.SpdrSinisterSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types qualified as Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Environment.Cards qualified as Environments
import Marvel.Environment.Types qualified as Environment
import Marvel.Keyword qualified as K
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as Scheme
import Marvel.Window qualified as W
import TestImport

settle :: GameAppT ()
settle = do
  pressWhenRevealed
  forced <- offers isForced
  if forced then choose "resolve forced ability" isForced >> settle
    else pass >> whenM (offers isForced) settle
 where
  isForced = \case
    UseAbility a -> abilityType a `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse]
    _ -> False

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

minionAttrs :: MinionId -> GameAppT (Attrs Minion)
minionAttrs mid = getsGame (maybe (error "missing minion") toAttrs . HashMap.lookup mid . gameMinions)

resolveSinisterBoost :: CardDef -> GameAppT ()
resolveSinisterBoost def = do
  card <- encounterCard def
  villain <- villainIdNow
  resolve [RevealBoostCard card (EnemyVillainId villain)]

spec :: Spec
spec = describe "SP//dr Iron Spider's Sinister Six" do
  describe "Grand Larceny" do
    it "has four fixed threat and crisis with two players" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        card <- revealNow (player1 tg) Schemes.grandLarceny
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 4
        crisis <- getsGame $ maybe False (Scheme.sideSchemeCrisis . toAttrs) . HashMap.lookup (sideSchemeOf card) . gameSideSchemes
        crisis `shouldBe` True

    it "blocks all threat removal while a Criminal minion is in play even engaged with someone else" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
      inGame tg do
        criminal <- addMinion (player2 tg) Minions.ironSpider
        card <- revealNow p Schemes.grandLarceny
        settle
        resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThreatRemoved GameRef 2)]
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 4
        resolve [RemoveFromPlay $ MinionRef criminal]
        resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThreatRemoved GameRef 2)]
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 2

    it "does not block threat removal for a non-Criminal minion" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.hydraMercenary
        card <- revealNow p Schemes.grandLarceny
        settle
        resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThreatRemoved GameRef 2)]
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 2

  describe "Bombshell" do
    it "divides attack damage evenly among all controlled characters" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        fury <- putIntoPlay p "01084"
        bird <- putIntoPlay p "01083"
        mid <- addMinion p Minions.bombshellSinister
        runNow [MinionMessage mid (MinionAttacks p)]
        settle
        damageOf p `shouldReturn` 1
        allyDamage <$> allyIn "01084" `shouldReturn` 1
        allyDamage <$> allyIn "01083" `shouldReturn` 1
        getAllyCount p `shouldReturn` 2

    it "subtracts a defending hero's DEF before dividing damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "01084"
        mid <- addMinion p Minions.bombshellSinister
        runNow [MinionMessage mid (MinionAttacks p)]
        choose "Spider-Man defends" isDefend
        settle
        damageOf p `shouldReturn` 0
        allyDamage <$> allyIn "01084" `shouldReturn` 0

    it "applies a defending ally's personal reduction only to its allocated share" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        victor <- putIntoPlay p "26015"
        mid <- addMinion p Minions.bombshellSinister
        runNow [MinionMessage mid (MinionAttacks p)]
        choose "Victor Mancha defends" \case AllyDefend aid _ -> aid == allyOf victor; _ -> False
        choose "give the extra point to the hero" (isTargetChoice p)
        settle
        damageOf p `shouldReturn` 2
        allyDamage <$> allyIn "26015" `shouldReturn` 0

    it "applies overkill to each defeated ally's own share rather than the full attack" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        fury <- putIntoPlay p "01084"
        bird <- putIntoPlay p "01083"
        resolve [AllyMessage (allyOf fury) $ AllyDamaged GameRef (toDamage 2 FromAbility), AllyMessage (allyOf bird) $ AllyDamaged GameRef (toDamage 2 FromAbility)]
        mid <- addMinion p Minions.bombshellSinister
        _ <- revealNow p Attachments.neurologicalImplants
        settle
        runNow [MinionMessage mid (MinionAttacks p)]
        resolve [MinionMessage mid $ MinionAttackGains K.Overkill]
        choose "take the divided attack" isNoDefender
        choose "first extra point to Nick Fury" (isTargetChoice $ allyOf fury)
        choose "second extra point to Mockingbird" (isTargetChoice $ allyOf bird)
        settle
        getAllyCount p `shouldReturn` 0
        damageOf p `shouldReturn` 3

    it "boost deals one indirect damage to each player and exhausts damaged characters" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        resolveSinisterBoost Minions.bombshellSinister
        damageOf p `shouldReturn` 1
        damageOf other `shouldReturn` 1
        isExhaustedNow p `shouldReturn` True
        isExhaustedNow other `shouldReturn` True

    it "boost does not exhaust a character whose tough prevents the damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        resolveSinisterBoost Minions.bombshellSinister
        damageOf p `shouldReturn` 0
        isExhaustedNow p `shouldReturn` False
        playerIdentityTough <$> playerAttrs p `shouldReturn` False

  describe "Electro" do
    it "attaches a chosen printed-energy hand card on engagement and gains HP per icon" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        energy <- putInHand p "01088"
        mental <- putInHand p "01089"
        card <- revealNow p Minions.electroSinister
        choose "resolve Electro" (isAbilityOf $ minionOf card)
        offers (isTargetChoice mental) `shouldReturn` False
        choose "attach Energy" (isTargetChoice energy)
        settle
        attrs <- minionAttrs $ minionOf card
        minionHeldCards attrs `shouldBe` [energy]
        Minion.getModifiedHitPoints attrs `shouldReturn` 5
        handOf p >>= (`shouldSatisfy` notElem energy)

    it "does not treat a printed wild as a printed energy resource" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        wild <- putInHand p "01044"
        mid <- addMinion p Minions.electroSinister
        minionHeldCards <$> minionAttrs mid `shouldReturn` []
        handOf p >>= (`shouldSatisfy` elem wild)

    it "takes another eligible card after scheming and discards held cards to their owner on removal" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        mid <- addMinion p Minions.electroSinister
        energy <- putInHand p "01088"
        runNow [MinionMessage mid MinionSchemes]
        choose "resolve Electro after scheming" (isAbilityOf mid)
        choose "attach Energy" (isTargetChoice energy)
        settle
        minionHeldCards <$> minionAttrs mid `shouldReturn` [energy]
        resolve [RemoveFromPlay $ MinionRef mid]
        discardOf p >>= (`shouldSatisfy` elem energy)

    it "takes a card after attacking and returns it when defeated" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        mid <- addMinion p Minions.electroSinister
        energy <- putInHand p "01088"
        runNow [MinionMessage mid (MinionAttacks p)]
        settle
        choose "attach Energy" (isTargetChoice energy)
        settle
        minionHeldCards <$> minionAttrs mid `shouldReturn` [energy]
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 5 $ FromPlayerAttack p)]
        discardOf p >>= (`shouldSatisfy` elem energy)

    it "cleans held cards when removed from the game as well as when discarded" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        energy <- putInHand p "01088"
        mid <- addMinion p Minions.electroSinister
        resolve [RemoveFromGame $ MinionRef mid]
        discardOf p >>= (`shouldSatisfy` elem energy)

  describe "Hobgoblin" do
    it "replaces the attack with discards equal to ATK and sums boost icons as indirect damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        mid <- addMinion p Minions.hobgoblinSinister
        _ <- stackEncounterDeck [Minions.ironSpider, Minions.hydraMercenary]
        before <- encounterDeckSize
        runNow [MinionMessage mid (MinionAttacks p)]
        choose "replace Hobgoblin's attack" (isAbilityOf mid)
        settle
        damageOf p `shouldReturn` 4
        encounterDeckSize `shouldReturn` before - 2
        isExhaustedNow p `shouldReturn` False

    it "uses modified ATK rather than printed ATK to determine its discard count" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        mid <- addMinion p Minions.hobgoblinSinister
        _ <- revealNow p Attachments.neurologicalImplants
        settle
        _ <- stackEncounterDeck $ replicate 4 Minions.bombshellSinister
        before <- encounterDeckSize
        runNow [MinionMessage mid (MinionAttacks p)]
        choose "replace modified Hobgoblin attack" (isAbilityOf mid)
        settle
        encounterDeckSize `shouldReturn` before - 4
        damageOf p `shouldReturn` 0

    it "does not replace an attack that a stun card already cancels" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        mid <- addMinion p Minions.hobgoblinSinister
        before <- encounterDeckSize
        resolve [MinionMessage mid (MinionStunned GameRef), MinionMessage mid (MinionAttacks p)]
        damageOf p `shouldReturn` 0
        encounterDeckSize `shouldReturn` before

  describe "Iron Spider" do
    it "enters tough and has guard, patrol, retaliate one and attack overkill" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        mid <- addMinion p Minions.ironSpider
        attrs <- minionAttrs mid
        minionTough attrs `shouldBe` True
        keywords <- Minion.getModifiedKeywords attrs
        keywords `shouldSatisfy` (\ks -> all (`elem` ks) [K.Guard, K.Patrol, K.Retaliate 1, K.Overkill])
        main <- mainSchemeIdNow
        offers (isHeroThwart p) `shouldReturn` False
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 1 $ FromPlayerAttack p)]
        minionDamageOf mid `shouldReturn` Just 0
        minionTough <$> minionAttrs mid `shouldReturn` False

    it "deals overkill past a defending ally" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        ally <- putIntoPlay p "01083"
        resolve [AllyMessage (allyOf ally) $ AllyDamaged GameRef (toDamage 2 FromAbility)]
        mid <- addMinion p Minions.ironSpider
        runNow [MinionMessage mid (MinionAttacks p)]
        choose "ally defends" \case AllyDefend aid _ -> aid == allyOf ally; _ -> False
        settle
        damageOf p `shouldReturn` 1

  describe "Sandman" do
    it "discards seven encounter cards after taking attack damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.sandmanSinister
        before <- encounterDeckSize
        runNow [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 1 $ FromPlayerAttack p)]
        settle
        encounterDeckSize `shouldReturn` before - 7
        minionDamageOf mid `shouldReturn` Just 1

    it "does not respond to nonattack or tough-prevented damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.sandmanSinister
        before <- encounterDeckSize
        resolve [MinionMessage mid $ MinionDamaged GameRef (toDamage 1 FromAbility)]
        encounterDeckSize `shouldReturn` before
        resolve [MinionMessage mid MinionBecomeTough, MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 1 $ FromPlayerAttack p)]
        encounterDeckSize `shouldReturn` before

    it "also discards seven cards as a boost" do
      tg <- testGame ["spider-man"]
      inGame tg do
        before <- encounterDeckSize
        resolveSinisterBoost Minions.sandmanSinister
        encounterDeckSize `shouldReturn` before - 7

  describe "Spot" do
    it "shuffles into the encounter deck when defeated with exactly lethal damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.spot
        before <- encounterDeckSize
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 4 $ FromPlayerAttack p)]
        selectList (MinionIs Minions.spot) `shouldReturn` []
        encounterDeckSize `shouldReturn` before + 1
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "31036")

    it "still opens after-defeat responses when its When Defeated ability shuffles it" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        mid <- addMinion p Minions.spot
        placeMainThreat 2
        before <- mainSchemeThreatNow
        chase <- putInHand p "01052"
        runNow [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 4 $ FromPlayerAttack p)]
        -- runNow acknowledges When Defeated; its effect must already have shuffled Spot.
        selectList (MinionIs Minions.spot) `shouldReturn` []
        map toCardCode <$> scenarioEncounterDeckNow >>= (`shouldSatisfy` elem "31036")
        playWith chase []
        chooseMainScheme
        settle
        mainSchemeThreatNow `shouldReturn` before - 2

    it "is discarded when defeated with excess damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.spot
        before <- encounterDeckSize
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 5 $ FromPlayerAttack p)]
        encounterDeckSize `shouldReturn` before
        encounterDiscardCodes >>= (`shouldSatisfy` elem "31036")

    it "does not shuffle if Biomechanical Upgrades prevents his defeat" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.spot
        _ <- revealNow p Attachments.biomechanicalUpgrades
        settle
        before <- encounterDeckSize
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 4 $ FromPlayerAttack p)]
        selectList (MinionIs Minions.spot) `shouldReturn` [mid]
        encounterDeckSize `shouldReturn` before
        minionDamageOf mid `shouldReturn` Just 0

    it "enters engaged with the attacked player as a boost" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        resolveSinisterBoost Minions.spot
        mids <- selectList (MinionIs Minions.spot)
        length mids `shouldBe` 1
        minionEngagedIdentity <$> minionAttrs (head mids) `shouldReturn` p

  describe "Surge in Crime" do
    it "surges itself and makes Criminal minions surge" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [Minions.hydraMercenary, Minions.hydraMercenary]
        before <- length <$> dealtTo p
        _ <- revealNow p Environments.surgeInCrime
        settle
        length <$> dealtTo p `shouldReturn` before + 1
        _ <- revealNow p Minions.ironSpider
        settle
        length <$> dealtTo p `shouldReturn` before + 2

    it "does not grant surge to non-Criminal minions" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Environments.surgeInCrime
        settle
        before <- length <$> dealtTo p
        _ <- revealNow p Minions.hydraMercenary
        settle
        length <$> dealtTo p `shouldReturn` before

    it "allows a hero to spend two resources to discard it only without Criminal minions" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        card <- revealNow p Environments.surgeInCrime
        settle
        let eid = EnvironmentId $ unCardId $ ecCardId card
        criminal <- addMinion p Minions.ironSpider
        offers (isAbilityOf eid) `shouldReturn` False
        resolve [RemoveFromPlay $ MinionRef criminal]
        genius <- putInHand p "01089"
        choose "remove Surge in Crime" (isAbilityOf eid)
        choose "pay two resources" (isPayWith genius)
        whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
        settle
        selectList (EnvironmentIs Environments.surgeInCrime) `shouldReturn` []
        encounterDiscardCodes >>= (`shouldSatisfy` elem "31037")

allyOf :: PlayerCard -> AllyId
allyOf = allyIdOf

getAllyCount :: IdentityId -> GameAppT Int
getAllyCount p = length <$> selectList (AllyControlledBy $ IdentityWithId p)
