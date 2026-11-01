module Marvel.Hero.DeadpoolSignatureSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Keyword (Keyword (Stalwart))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Resource (resourcesFor)
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Status
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W
import TestImport

newtype SignatureModifiers = SignatureModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage SignatureModifiers where
  runMessage msg (SignatureModifiers attrs) = SignatureModifiers <$> runMessage msg attrs

modifyFor :: EntityMatcher -> [Modifier] -> GameAppT ()
modifyFor matcher modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (SignatureModifiers EffectAttrs
    { effectId = eid, effectCardCode = "44003", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = modifiers, effectEnds = Nothing })

asHero :: IdentityId -> GameAppT ()
asHero you = clearHand you >> changeForm you >> pass

fireAbility :: PlayerCard -> Choice -> Bool
fireAbility card = \case
  UseAbility ab -> abilitySource ab == CardIdRef (pcCardId card) && abilityIndex ab == 1
  _ -> False

identityAbility :: IdentityId -> Choice -> Bool
identityAbility you = \case
  UseAbility ab -> abilitySource ab == IdentityRef you && abilityIndex ab == 1
  _ -> False

takeDamageChoice :: Natural -> GameAppT ()
takeDamageChoice n = choose "choose damage to take" (isLabel $ countLabel "takeDamage" n)

playYooHoo :: IdentityId -> Natural -> GameAppT ()
playYooHoo you n = do
  event <- putInHand you "44006"
  payment <- putInHand you "01083"
  playWith event [payment]
  takeDamageChoice n

setMainThreat :: Natural -> GameAppT ()
setMainThreat n = do
  scheme <- mainSchemeIdNow
  resolve [MainSchemeMessage scheme (MainSchemeThwarted GameRef 99), MainSchemeMessage scheme (MainSchemePlaceThreat n)]

spec :: Spec
spec = describe "Deadpool signature" do
  describe "Exhausting Personality" do
    it "pays an acceleration token before stunning and confusing the villain" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        event <- putInHand you "44003"
        playWith event []
        offers (isLabel "$label.deadpoolExhaustAndDraw") `shouldReturn` False
        choose "add an acceleration token" (isLabel "$label.deadpoolStunAndConfuse")
        pass
        getMainSchemeTokenCount `shouldReturn` 1
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` True
        Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` True

    it "exhausts another player's identity and draws cards for that player based on tokens" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        asHero you
        scheme <- mainSchemeIdNow
        resolve [PlaceAccelerationToken scheme, PlaceAccelerationToken scheme]
        before <- length <$> handOf other
        event <- putInHand you "44003"
        playWith event []
        choose "exhaust an identity" (isLabel "$label.deadpoolExhaustAndDraw")
        choose "the other player's identity" (isTargetChoice other)
        pass
        isExhaustedPlayer other `shouldReturn` True
        isExhaustedPlayer you `shouldReturn` False
        (length <$> handOf other) `shouldReturn` (before + 2)
        handOf you `shouldReturn` []
        getMainSchemeTokenCount `shouldReturn` 2

    it "does not count acceleration icons as tokens for the draw option" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        _ <- putIntoPlay you "44013"
        event <- putInHand you "44003"
        playWith event []
        offers (isLabel "$label.deadpoolExhaustAndDraw") `shouldReturn` False

    it "does not offer to exhaust an already exhausted identity" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        asHero you
        scheme <- mainSchemeIdNow
        resolve [PlaceAccelerationToken scheme, IdentityMessage other ExhaustedIdentity]
        event <- putInHand you "44003"
        playWith event []
        choose "draw by exhausting" (isLabel "$label.deadpoolExhaustAndDraw")
        offers (isTargetChoice other) `shouldReturn` False
        choose "exhaust Deadpool" (isTargetChoice you)
        pass
        isExhaustedPlayer you `shouldReturn` True
        length <$> handOf you `shouldReturn` 1

    it "cannot use costs alone when neither option can change the game" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        villain <- villainIdNow
        resolve [GiveStatus (VillainRef villain) StunnedStatus, GiveStatus (VillainRef villain) ConfusedStatus]
        event <- putInHand you "44003"
        offers (isPlay event) `shouldReturn` False
        getMainSchemeTokenCount `shouldReturn` 0

    it "omits the status option against a stalwart villain while allowing a meaningful draw" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        villain <- villainIdNow
        modifyFor (EnemyEntity $ EnemyWithId $ EnemyVillainId villain) [KeywordModifier Stalwart]
        scheme <- mainSchemeIdNow
        resolve [PlaceAccelerationToken scheme]
        event <- putInHand you "44003"
        playWith event []
        offers (isLabel "$label.deadpoolStunAndConfuse") `shouldReturn` False
        offers (isLabel "$label.deadpoolExhaustAndDraw") `shouldReturn` True

  describe "Maximum Effort" do
    it "takes the chosen damage as a cost and attacks for the amount paid" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        event <- putInHand you "44004"
        playWith event []
        takeDamageChoice 4
        damageOf you `shouldReturn` 4
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4

    it "can take all remaining hit points and still attack after Deadpool becomes Wade" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        setDamage you 6
        event <- putInHand you "44004"
        playWith event []
        takeDamageChoice 3
        choose "replace Deadpool's defeat" (identityAbility you)
        chooseVillain
        pass
        damageOf you `shouldReturn` 8
        toCardCode <$> getPlayer you `shouldReturn` "44001b"
        villainDamageNow `shouldReturn` 3
        getMainSchemeTokenCount `shouldReturn` 1

    it "still takes the cost while stunned, then removes stun without attacking" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        resolve [GiveStatus (IdentityRef you) StunnedStatus]
        event <- putInHand you "44004"
        playWith event []
        takeDamageChoice 2
        pass
        damageOf you `shouldReturn` 2
        villainDamageNow `shouldReturn` 0
        isStunned you `shouldReturn` False

    it "cannot be paid through tough and produces no attack if an interrupt prevents part of the cost" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        event <- putInHand you "44004"
        resolve [IdentityMessage you IdentityBecomeTough]
        offers (isPlay event) `shouldReturn` False
        resolve [RemoveStatus (IdentityRef you) ToughStatus]
        stance <- putIntoPlay you "08032"
        playWith event []
        takeDamageChoice 4
        choose "prevent three damage" (isAbilityOfCard stance)
        pass
        damageOf you `shouldReturn` 1
        villainDamageNow `shouldReturn` 0

  describe "Yoo-Hoo!" do
    it "pays its resource and damage costs before removing an equal amount of threat" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        setMainThreat 5
        playYooHoo you 3
        damageOf you `shouldReturn` 3
        chooseMainScheme
        pass
        mainSchemeThreat `shouldReturn` 2

    it "continues its thwart after paying the last hit points and changing to alter-ego" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        setMainThreat 5
        setDamage you 6
        event <- putInHand you "44006"
        payment <- putInHand you "01083"
        playWith event [payment]
        takeDamageChoice 3
        choose "replace defeat" (identityAbility you)
        chooseMainScheme
        pass
        toCardCode <$> getPlayer you `shouldReturn` "44001b"
        mainSchemeThreat `shouldReturn` 2

    it "takes the cost while confused but removes no threat" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        setMainThreat 5
        resolve [GiveStatus (IdentityRef you) ConfusedStatus]
        playYooHoo you 2
        pass
        damageOf you `shouldReturn` 2
        mainSchemeThreat `shouldReturn` 5
        isConfused you `shouldReturn` False

    it "does not remove threat if some damage in its take-damage cost is prevented" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        setMainThreat 5
        stance <- putIntoPlay you "08032"
        playYooHoo you 4
        choose "prevent the cost" (isAbilityOfCard stance)
        pass
        damageOf you `shouldReturn` 1
        mainSchemeThreat `shouldReturn` 5

  describe "Metaknowledge" do
    it "cancels a revealed minion entirely and takes its printed boost damage" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        event <- putInHand you "44005"
        payment <- putInHand you "01083"
        card <- revealNow you Minions.shocker
        playWith event [payment]
        pass
        getsGame (HashMap.member (minionOf card) . gameMinions) `shouldReturn` False
        damageOf you `shouldReturn` 2
        encounterDiscardNow >>= (`shouldSatisfy` elem card)

    it "counts stars as icons without adding amplify icons" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        _ <- putIntoPlay you "44014"
        event <- putInHand you "44005"
        payment <- putInHand you "01083"
        card <- revealNow you Treacheries.sonicBoom
        playWith event [payment]
        pass
        damageOf you `shouldReturn` 1
        isExhaustedPlayer you `shouldReturn` False
        encounterDiscardNow >>= (`shouldSatisfy` elem card)

    it "cancels a side scheme before it enters play" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        event <- putInHand you "44005"
        payment <- putInHand you "01083"
        card <- revealNow you Schemes.breakinAndTakin
        playWith event [payment]
        pass
        getsGame (HashMap.member (sideSchemeOf card) . gameSideSchemes) `shouldReturn` False
        damageOf you `shouldReturn` 2
        encounterDiscardNow >>= (`shouldSatisfy` elem card)

    it "cancels surge as well as the revealed card's printed effects" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        event <- putInHand you "44005"
        payment <- putInHand you "01083"
        before <- encounterDeckSize
        card <- revealNow you Minions.weaponsRunner
        playWith event [payment]
        pass
        encounterDeckSize `shouldReturn` before
        getsGame (HashMap.member (minionOf card) . gameMinions) `shouldReturn` False
        damageOf you `shouldReturn` 1

    it "can cancel a reveal from set-aside and a reveal for another player" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        asHero you
        event <- putInHand you "44005"
        payment <- putInHand you "01083"
        card <- encounterCard Minions.shocker
        runNow [RevealEncounterCardFrom W.RevealedFromSetAside other card]
        playWith event [payment]
        pass
        getsGame (HashMap.member (minionOf card) . gameMinions) `shouldReturn` False
        damageOf you `shouldReturn` 2
        damageOf other `shouldReturn` 0

    it "can prevent its damage after cancellation and cannot be played as Wade" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        event <- putInHand you "44005"
        payment <- putInHand you "01083"
        _ <- revealNow you Treacheries.advance
        offers (isPlay event) `shouldReturn` False
        pass
        changeForm you
        resolve [IdentityMessage you IdentityBecomeTough]
        card <- revealNow you Minions.shocker
        playWith event [payment]
        pass
        damageOf you `shouldReturn` 0
        getsGame (HashMap.member (minionOf card) . gameMinions) `shouldReturn` False

  describe "This Card is Fire" do
    it "attacks for sustained damage and can be paid with Montage's generated resources" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        setDamage you 5
        scheme <- mainSchemeIdNow
        resolve [PlaceAccelerationToken scheme, PlaceAccelerationToken scheme]
        event <- putInHand you "44012"
        montage <- putInHand you "44007"
        resourcesFor montage (Just event) `shouldReturn` [Wild, Wild, Wild]
        printedResourcesFor montage `shouldReturn` [Wild]
        playWith event [montage]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5
        damageOf you `shouldReturn` 5

    it "triggers separately from each physical copy in hand after the owner's turn ends" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        first <- putInHand you "44012"
        second <- putInHand you "44012"
        runNow [IdentityMessage you EndedTurn]
        offers (fireAbility first) `shouldReturn` True
        offers (fireAbility second) `shouldReturn` True
        choose "the first hot card" (fireAbility first)
        damageOf you `shouldReturn` 1
        offers (fireAbility first) `shouldReturn` False
        choose "the second hot card" (fireAbility second)
        damageOf you `shouldReturn` 2
        handOf you >>= (`shouldSatisfy` elem first)
        handOf you >>= (`shouldSatisfy` elem second)

    it "also burns Wade, but does not trigger at the end of another player's turn" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        clearHand you
        card <- putInHand you "44012"
        runNow [CheckWindows [W.Window W.After $ W.TurnEnded other]]
        offers (fireAbility card) `shouldReturn` False
        runNow [IdentityMessage you EndedTurn]
        choose "the card burns Wade" (fireAbility card)
        damageOf you `shouldReturn` 1
        toCardCode <$> getPlayer you `shouldReturn` "44001b"

    it "does not trigger before the turn ends and damage from it can be prevented by tough" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        card <- putInHand you "44012"
        resolve [IdentityMessage you IdentityBecomeTough]
        runNow [CheckWindows [W.Window W.When $ W.TurnEnded you]]
        offers (fireAbility card) `shouldReturn` False
        runNow [IdentityMessage you EndedTurn]
        choose "burn the tough" (fireAbility card)
        damageOf you `shouldReturn` 0
        playerIdentityTough <$> playerAttrs you `shouldReturn` False

    it "does not burn from a played copy in the discard pile" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        setDamage you 3
        card <- putInHand you "44012"
        payment <- putInHand you "44025"
        extra <- putInHand you "01088"
        playWith card [payment, extra]
        chooseVillain
        pass
        runNow [IdentityMessage you EndedTurn]
        offers (fireAbility card) `shouldReturn` False
        damageOf you `shouldReturn` 3

    it "cannot deal zero attack damage as its only effect, but can clear stun at zero damage" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        card <- putInHand you "44012"
        payment <- putInHand you "44025"
        offers (isPlay card) `shouldReturn` False
        resolve [GiveStatus (IdentityRef you) StunnedStatus]
        offers (isPlay card) `shouldReturn` True
        playWith card [payment]
        pass
        isStunned you `shouldReturn` False
        villainDamageNow `shouldReturn` 0
