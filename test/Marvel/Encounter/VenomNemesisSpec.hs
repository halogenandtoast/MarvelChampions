module Marvel.Encounter.VenomNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Matchers (SchemeMatcher (SchemeYouCanThwart))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Query (select)
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

venom :: IO TestGame
venom = testGame ["venom"]

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

sideOf :: IdentityId -> GameAppT Side
sideOf p = (.playerIdentitySide) <$> playerAttrs p

-- | The Enraged Symbiotes in play: who each is engaged with.
symbiotesEngaged :: GameAppT [IdentityId]
symbiotesEngaged =
  getsGame
    ( map minionEngagedIdentity
        . filter ((== "20025") . toCardCode . minionCardDef)
        . map toAttrs
        . HashMap.elems
        . gameMinions
    )

exhaustFlash, putSymbiote :: Choice -> Bool
exhaustFlash = isLabel "$label.exhaustFlashThompsonTakeDamage"
putSymbiote = isLabel "$label.putEnragedSymbioteIntoPlayOrSurge"

-- | Whether the encounter card stacked before the reveal was dealt to the player.
dealtNext :: IdentityId -> EncounterCard -> GameAppT Bool
dealtNext p next = elem (ecCardId next) . map ecCardId <$> dealtTo p

spec :: Spec
spec = describe "Venom's obligation and nemesis set" do
  describe "Struggle for Control (20023)" do
    it "is given to the Flash Thompson player" do
      tg <- testGame ["spider-man", "venom"]
      let p1 = player1 tg
          p2 = player2 tg
      inGame tg do
        _ <- revealNow p1 Obligations.struggleForControl
        fmap fst <$> asked `shouldReturn` Just p2

    it "exhausting Flash Thompson and taking 2 damage discards it" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.struggleForControl
        offers putSymbiote `shouldReturn` True
        choose "exhaust Flash" exhaustFlash
        pass
        isExhaustedNow p `shouldReturn` True
        damageOf p `shouldReturn` 2
        symbiotesEngaged `shouldReturn` []
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "20023")

    it "a hero may flip to alter-ego to exhaust Flash Thompson" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- revealNow p Obligations.struggleForControl
        choose "flip to alter-ego" (isLabel "$label.changeToB")
        sideOf p `shouldReturn` B
        choose "exhaust Flash" exhaustFlash
        pass
        damageOf p `shouldReturn` 2
        obligationsInPlay `shouldReturn` 0

    it "puts a set-aside Enraged Symbiote into play engaged with the first player, and does not surge" do
      tg <- testGame ["spider-man", "venom"]
      let p1 = player1 tg
          p2 = player2 tg
      inGame tg do
        [next] <- stackEncounterDeck [Treacheries.advance]
        _ <- revealNow p1 Obligations.struggleForControl
        choose "the symbiote" putSymbiote
        pass
        symbiotesEngaged `shouldReturn` [p1]
        isExhaustedNow p2 `shouldReturn` False
        damageOf p2 `shouldReturn` 0
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "20023")
        dealtNext p2 next `shouldReturn` False

    it "a hero who stays a hero cannot exhaust Flash Thompson: the symbiote comes" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- revealNow p Obligations.struggleForControl
        choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
        offers exhaustFlash `shouldReturn` False
        pass
        sideOf p `shouldReturn` A
        damageOf p `shouldReturn` 0
        symbiotesEngaged `shouldReturn` [p]

    it "an exhausted Flash Thompson cannot be exhausted for it" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow p Obligations.struggleForControl
        offers exhaustFlash `shouldReturn` False
        pass
        damageOf p `shouldReturn` 0
        symbiotesEngaged `shouldReturn` [p]

    it "with a tough status card the 2 damage cannot be paid" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        _ <- revealNow p Obligations.struggleForControl
        offers exhaustFlash `shouldReturn` False
        pass
        isExhaustedNow p `shouldReturn` False
        symbiotesEngaged `shouldReturn` [p]

    it "with no Enraged Symbiote set aside, it gains surge" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Treacheries.shadowOfThePast
        pass
        length <$> symbiotesEngaged `shouldReturn` 4
        resolve [IdentityMessage p ExhaustedIdentity]
        [next] <- stackEncounterDeck [Treacheries.advance]
        _ <- revealNow p Obligations.struggleForControl
        pass
        length <$> symbiotesEngaged `shouldReturn` 4
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "20023")
        dealtNext p next `shouldReturn` True

  describe "Shadow of the Past" do
    it "puts every set-aside Enraged Symbiote into play: each is Venom's nemesis minion" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Treacheries.shadowOfThePast
        pass
        symbiotesEngaged `shouldReturn` [p, p, p, p]
        getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes) `shouldReturn` ["20024"]

  describe "Klyntar Frenzy (20024)" do
    it "has 2 threat per player and 1 hazard" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
      inGame tg do
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.klyntarFrenzy
        pass
        sideSchemeThreat scheme `shouldReturn` 4
        getHazardCount `shouldReturn` 1

    it "threat cannot be removed from it while a Symbiote enemy is in play" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.klyntarFrenzy
        pass
        sideSchemeThreat scheme `shouldReturn` 2
        symbiote <- addMinion p Minions.enragedSymbiote
        resolve [SideSchemeMessage scheme (SideSchemeThwarted GameRef 1)]
        sideSchemeThreat scheme `shouldReturn` 2
        resolve [MinionMessage symbiote MinionDefeated]
        resolve [SideSchemeMessage scheme (SideSchemeThwarted GameRef 1)]
        sideSchemeThreat scheme `shouldReturn` 1

    it "a non-Symbiote enemy does not stop threat being removed" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.klyntarFrenzy
        pass
        _ <- addMinion p Minions.hydraMercenary
        resolve [SideSchemeMessage scheme (SideSchemeThwarted GameRef 1)]
        sideSchemeThreat scheme `shouldReturn` 1

  describe "Enraged Symbiote (20025)" do
    it "has guard: the villain cannot be attacked while it is engaged" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        symbiote <- addMinion p Minions.enragedSymbiote
        vid <- villainIdNow
        clearHand p
        choose "attack" (isHeroAttack p)
        pass
        offers (isTargetChoice symbiote) `shouldReturn` True
        offers (isTargetChoice vid) `shouldReturn` False

    it "has patrol: the engaged player cannot thwart the main scheme, the other can" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        ms <- mainSchemeId
        resolve [MainSchemeMessage ms (MainSchemePlaceThreat 2)]
        _ <- addMinion p Minions.enragedSymbiote
        mine <- asYou p (select SchemeYouCanThwart)
        liftIO $ mine `shouldNotSatisfy` HashSet.member (SchemeMainSchemeId ms)
        theirs <- asYou other (select SchemeYouCanThwart)
        liftIO $ theirs `shouldSatisfy` HashSet.member (SchemeMainSchemeId ms)

    it "attacks for 2" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        symbiote <- addMinion p Minions.enragedSymbiote
        _ <- stackEncounterDeck [Treacheries.hailHydra]
        runNow [MinionMessage symbiote (MinionAttacks p)]
        pass
        damageOf p `shouldReturn` 2

    it "as a boost, enters play engaged with the player attacked" do
      tg <- testGame ["venom", "spider-man"]
      let p2 = player2 tg
      inGame tg do
        _ <- stackEncounterDeck [Minions.enragedSymbiote]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p2)]
        pass
        symbiotesEngaged `shouldReturn` [p2]
