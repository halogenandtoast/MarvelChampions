module Marvel.Encounter.MilesMoralesNemesisSpec (spec) where

import Data.Aeson (Result (..), Value (Object), fromJSON, toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Runner qualified as Minion
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

attachmentTarget :: CardCode -> GameAppT (Maybe EnemyId)
attachmentTarget code = getsGame $ headMay . mapMaybe (Attachment.attachmentEnemy . toAttrs)
  . filter ((== code) . toCardCode . getCardDef) . HashMap.elems . gameAttachments

bombDealtTo :: IdentityId -> GameAppT Bool
bombDealtTo p = any ((== "01109") . toCardCode) <$> dealtTo p

spec :: Spec
spec = describe "Miles Morales nemesis" do
  describe "Tracking Prey" do
    for_ [False, True] \hero ->
      it ("starts with fixed four threat and " <> if hero then "no token in hero form" else "an acceleration token in alter-ego") do
        tg <- testGame ["spider-man-miles-morales", "spider-man"]
        let p = player1 tg
        inGame tg do
          when hero $ changeForm p >> pass
          before <- getAccelerationCount
          scheme <- revealNow p Schemes.trackingPrey
          pass
          let sid = SideSchemeId $ unCardId $ ecCardId scheme
          sideSchemeThreat sid `shouldReturn` 4
          attrs <- getsGame (toAttrs . fromMaybe (error "missing Tracking Prey") . HashMap.lookup sid . gameSideSchemes)
          SideScheme.sideSchemeAccelerationTokens attrs `shouldBe` (if hero then 0 else 1)
          getAccelerationCount `shouldReturn` (before + if hero then 1 else 2)
          runNow [RemoveFromPlay (toRef sid)]
          pass
          getAccelerationCount `shouldReturn` before

    it "loads an older side-scheme save with no acceleration tokens" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p Schemes.trackingPrey
        pass
        let sid = SideSchemeId $ unCardId $ ecCardId card
        attrs <- getsGame (toAttrs . fromMaybe (error "missing scheme") . HashMap.lookup sid . gameSideSchemes)
        case toJSON attrs of
          Object object -> case fromJSON (Object $ KeyMap.delete "sideSchemeAccelerationTokens" object) of
            Success (old :: Attrs SideScheme.SideScheme) -> SideScheme.sideSchemeAccelerationTokens old `shouldBe` 0
            Error err -> error err
          _ -> error "expected side scheme object"

  describe "Prowler" do
    for_ [False, True] \hero ->
      it ("is stalwart and " <> if hero then "does not gain tough when revealed in hero form" else "gains tough when revealed in alter-ego") do
        tg <- testGame ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          when hero $ changeForm p >> pass
          mid <- addMinion p Minions.prowler
          resolve [MinionMessage mid (MinionStunned GameRef), MinionMessage mid (MinionConfused GameRef)]
          attrs <- getsGame (toAttrs . fromMaybe (error "missing Prowler") . HashMap.lookup mid . gameMinions)
          minionStunned attrs `shouldBe` False
          minionConfused attrs `shouldBe` False
          minionTough attrs `shouldBe` not hero

  describe "Razor Claws" do
    it "chooses highest printed hit points rather than modified or remaining hit points and grants piercing and +2 ATK" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        lizard <- addMinion p Minions.theLizard
        soldier <- addMinion p Minions.hydraSoldier
        resolve [MinionMessage lizard (MinionDamaged GameRef $ toDamage 4 FromAbility)]
        _ <- revealNow p Attachments.experimentalInjection
        pass
        attachmentTarget "27028" `shouldReturn` Just (EnemyMinionId soldier)
        _ <- revealNow p Attachments.razorClaws
        pass
        attachmentTarget "27059" `shouldReturn` Just (EnemyMinionId lizard)
        resolve [IdentityMessage p IdentityBecomeTough]
        start [MinionMessage lizard (MinionAttacks p)]
        pass
        damageOf p `shouldReturn` 5
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False

    it "allows the revealing player to break printed-hit-point ties" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        first <- addMinion p Minions.hydraSoldier
        second <- addMinion p Minions.hydraSoldier
        _ <- revealNow p Attachments.razorClaws
        offers (isTargetChoice first) `shouldReturn` True
        offers (isTargetChoice second) `shouldReturn` True
        choose "the second minion" (isTargetChoice second)
        pass
        attachmentTarget "27059" `shouldReturn` Just (EnemyMinionId second)

    it "surges when no minion is available" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [Schemes.bombScare]
        _ <- revealNow p Attachments.razorClaws
        pass
        attachmentTarget "27059" `shouldReturn` Nothing
        encounterDiscardCodes >>= (`shouldSatisfy` elem "27059")
        bombDealtTo p `shouldReturn` True

  describe "Slice and Dice" do
    it "attacks the player with fewest remaining hit points, even in alter-ego, without surging after a nonlethal attack" do
      tg <- testGame ["spider-man-miles-morales", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        clearHand other
        _ <- addMinion p Minions.prowler
        setDamage p 4
        setDamage other 6
        _ <- stackEncounterDeck [Schemes.bombScare]
        _ <- revealNow p Treacheries.sliceAndDice
        pass
        damageOf p `shouldReturn` 4
        damageOf other `shouldReturn` 8
        bombDealtTo p `shouldReturn` False

    it "allows choosing among players tied for fewest remaining hit points" do
      tg <- testGame ["spider-man-miles-morales", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        clearHand other
        _ <- addMinion p Minions.prowler
        setDamage p 4
        setDamage other 5
        _ <- revealNow p Treacheries.sliceAndDice
        offers (isTargetChoice p) `shouldReturn` True
        offers (isTargetChoice other) `shouldReturn` True
        choose "attack the other player" (isTargetChoice other)
        pass
        damageOf p `shouldReturn` 4
        damageOf other `shouldReturn` 7

    it "surges if Prowler is absent" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [Schemes.bombScare]
        _ <- revealNow p Treacheries.sliceAndDice
        pass
        bombDealtTo p `shouldReturn` True

    it "surges when Web Binding cancels the activation before Prowler makes an attack" do
      tg <- testGame ["ghost-spider", "spider-man-miles-morales"]
      let gwen = player1 tg
          miles = player2 tg
      inGame tg do
        changeForm gwen
        pass
        clearHand gwen
        clearHand miles
        _ <- addMinion miles Minions.prowler
        setDamage miles 8
        binding <- putInHand gwen "27006"
        resource <- putInHand gwen "01089"
        _ <- stackEncounterDeck [Schemes.bombScare]
        _ <- revealNow miles Treacheries.sliceAndDice
        playWith binding [resource]
        pass
        damageOf miles `shouldReturn` 8
        bombDealtTo miles `shouldReturn` True

    it "does not surge when tough prevents all damage from a completed attack" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- addMinion p Minions.prowler
        resolve [IdentityMessage p IdentityBecomeTough]
        _ <- stackEncounterDeck [Schemes.bombScare]
        _ <- revealNow p Treacheries.sliceAndDice
        pass
        damageOf p `shouldReturn` 0
        bombDealtTo p `shouldReturn` False

    it "surges when Prowler's attack defeats a defending ally" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- addMinion p Minions.prowler
        ally <- putIntoPlay p "01075"
        _ <- stackEncounterDeck [Schemes.bombScare]
        _ <- revealNow p Treacheries.sliceAndDice
        choose "Black Widow defends" \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
        pass
        alliesWith "01075" `shouldReturn` []
        bombDealtTo p `shouldReturn` True

    it "surges for the revealing player when Prowler defeats another identity" do
      tg <- testGame ["spider-man-miles-morales", "spider-man"]
      let p = player1 tg
          revealer = player2 tg
      inGame tg do
        clearHand p
        clearHand revealer
        _ <- addMinion revealer Minions.prowler
        setDamage p 8
        _ <- stackEncounterDeck [Schemes.bombScare]
        _ <- revealNow revealer Treacheries.sliceAndDice
        pass
        getsGame (HashMap.member p . gamePlayers) `shouldReturn` False
        bombDealtTo revealer `shouldReturn` True
