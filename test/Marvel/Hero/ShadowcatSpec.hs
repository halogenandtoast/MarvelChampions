module Marvel.Hero.ShadowcatSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Minion.Cards qualified as Minions
import Marvel.Villain.Types (villainConfused)
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Query
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Obligation.Cards qualified as Obligations
import TestImport

massFormIs :: IdentityId -> CardDef -> GameAppT Bool
massFormIs p def = selectAny (UpgradeIs def <> UpgradeControlledBy (IdentityWithId p))

asShadowcat :: IdentityId -> GameAppT ()
asShadowcat p = changeForm p >> pass >> clearHand p

spec :: Spec
spec = describe "Shadowcat" do
  it "sets up Solid in play and starts as Kitty with nine hit points" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 9
      massFormIs p Upgrades.solid `shouldReturn` True
      massFormIs p Upgrades.phased `shouldReturn` False

  it "Kitty can flip mass form once per round without changing identity form" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      choose "Phase Control" \case
        UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 2
        _ -> False
      massFormIs p Upgrades.phased `shouldReturn` True
      (.playerIdentitySide) <$> playerAttrs p `shouldReturn` B
      offers (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 2; _ -> False) `shouldReturn` False

  it "Airwalk removes two threat in Solid and leaves the mass form unchanged" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      asShadowcat p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      card <- putInHand p "32039"
      payment <- putInHand p "01090"
      playWith card [payment]
      sid <- mainSchemeIdNow
      choose "main scheme" (isTargetChoice sid)
      mainSchemeThreatNow `shouldReturn` (before - 2)
      massFormIs p Upgrades.solid `shouldReturn` True

  it "Shadowcat Surprise deals three damage and readies her" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      asShadowcat p
      runNow [IdentityMessage p ExhaustedIdentity]
      card <- putInHand p "32037"
      payment <- putInHand p "01090"
      playWith card [payment]
      chooseVillain
      villainDamageNow `shouldReturn` 3
      ((.playerIdentityExhausted) <$> playerAttrs p) `shouldReturn` False

  it "Kitty's Room heals two damage while in Solid alter-ego" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      setDamage p 4
      room <- putIntoPlay p "32033"
      choose "Kitty's Room" (isAbilityOfCard room)
      damageOf p `shouldReturn` 2

  it "Lockheed deals two damage on entering in Solid form" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      asShadowcat p
      card <- putInHand p "32032"
      payment <- putInHand p "01090"
      playWith card [payment]
      choose "Lockheed" (isAbilityOfCard card)
      chooseVillain
      villainDamageNow `shouldReturn` 2

  it "Solid may flip after a basic attack; Phased must flip after the next attack" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      asShadowcat p
      choose "attack" (isHeroAttack p)
      chooseVillain
      choose "Solid response" \case
        UseAbility ab -> case abilitySource ab of UpgradeRef _ -> abilityIndex ab == 1; _ -> False
        _ -> False
      massFormIs p Upgrades.phased `shouldReturn` True
      runNow [IdentityMessage p ReadiedIdentity]
      choose "attack again" (isHeroAttack p)
      chooseVillain
      choose "Phased forced response" \case
        UseAbility ab -> case abilitySource ab of UpgradeRef _ -> abilityIndex ab == 1; _ -> False
        _ -> False
      massFormIs p Upgrades.solid `shouldReturn` True

  it "Airwalk removes four threat in Phased form without flipping mass form" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      choose "Phase Control" \case
        UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 2
        _ -> False
      asShadowcat p
      placeMainThreat 6
      before <- mainSchemeThreatNow
      card <- putInHand p "32039"
      payment <- putInHand p "01090"
      playWith card [payment]
      sid <- mainSchemeIdNow
      choose "main scheme" (isTargetChoice sid)
      mainSchemeThreatNow `shouldReturn` (before - 4)
      massFormIs p Upgrades.phased `shouldReturn` True

  it "Kitty's Room draws a card in Phased alter-ego" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      choose "Phase Control" \case
        UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 2
        _ -> False
      room <- putIntoPlay p "32033"
      before <- length <$> handOf p
      choose "Kitty's Room" (isAbilityOfCard room)
      (length <$> handOf p) `shouldReturn` (before + 1)

  it "Phased and Confused replaces an enemy attack and then confuses it" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      asShadowcat p
      card <- putInHand p "32036"
      two <- putInHand p "01090"
      one <- putInHand p "32033"
      vid <- villainIdNow
      playOn card vid [two,one]
      resolve [VillainMessage vid (VillainAttacks p)]
      damageOf p `shouldReturn` 0
      v <- getVillain
      villainConfused (toAttrs v) `shouldBe` True

  it "Quick Shift enters Phased before the attack deals damage and flips after defending" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      asShadowcat p
      card <- putInHand p "32040"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacks p)]
      playWith card []
      massFormIs p Upgrades.phased `shouldReturn` True
      pass
      damageOf p `shouldReturn` 0
      forms <- selectList (UpgradeIs Upgrades.phased <> UpgradeControlledBy (IdentityWithId p))
      for_ forms (\uid -> chooseIf (isAbilityOf $ UpgradeRef uid))
      massFormIs p Upgrades.solid `shouldReturn` True

  it "Acute Control damages a minion whose guard Shadowcat actually bypassed" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      choose "Phase Control" \case
        UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 2
        _ -> False
      asShadowcat p
      acute <- putIntoPlay p "32034"
      mid <- addMinion p Minions.hellfirePawn
      choose "attack" (isHeroAttack p)
      chooseVillain
      choose "Acute Control" (isAbilityOfCard acute)
      choose "guard minion" (isTargetChoice mid)
      dealt <- getsGame $ maybe 0 (minionDamage . toAttrs) . HashMap.lookup mid . gameMinions
      dealt `shouldBe` 2

  it "Intangible Interference removes threat from a crisis scheme Shadowcat bypassed" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      choose "Phase Control" \case
        UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 2
        _ -> False
      asShadowcat p
      interference <- putIntoPlay p "32035"
      scheme <- revealNow p SideSchemes.defenseNetwork
      pass
      startingThreat <- sideSchemeThreat (sideSchemeOf scheme)
      startingThreat `shouldBe` 3
      placeMainThreat 4
      offers (isAbilityOfCard interference) `shouldReturn` False
      choose "thwart" (isHeroThwart p)
      sid <- mainSchemeIdNow
      choose "main scheme" (isTargetChoice sid)
      choose "Intangible Interference" (isAbilityOfCard interference)
      choose "the crisis scheme" (isTargetChoice $ sideSchemeOf scheme)
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` (startingThreat - 2)

  it "Phased Strike deals six damage and can discard an enemy's Hero Action attachment" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      clearHand p
      choose "Phase Control" \case
        UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 2
        _ -> False
      asShadowcat p
      attachment <- revealNow p Attachments.hydraSidearm
      pass
      card <- putInHand p "32038"
      two <- putInHand p "01090"
      one <- putInHand p "32033"
      playWith card [two,one]
      chooseVillain
      let aid = AttachmentId (unCardId $ ecCardId attachment)
      choose "discard Hero Action attachment" (isTargetChoice aid)
      villainDamageNow `shouldReturn` 6
      getsGame (HashMap.member aid . gameAttachments) `shouldReturn` False

  it "Permanently Phased stays in play, locks mass form and attacks, and can be removed in alter-ego" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      asShadowcat p
      _ <- revealNow p Obligations.permanentlyPhased
      pass
      massFormIs p Upgrades.phased `shouldReturn` True
      offers (isHeroAttack p) `shouldReturn` False
      hasObligation <- getsGame $ any ((== "32055") . toCardCode . getCardDef) . HashMap.elems . gameObligations
      hasObligation `shouldBe` True
      runNow [IdentityMessage p (ChangedToForm B)]
      choose "remove Permanently Phased" \case
        UseAbility ab -> case abilitySource ab of ObligationRef _ -> abilityIndex ab == 1; _ -> False
        _ -> False
      getsGame (any ((== "32055") . toCardCode . getCardDef) . HashMap.elems . gameObligations) `shouldReturn` False
