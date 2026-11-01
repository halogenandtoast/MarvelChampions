module Marvel.Hero.VisionSignatureSpec (spec) where

import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Modifier (Modifier (ThreatCannotBeRemovedByThwarting))
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Hero.VisionSpec ( changeMass )
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Query
import TestImport

-- A real effect-source restriction, independent of any scenario's signature
-- scheme association. It mirrors Held Hostage's thwart-only prohibition.
newtype ThwartPreventionEffect = ThwartPreventionEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage ThwartPreventionEffect where
  runMessage msg (ThwartPreventionEffect attrs) = ThwartPreventionEffect <$> runMessage msg attrs

preventMainThwarting :: GameAppT ()
preventMainThwarting = do
  mid <- mainSchemeIdNow
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (ThwartPreventionEffect EffectAttrs
    { effectId = eid, effectCardCode = "07005", effectSource = GameRef
    , effectMatcher = MainSchemeEntity mid
    , effectModifiers = [ThreatCannotBeRemovedByThwarting], effectEnds = Nothing })

vision :: IO TestGame
vision = testGame ["vision"]

asVision :: IdentityId -> GameAppT ()
asVision p = changeForm p >> pass >> clearHand p

asDense :: IdentityId -> GameAppT ()
asDense p = do
  asVision p
  changeMass p
  pass
  clearHand p

paying :: IdentityId -> CardCode -> GameAppT PlayerCard
paying p code = do
  card <- putInHand p code
  cost <- traverse (putInHand p) ["01088", "01089"]
  playWith card cost
  pure card

spec :: Spec
spec = describe "Vision signature events" do
  describe "Superdense Strike" do
    it "deals five damage and pierces tough in Dense form" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asDense p
        vid <- villainIdNow
        resolve [VillainMessage vid VillainBecomeTough]
        card <- putInHand p "26009"
        resource <- putInHand p "01090"
        playWith card [resource]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5

    it "cannot be played while Intangible" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        card <- putInHand p "26009"
        _ <- putInHand p "01090"
        offers (isPlay card) `shouldReturn` False

  describe "Solar Beam" do
    it "cannot initiate its Intangible thwart when patrol leaves no legal scheme" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        _ <- addMinion p Minions.badoonLieutenant
        placeMainThreat 4
        card <- putInHand p "26008"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay card) `shouldReturn` False

    it "excludes a patrolled main scheme but performs a real thwart that can trigger Justice Served" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        _ <- addMinion p Minions.badoonLieutenant
        side <- revealNow p Schemes.bombScare
        pass
        placeMainThreat 4
        justice <- putIntoPlay p "22014"
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- paying p "26008"
        mid <- mainSchemeIdNow
        offers (isTargetChoice mid) `shouldReturn` False
        choose "thwart the legal side scheme" (isTargetChoice $ sideSchemeOf side)
        exhaustedNow p `shouldReturn` True
        offers (isAbilityOfCard justice) `shouldReturn` True
        choose "respond to Solar Beam removing all threat" (isAbilityOfCard justice)
        pass
        exhaustedNow p `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` 4
        sideSchemeThreat (sideSchemeOf side) `shouldReturn` 0

    it "removes five threat while Intangible" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        _ <- paying p "26008"
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 5

    it "deals seven damage while Dense" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asDense p
        _ <- paying p "26008"
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 7

    it "does not consume stun when its Intangible action thwarts" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        resolve [IdentityMessage p IdentityStunned]
        _ <- paying p "26008"
        chooseMainScheme
        pass
        isStunned p `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` before - 5

    it "does not consume confusion when its Dense action attacks" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asDense p
        resolve [IdentityMessage p IdentityConfused]
        _ <- paying p "26008"
        chooseVillain
        pass
        isConfused p `shouldReturn` True
        villainDamageNow `shouldReturn` 7

  describe "Just Passing Through" do
    it "still ignores patrol and crisis when choosing the main scheme" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        _ <- addMinion p Minions.badoonLieutenant
        crisis <- revealNow p Schemes.crowdControl
        pass
        placeMainThreat 4
        card <- putInHand p "26010"
        resource <- putInHand p "01093"
        playWith card [resource]
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` 1
        sideSchemeThreat (sideSchemeOf crisis) `shouldReturn` 2

    it "does not bypass a thwart prohibition while ignoring patrol and crisis" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        _ <- addMinion p Minions.badoonLieutenant
        crisis <- revealNow p Schemes.crowdControl
        pass
        placeMainThreat 4
        preventMainThwarting
        card <- putInHand p "26010"
        resource <- putInHand p "01093"
        playWith card [resource]
        mid <- mainSchemeIdNow
        offers (isTargetChoice mid) `shouldReturn` False
        choose "thwart the unprotected crisis scheme" (isTargetChoice $ sideSchemeOf crisis)
        pass
        mainSchemeThreatNow `shouldReturn` 4
        sideSchemeThreat (sideSchemeOf crisis) `shouldReturn` 0

    it "removes three threat in Intangible form" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        placeMainThreat 4
        before <- mainSchemeThreatNow
        card <- putInHand p "26010"
        resource <- putInHand p "01093"
        playWith card [resource]
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 3

    it "cannot be played in Dense form" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asDense p
        placeMainThreat 4
        card <- putInHand p "26010"
        _ <- putInHand p "01093"
        offers (isPlay card) `shouldReturn` False

  describe "thwart-only threat prevention" do
    for_ ["26008", "26010", "25033"] $ \code ->
      it ("prevents a labeled thwart's initiation when every scheme is protected: " <> show code) do
        tg <- vision
        let p = player1 tg
        inGame tg do
          asVision p
          when (code == "25033") $ void $ putIntoPlay p "25014"
          placeMainThreat 4
          preventMainThwarting
          card <- putInHand p code
          _ <- putInHand p "01088"
          _ <- putInHand p "01089"
          offers (isPlay card) `shouldReturn` False

    for_ ["26008", "26010"] $ \code ->
      it ("excludes the protected main scheme from a labeled thwart while another scheme remains legal: " <> show code) do
        tg <- vision
        let p = player1 tg
        inGame tg do
          asVision p
          side <- revealNow p Schemes.bombScare
          pass
          placeMainThreat 4
          preventMainThwarting
          card <- putInHand p code
          resources <- traverse (putInHand p) (if code == "26008" then ["01088", "01089"] else ["01093"])
          playWith card resources
          mid <- mainSchemeIdNow
          offers (isTargetChoice mid) `shouldReturn` False
          choose "thwart the legal side scheme" (isTargetChoice $ sideSchemeOf side)
          pass
          mainSchemeThreatNow `shouldReturn` 4
          sideSchemeThreat (sideSchemeOf side) `shouldReturn` 0

    it "still permits an unlabeled remove-threat action on the protected scheme" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        placeMainThreat 4
        preventMainThwarting
        team <- putIntoPlay p "01064"
        choose "Surveillance Team removes threat without thwarting" (isAbilityOfCard team)
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` 3

  describe "Phase Disruption" do
    it "confuses the chosen enemy and discards its Hero Action attachment" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        vid <- villainIdNow
        _ <- revealNow p Attachments.enhancedIvoryHorn
        pass
        horn <- selectJust (AttachmentOnEnemyWithHeroAbility $ EnemyVillainId vid)
        card <- putInHand p "26011"
        resource <- putInHand p "01088"
        playOn card (EnemyVillainId vid) [resource]
        choose "discard the horn" (isTargetChoice horn)
        pass
        enemyMatches ConfusedEnemy (EnemyVillainId vid) `shouldReturn` True
        selectAny (AttachmentOnEnemyWithHeroAbility $ EnemyVillainId vid) `shouldReturn` False

    it "still confuses when the enemy has no removable attachment" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        vid <- villainIdNow
        card <- putInHand p "26011"
        resource <- putInHand p "01088"
        playOn card (EnemyVillainId vid) [resource]
        pass
        enemyMatches ConfusedEnemy (EnemyVillainId vid) `shouldReturn` True

    it "cannot be played while Dense" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asDense p
        card <- putInHand p "26011"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False

  describe "Mass Increase" do
    it "prevents attack damage and stuns the attacker after Vision's defense" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asDense p
        card <- putInHand p "26012"
        resource <- putInHand p "01093"
        _ <- stackEncounterDeck [Minions.vulture]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "Vision defends" isDefend
        choose "Mass Increase" (isPlay card)
        enemyMatches StunnedEnemy (EnemyVillainId vid) `shouldReturn` False
        payWith [resource]
        pass
        damageOf p `shouldReturn` 0
        enemyMatches StunnedEnemy (EnemyVillainId vid) `shouldReturn` True
        isExhaustedNow p `shouldReturn` True

    it "requires Dense even when Corrupted Programming permits an Intangible defense" do
      tg <- vision
      let p = player1 tg
      inGame tg do
        asVision p
        _ <- revealNow p Obligations.corruptedProgramming
        pass
        card <- putInHand p "26012"
        _ <- putInHand p "01093"
        _ <- stackEncounterDeck [Minions.vulture]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "Vision defends" isDefend
        offers (isPlay card) `shouldReturn` False
