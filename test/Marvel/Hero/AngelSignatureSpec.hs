module Marvel.Hero.AngelSignatureSpec (spec) where

import Control.Monad (replicateM)
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Villain.Types (villainConfused, villainStunned)
import TestImport

angel :: IO TestGame
angel = testGame ["angel"]

inForm :: IdentityId -> Side -> GameAppT ()
inForm p side = changeFormTo p side >> pass >> clearHand p

pay :: IdentityId -> PlayerCard -> Natural -> GameAppT ()
pay p card n = do
  resources <- replicateM (fromIntegral n) (putInHand p "42022")
  playWith card resources

spec :: Spec
spec = describe "Angel signature events" do
  describe "Adaptive Plumage" do
    it "Angel removes three threat and confuses an independently chosen enemy" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        placeMainThreat 5
        before <- mainSchemeThreatNow
        card <- putInHand p "42003"
        pay p card 2
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` before - 3
        chooseVillain
        villainConfused . toAttrs <$> getVillain `shouldReturn` True
        villainDamageNow `shouldReturn` 0

    it "Archangel deals four damage and stuns that enemy" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        card <- putInHand p "42003"
        pay p card 2
        chooseVillain
        villainDamageNow `shouldReturn` 4
        villainStunned . toAttrs <$> getVillain `shouldReturn` True

    it "confusion replaces Angel's entire thwart ability, including its confuse" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        placeMainThreat 5
        before <- mainSchemeThreatNow
        resolve [IdentityMessage p IdentityConfused]
        card <- putInHand p "42003"
        pay p card 2
        mainSchemeThreatNow `shouldReturn` before
        villainConfused . toAttrs <$> getVillain `shouldReturn` False

    it "stun replaces Archangel's attack, including its stun" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        resolve [IdentityMessage p IdentityStunned]
        card <- putInHand p "42003"
        pay p card 2
        villainDamageNow `shouldReturn` 0
        villainStunned . toAttrs <$> getVillain `shouldReturn` False

    it "being stunned does not replace Angel's thwart" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        placeMainThreat 5
        before <- mainSchemeThreatNow
        resolve [IdentityMessage p IdentityStunned]
        card <- putInHand p "42003"
        pay p card 2
        chooseMainScheme
        chooseVillain
        mainSchemeThreatNow `shouldReturn` before - 3

  describe "Metamorphosis" do
    for_ [(A, C), (C, A), (B, A), (B, C)] $ \(from, to) ->
      it ("changes from " <> show from <> " to " <> show to <> " and resolves the new form's effect") do
        tg <- angel
        let p = player1 tg
        inGame tg do
          when (from /= B) $ inForm p from
          clearHand p
          placeMainThreat 5
          before <- mainSchemeThreatNow
          card <- putInHand p "42005"
          pay p card 1
          choose "new form" (isFormChoice $ Just to)
          playerIdentitySide <$> playerAttrs p `shouldReturn` to
          if to == A then do
            chooseMainScheme
            mainSchemeThreatNow `shouldReturn` before - 2
          else do
            chooseVillain
            villainDamageNow `shouldReturn` 3

    it "drawing in Warren follows changing to alter ego" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        card <- putInHand p "42005"
        pay p card 1
        choose "Warren" (isFormChoice $ Just B)
        length <$> handOf p `shouldReturn` 1

  describe "Natural Flight" do
    for_ [A, C] $ \side -> it ("removes four threat in " <> show side) do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p side
        placeMainThreat 5
        before <- mainSchemeThreatNow
        card <- putInHand p "42006"
        pay p card 1
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` before - 4

    it "Angel ignores crisis to remove main-scheme threat" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        void $ revealNow p SideSchemes.crowdControl
        pass
        placeMainThreat 5
        before <- mainSchemeThreatNow
        card <- putInHand p "42006"
        pay p card 1
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` before - 4

    it "Angel ignores patrol while Archangel cannot target the main scheme" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        void $ addMinion p Minions.servantBot
        void $ revealNow p SideSchemes.breakinAndTakin
        pass
        placeMainThreat 5
        before <- mainSchemeThreatNow
        card <- putInHand p "42006"
        pay p card 1
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` before - 4
        pass
        resolve [IdentityMessage p $ ChangedToForm C]
        next <- putInHand p "42006"
        pay p next 1
        main <- mainSchemeIdNow
        offers (isTargetChoice main) `shouldReturn` False

  describe "Razor Dive" do
    for_ [A, C] $ \side -> it ("deals six attack damage in " <> show side) do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p side
        card <- putInHand p "42007"
        pay p card 2
        chooseVillain
        villainDamageNow `shouldReturn` 6

    it "Archangel pierces tough" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        vid <- villainIdNow
        resolve [VillainMessage vid VillainBecomeTough]
        card <- putInHand p "42007"
        pay p card 2
        chooseVillain
        villainDamageNow `shouldReturn` 6

    it "Angel's attack is stopped by tough" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        vid <- villainIdNow
        resolve [VillainMessage vid VillainBecomeTough]
        card <- putInHand p "42007"
        pay p card 2
        chooseVillain
        villainDamageNow `shouldReturn` 0

    it "Archangel sends excess minion damage to the villain" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        mid <- addMinion p Minions.hydraBomber
        card <- putInHand p "42007"
        pay p card 2
        choose "minion" (isTargetChoice mid)
        villainDamageNow `shouldReturn` 4

  describe "Avian Anatomy" do
    it "optionally returns the exact paid Aerial event after its effects" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        placeMainThreat 5
        card <- putInHand p "42006"
        anatomy <- putInHand p "42008"
        other <- putInHand p "42022"
        playWith card [anatomy, other]
        choose "Avian Anatomy" (isLabel "$label.avianAnatomy")
        elem card <$> handOf p `shouldReturn` False
        chooseMainScheme
        elem card <$> handOf p `shouldReturn` True
        elem anatomy <$> discardOf p `shouldReturn` True

    it "can decline the spending response" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        placeMainThreat 5
        card <- putInHand p "42006"
        anatomy <- putInHand p "42008"
        other <- putInHand p "42022"
        playWith card [anatomy, other]
        pass
        chooseMainScheme
        elem card <$> discardOf p `shouldReturn` True

    it "returns an Aerial event even when a status replaces its effect" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        resolve [IdentityMessage p IdentityConfused]
        placeMainThreat 5
        card <- putInHand p "42006"
        anatomy <- putInHand p "42008"
        other <- putInHand p "42022"
        playWith card [anatomy, other]
        choose "Avian Anatomy" (isLabel "$label.avianAnatomy")
        elem card <$> handOf p `shouldReturn` True

    it "is not offered when spent on a non-Aerial card" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        card <- putInHand p "01092"
        anatomy <- putInHand p "42008"
        energy <- putInHand p "01088"
        playWith card [anatomy, energy]
        offers (isLabel "$label.avianAnatomy") `shouldReturn` False
        elem card <$> discardOf p `shouldReturn` False

  describe "Aerial Agility" do
    it "still allows the hero to make a basic defense after playing it" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        void $ stackEncounterDeck [Treacheries.stampede]
        card <- putInHand p "42004"
        resource <- putInHand p "42022"
        vid <- villainIdNow
        start [VillainMessage vid $ VillainAttacks p]
        playWith card [resource]
        chooseIf isNoResponse
        offers isDefend `shouldReturn` True
        choose "basic defense" isDefend
        pass
        damageOf p `shouldReturn` 0
        isExhaustedPlayer p `shouldReturn` True


    it "Angel ignores every boost icon and boost ability for that attack only" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        void $ stackEncounterDeck [Treacheries.spearShot, Treacheries.stampede]
        card <- putInHand p "42004"
        resource <- putInHand p "42022"
        vid <- villainIdNow
        start [VillainMessage vid $ VillainAttacksWithAdditionalBoostCards p 1]
        playWith card [resource]
        pass
        damageOf p `shouldReturn` 2
        void $ stackEncounterDeck [Treacheries.stampede]
        start [VillainMessage vid $ VillainAttacks p]
        pass
        damageOf p `shouldReturn` 5

    it "Avian Anatomy returns the defense before boost cards are flipped" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        void $ stackEncounterDeck [Treacheries.spearShot]
        card <- putInHand p "42004"
        anatomy <- putInHand p "42008"
        vid <- villainIdNow
        start [VillainMessage vid $ VillainAttacks p]
        playWith card [anatomy]
        choose "Avian Anatomy" (isLabel "$label.avianAnatomy")
        pass
        elem card <$> handOf p `shouldReturn` True
        damageOf p `shouldReturn` 2


    it "Archangel gains tough and retaliate for the attack" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        void $ stackEncounterDeck [Minions.hydraBomber]
        card <- putInHand p "42004"
        resource <- putInHand p "42022"
        vid <- villainIdNow
        start [VillainMessage vid $ VillainAttacks p]
        playWith card [resource]
        pass
        pass
        damageOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 1
