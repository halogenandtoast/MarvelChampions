module Marvel.Hero.ScarletWitchSignatureSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport

scarletWitch :: IO TestGame
scarletWitch = testGame ["scarlet-witch"]

-- | Scarlet Witch in hero form, her hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeForm p
  pass
  clearHand p

isCharacter :: CharacterId -> Choice -> Bool
isCharacter c = \case
  TargetLabel t _ -> t == characterRef c
  _ -> False

-- | Decline Chaos Control, offered as each boost-icon count is made in hero form.
declineCounts :: GameAppT ()
declineCounts = whenM (offers isNoResponse) $ choose "decline Chaos Control" isNoResponse >> declineCounts

isAnyTarget :: Choice -> Bool
isAnyTarget = \case
  TargetLabel {} -> True
  _ -> False

-- Advance has no boost icon, Stampede 1, Explosion 2, Under Fire 3.
spec :: Spec
spec = describe "Scarlet Witch's signature events" do
  describe "Chaos Magic (15003)" do
    it "plays a card from hand for nothing, then discards its printed cost from the encounter deck" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- setEncounterDeck (replicate 5 Treacheries.advance) []
        chaos <- putInHand p "15003"
        mansion <- putInHand p "01091"
        helicarrier <- putInHand p "01092"
        playWith chaos []
        -- neither is affordable, both are offered: the cost is ignored
        offers (isPlay helicarrier) `shouldReturn` True
        choose "play Avengers Mansion" (isPlay mansion)
        payWith []
        supportsWith "01091" >>= (`shouldSatisfy` ((== 1) . length))
        length <$> encounterDiscardNow `shouldReturn` 4
        encounterDeckSize `shouldReturn` 1
        -- a later card costs as usual
        offers (isPlay helicarrier) `shouldReturn` False
        handOf p `shouldReturn` [helicarrier]

    it "cannot be played without another card in hand to play" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        chaos <- putInHand p "15003"
        _ <- putInHand p "01088"
        offers (isPlay chaos) `shouldReturn` False
        _ <- putInHand p "01092"
        offers (isPlay chaos) `shouldReturn` True

  describe "Hex Bolt (15004)" do
    it "for 0, 1 and 2 boost icons: 2 damage to an enemy, 2 threat removed, a card drawn" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        threat <- mainSchemeThreatNow
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.stampede, Treacheries.explosion, Treacheries.advance] []
        hex <- putInHand p "15004"
        energy <- putInHand p "01088"
        playWith hex [energy]
        declineCounts
        chooseIf isAnyTarget -- the villain
        chooseIf isAnyTarget -- the main scheme
        villainDamageNow `shouldReturn` 2
        mainSchemeThreatNow `shouldReturn` (threat - 2)
        length <$> handOf p `shouldReturn` 1
        length <$> encounterDiscardNow `shouldReturn` 3

    it "for 3 or more boost icons, places a status card of the player's choice on any character" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        vid <- villainIdNow
        _ <- setEncounterDeck [Treacheries.underFire, Treacheries.underFire, Treacheries.explosion, Treacheries.advance] []
        hex <- putInHand p "15004"
        energy <- putInHand p "01088"
        playWith hex [energy]
        declineCounts
        -- the card drawn first, as the bullets are written
        length <$> handOf p `shouldReturn` 1
        choose "the villain" (isCharacter (VillainCharacter vid))
        choose "stunned" (isLabel "$label.stunnedStatus")
        choose "Scarlet Witch" (isCharacter (IdentityCharacter p))
        choose "tough" (isLabel "$label.toughStatus")
        V.villainStunned . toAttrs <$> getVillain `shouldReturn` True
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True

  describe "Molecular Decay (15005)" do
    it "deals 5 damage and 1 more for each boost icon on the top 2 encounter cards" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- setEncounterDeck [Treacheries.explosion, Treacheries.stampede, Treacheries.advance] []
        decay <- putInHand p "15005"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playWith decay payment
        chooseIf isAnyTarget
        declineCounts
        villainDamageNow `shouldReturn` 8
        encounterDeckSize `shouldReturn` 1

    it "is one instance of damage: a tough villain takes none of it" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        vid <- villainIdNow
        runNow [GiveStatus (characterRef (VillainCharacter vid)) ToughStatus]
        _ <- setEncounterDeck [Treacheries.explosion, Treacheries.stampede, Treacheries.advance] []
        decay <- putInHand p "15005"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playWith decay payment
        chooseIf isAnyTarget
        declineCounts
        villainDamageNow `shouldReturn` 0
        V.villainTough . toAttrs <$> getVillain `shouldReturn` False

  describe "Warp Reality (15006)" do
    it "cancels a treachery with surge and discards as many encounter cards as its boost icons" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.stampede, Treacheries.explosion] []
        warp <- putInHand p "15006"
        strength <- putInHand p "01090"
        exhaustion <- encounterCard Treacheries.exhaustion
        runNow [RevealEncounterCard p exhaustion]
        playWith warp [strength]
        declineCounts
        isExhaustedNow p `shouldReturn` False
        map ecCardId <$> encounterDiscardNow >>= (`shouldSatisfy` elem (ecCardId exhaustion))
        -- Exhaustion's 2 icons: 2 discarded, and no card revealed for its surge
        encounterDiscardCodes >>= (`shouldSatisfy` \codes -> length codes == 3)
        encounterDeckSize `shouldReturn` 1

    it "cancels a minion: it does not enter play, nor does its incite place threat" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        threat <- mainSchemeThreatNow
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.stampede, Treacheries.explosion] []
        warp <- putInHand p "15006"
        strength <- putInHand p "01090"
        regular <- encounterCard Minions.hydraRegular
        runNow [RevealEncounterCard p regular]
        playWith warp [strength]
        declineCounts
        minionDamages `shouldReturn` []
        mainSchemeThreatNow `shouldReturn` threat
        map ecCardId <$> encounterDiscardNow >>= (`shouldSatisfy` elem (ecCardId regular))
        encounterDeckSize `shouldReturn` 1

    it "is a hero interrupt" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        warp <- putInHand p "15006"
        _ <- putInHand p "01090"
        regular <- encounterCard Minions.hydraRegular
        runNow [RevealEncounterCard p regular]
        offers (isPlay warp) `shouldReturn` False
