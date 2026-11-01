module Marvel.Hero.SpectrumSpec (spec) where

import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity, getModifiedKeywords )
import Marvel.Keyword (Keyword (Retaliate))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import TestImport

spectrum :: IO TestGame
spectrum = testGame ["spectrum"]

gamma, photon, pulsar :: CardCode
gamma = "21002"
photon = "21003"
pulsar = "21004"

-- | The player's one energy form upgrade with this code.
formOf :: CardCode -> GameAppT (Attrs Upgrade)
formOf code =
  upgradesWith code >>= \case
    [u] -> pure u
    us -> error $ "expected one " <> show code <> ", found " <> show (length us)

formIdOf :: CardCode -> GameAppT UpgradeId
formIdOf code = upgradeId <$> formOf code

-- | Which energy forms are faceup.
faceup :: GameAppT [CardCode]
faceup = filterM (fmap (not . upgradeFaceDown) . formOf) [gamma, photon, pulsar]

-- | Energy Transformation's choice of a facedown energy form.
chooseForm :: (HasCallStack) => CardCode -> GameAppT ()
chooseForm code = do
  u <- formIdOf code
  choose "the energy form" (isTargetChoice u)

-- | Monica flips to Spectrum with the turn's change of form, and changes to this energy
-- form; what answers it is left asked.
toSpectrum :: (HasCallStack) => IdentityId -> CardCode -> GameAppT ()
toSpectrum p code = do
  changeForm p
  choose "Energy Transformation" (isNamedAbility "Energy Transformation")
  chooseForm code

-- | The same, by a card's flip rather than the turn's.
flipToSpectrum :: (HasCallStack) => IdentityId -> CardCode -> GameAppT ()
flipToSpectrum p code = do
  runNow [IdentityMessage p (ChangedToForm A)]
  choose "Energy Transformation" (isNamedAbility "Energy Transformation")
  chooseForm code

data Stats = Stats {thwart :: Natural, attack :: Natural, defense :: Natural}
  deriving stock (Show, Eq)

heroStats :: IdentityId -> GameAppT Stats
heroStats p =
  currentIdentity <$> getPlayer p >>= \case
    HeroSide h -> Stats <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
    AlterEgoSide _ -> pure (Stats 0 0 0)

-- | Rhino attacks her with a boost of 2 (4 damage), and she defends.
defends :: IdentityId -> GameAppT ()
defends p = do
  _ <- stackEncounterDeck [Minions.vulture]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]
  choose "defend" isDefend

obligationOf :: EncounterCard -> ObligationId
obligationOf = ObligationId . unCardId . ecCardId

isChangeForm :: IdentityId -> Choice -> Bool
isChangeForm p = \case
  UseAbility ab -> ChangeForm `elem` abilityChoices ab && abilitySource ab == toSource p
  _ -> False

spec :: Spec
spec = describe "Spectrum" do
  describe "setup" do
    it "the starter deck is 40 cards and the 3 Permanent energy forms" do
      sum (map snd (starter "spectrum").cards) `shouldBe` (43 :: Int) :: IO ()

    it "Monica Rambeau's Setup puts the three energy forms into play facedown, out of the deck" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        for_ [gamma, photon, pulsar] \code -> do
          u <- formOf code
          upgradeFaceDown u `shouldBe` True
          upgradeController u `shouldBe` p
        faceup `shouldReturn` []
        hand <- handOf p
        deck <- unDeck . (.playerIdentityDeck) <$> playerAttrs p
        let codes = map toCardCode (hand <> deck)
        codes `shouldSatisfy` all (`notElem` [gamma, photon, pulsar])
        length codes `shouldBe` 40

  describe "Energy Transformation" do
    it "after changing to Spectrum she flips a facedown energy form of her choice faceup" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        changeForm p
        choose "Energy Transformation" (isNamedAbility "Energy Transformation")
        for_ [gamma, photon, pulsar] \code -> do
          u <- formIdOf code
          offers (isTargetChoice u) `shouldReturn` True
        chooseForm photon
        faceup `shouldReturn` [photon]
        heroStats p `shouldReturn` Stats 3 1 1

    it "changing form this way is not the turn's change of form" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        flipToSpectrum p gamma
        pass
        -- a change of energy form leaves the turn's flip to be used
        offers (isChangeForm p) `shouldReturn` True

    it "under Loss of Control, no energy form is turned faceup" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.lossOfControl
        pass
        changeForm p
        choose "Energy Transformation" (isNamedAbility "Energy Transformation")
        pass
        faceup `shouldReturn` []
        heroStats p `shouldReturn` Stats 1 1 1

  describe "Power Down" do
    it "after changing to Monica Rambeau, every energy form is turned facedown" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p pulsar
        pass
        runNow [IdentityMessage p (ChangedToForm B)]
        choose "Power Down" (isNamedAbility "Power Down")
        pass
        faceup `shouldReturn` []

    it "still turns them facedown under Loss of Control" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p gamma
        pass
        _ <- revealNow p Obligations.lossOfControl
        pass
        runNow [IdentityMessage p (ChangedToForm B)]
        choose "Power Down" (isNamedAbility "Power Down")
        pass
        faceup `shouldReturn` []

  describe "Gamma (21002)" do
    it "gives Spectrum +2 ATK, and after she changes to it deals 1 damage to an enemy" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p gamma
        heroStats p `shouldReturn` Stats 1 3 1
        u <- formIdOf gamma
        choose "Gamma" (isAbilityOf u)
        vid <- villainIdNow
        choose "the villain" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 1

    it "facedown, gives nothing and offers nothing" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p photon
        g <- formIdOf gamma
        offers (isAbilityOf g) `shouldReturn` False
        heroStats p `shouldReturn` Stats 3 1 1

  describe "Photon (21003)" do
    it "gives Spectrum +2 THW, and after she changes to it removes 1 threat from a scheme" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        placeMainThreat 3
        before <- mainSchemeThreatNow
        toSpectrum p photon
        u <- formIdOf photon
        choose "Photon" (isAbilityOf u)
        m <- mainSchemeIdNow
        choose "the main scheme" (isTargetChoice m)
        mainSchemeThreatNow `shouldReturn` before - 1

  describe "Pulsar (21004)" do
    it "gives Spectrum +2 DEF, and after she changes to it heals 1 damage from her" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        setDamage p 3
        toSpectrum p pulsar
        heroStats p `shouldReturn` Stats 1 1 3
        u <- formIdOf pulsar
        choose "Pulsar" (isAbilityOf u)
        damageOf p `shouldReturn` 2

  describe "Blue Marvel (21005)" do
    it "after he enters play in hero form, Spectrum changes energy forms" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p gamma
        pass
        clearHand p
        blueMarvel <- putInHand p "21005"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playWith blueMarvel payment
        choose "Blue Marvel" (isAbilityOfCard blueMarvel)
        -- a different form: Gamma, faceup, is not offered
        g <- formIdOf gamma
        offers (isTargetChoice g) `shouldReturn` False
        chooseForm pulsar
        faceup `shouldReturn` [pulsar]

    it "in alter-ego form offers nothing" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        blueMarvel <- putInHand p "21005"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playWith blueMarvel payment
        -- no response window: back at the turn (his basic powers are offered there)
        offers isEndTurn `shouldReturn` True
        faceup `shouldReturn` []

  describe "Energy Duplication (21006)" do
    it "generates the printed resource of her faceup energy form" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "21006"
        clearHand p
        -- in alter-ego form no form is faceup: it generates nothing
        getAvailableResourcesFor p Nothing `shouldReturn` []
        toSpectrum p photon
        pass
        getAvailableResourcesFor p Nothing `shouldReturn` [Mental]

    it "with no energy form faceup it cannot pay (R24)" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        duplication <- putIntoPlay p "21006"
        clearHand p
        rumble <- putInHand p "21022"
        -- Monica: every form facedown
        offers (isPlay rumble) `shouldReturn` False
        -- Spectrum under Loss of Control: still none faceup
        _ <- revealNow p Obligations.lossOfControl
        pass
        changeForm p
        choose "Energy Transformation" (isNamedAbility "Energy Transformation")
        pass
        faceup `shouldReturn` []
        -- nothing pays for Ready to Rumble but itself, which cannot
        getAvailableResourcesFor p (Just rumble) `shouldReturn` []
        offers (isPlay rumble) `shouldReturn` False
        upgradeIsExhausted (upgradeIdOf duplication) `shouldReturn` False

    it "pays for a card as that resource" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        duplication <- putIntoPlay p "21006"
        toSpectrum p gamma
        pass
        clearHand p
        speed <- putInHand p "21008"
        energy <- putInHand p "01088"
        placeMainThreat 5
        before <- mainSchemeThreatNow
        choose "play Photon Speed" (isPlay speed)
        choose "Energy Duplication" (isAbilityOfCard duplication)
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        pass
        m <- mainSchemeIdNow
        choose "the main scheme" (isTargetChoice m)
        mainSchemeThreatNow `shouldReturn` before - 4
        upgradeIsExhausted (upgradeIdOf duplication) `shouldReturn` True

  describe "Gamma Blast (21007)" do
    it "changes her to Gamma energy form and deals 7 damage, without overkill" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p photon
        pass
        clearHand p
        mercenary <- addMinion p Minions.hydraMercenary
        blast <- putInHand p "21007"
        payment <- traverse (putInHand p) ["01090", "01090"]
        playWith blast payment
        pass
        faceup `shouldReturn` [gamma]
        choose "the mercenary" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 0

    it "already in Gamma energy form, the attack gains overkill" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p gamma
        pass
        clearHand p
        mercenary <- addMinion p Minions.hydraMercenary
        blast <- putInHand p "21007"
        payment <- traverse (putInHand p) ["01090", "01090"]
        playWith blast payment
        -- no change of form: Gamma's response is not offered again
        g <- formIdOf gamma
        offers (isAbilityOf g) `shouldReturn` False
        choose "the mercenary" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        -- 7 damage to 3 hit points: 4 go on to Rhino
        villainDamageNow `shouldReturn` 4

    it "the change of energy form opens Gamma's response" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p pulsar
        pass
        clearHand p
        blast <- putInHand p "21007"
        payment <- traverse (putInHand p) ["01090", "01090"]
        playWith blast payment
        g <- formIdOf gamma
        offers (isAbilityOf g) `shouldReturn` True

  describe "Photon Speed (21008)" do
    it "changes her to Photon energy form and removes 4 threat from a scheme" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p gamma
        pass
        clearHand p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        speed <- putInHand p "21008"
        payment <- traverse (putInHand p) ["01089"]
        playWith speed payment
        pass
        faceup `shouldReturn` [photon]
        m <- mainSchemeIdNow
        choose "the main scheme" (isTargetChoice m)
        mainSchemeThreatNow `shouldReturn` before - 4

    it "with a crisis in play, only the side scheme can be thwarted" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p gamma
        pass
        clearHand p
        placeMainThreat 6
        meltdown <- revealNow p SideSchemes.reactorMeltdown
        pass
        speed <- putInHand p "21008"
        payment <- traverse (putInHand p) ["01089"]
        playWith speed payment
        pass
        m <- mainSchemeIdNow
        offers (isTargetChoice m) `shouldReturn` False
        offers (isTargetChoice (sideSchemeOf meltdown)) `shouldReturn` True

    it "already in Photon energy form, it ignores the crisis icon" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p photon
        pass
        clearHand p
        placeMainThreat 6
        _ <- revealNow p SideSchemes.reactorMeltdown
        pass
        before <- mainSchemeThreatNow
        speed <- putInHand p "21008"
        payment <- traverse (putInHand p) ["01089"]
        playWith speed payment
        m <- mainSchemeIdNow
        choose "the main scheme" (isTargetChoice m)
        mainSchemeThreatNow `shouldReturn` before - 4

  describe "Pulsar Shield (21009)" do
    it "when she defends, changes her to Pulsar energy form and readies her" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p gamma
        pass
        clearHand p
        shield <- putInHand p "21009"
        payment <- traverse (putInHand p) ["01088"]
        defends p
        choose "Pulsar Shield" (isPlay shield)
        payWith payment
        pass
        faceup `shouldReturn` [pulsar]
        isExhaustedNow p `shouldReturn` False
        -- 4 damage against 1 DEF and Pulsar's 2
        damageOf p `shouldReturn` 1
        villainDamageNow `shouldReturn` 0
        keywords <- getModifiedKeywords =<< getPlayer p
        keywords `shouldSatisfy` notElem (Retaliate 1)

    it "already in Pulsar energy form, she gains retaliate 1 until the end of the phase" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p pulsar
        pass
        clearHand p
        shield <- putInHand p "21009"
        payment <- traverse (putInHand p) ["01088"]
        defends p
        choose "Pulsar Shield" (isPlay shield)
        payWith payment
        pass
        faceup `shouldReturn` [pulsar]
        isExhaustedNow p `shouldReturn` False
        damageOf p `shouldReturn` 1
        -- her retaliate 1 answered Rhino's attack
        villainDamageNow `shouldReturn` 1
        keywords <- getModifiedKeywords =<< getPlayer p
        keywords `shouldSatisfy` elem (Retaliate 1)

  describe "Speed of Light (21010)" do
    it "changes energy forms and draws 1 card" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p gamma
        pass
        clearHand p
        speed <- putInHand p "21010"
        choose "play Speed of Light" (isPlay speed)
        g <- formIdOf gamma
        offers (isTargetChoice g) `shouldReturn` False
        chooseForm photon
        pass
        faceup `shouldReturn` [photon]
        length <$> handOf p `shouldReturn` 1

    it "under Loss of Control only draws" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        toSpectrum p gamma
        pass
        _ <- revealNow p Obligations.lossOfControl
        pass
        clearHand p
        speed <- putInHand p "21010"
        choose "play Speed of Light" (isPlay speed)
        pass
        faceup `shouldReturn` [gamma]
        length <$> handOf p `shouldReturn` 1

  describe "Loss of Control (21026)" do
    it "stays in play, and exhausting Monica Rambeau removes it from the game" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        card <- revealNow p Obligations.lossOfControl
        pass
        obligationsInPlay `shouldReturn` 1
        choose "Loss of Control" (isAbilityOf (ObligationRef (obligationOf card)))
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "21026")

    it "its Alter-Ego Action is not offered in hero form" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        card <- revealNow p Obligations.lossOfControl
        pass
        changeForm p
        choose "Energy Transformation" (isNamedAbility "Energy Transformation")
        pass
        offers (isAbilityOf (ObligationRef (obligationOf card))) `shouldReturn` False
        obligationsInPlay `shouldReturn` 1

    it "is only its owner's to use" do
      tg <- testGame ["spectrum", "captain-america"]
      let p = player1 tg
          p2 = player2 tg
      inGame tg do
        card <- revealNow p Obligations.lossOfControl
        pass
        let isLoss = (== ObligationRef (obligationOf card)) . abilitySource
        any isLoss <$> getAbilitiesFor p `shouldReturn` True
        any isLoss <$> getAbilitiesFor p2 `shouldReturn` False
