module Marvel.Aspect.MadTitansShadowLeadershipSpec (spec) where

import Marvel.Ally.Types qualified as Ally
import Marvel.Card.Def (canonicalCardCode)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (PlayerIdentitySide (..), currentIdentity, getModifiedCost)
import Marvel.Villain.Types qualified as V
import TestImport

spectrum :: IO TestGame
spectrum = testGame ["spectrum"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

-- | The response of the card's own (its ability 1), not an ally's basic THW or ATK.
isResponseOf :: PlayerCard -> Choice -> Bool
isResponseOf c ch = isAbilityOfCard c ch && case ch of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

-- | The player's energy form upgrade of this code (Gamma 21002, Photon 21003, Pulsar 21004).
energyForm :: CardCode -> GameAppT UpgradeId
energyForm code = upgradesWith code >>= \case
  (u : _) -> pure (upgradeId u)
  [] -> error $ "no energy form " <> show code

-- | Monica Rambeau flips to Spectrum, and Energy Transformation flips the energy form
-- given faceup: the energy form's window is open next, then the flip's.
toSpectrum :: IdentityId -> CardCode -> GameAppT ()
toSpectrum p code = do
  changeForm p
  choose "Energy Transformation" (isLabeledAbility "Energy Transformation")
  form <- energyForm code
  choose "the energy form" (isTargetChoice form)

-- | Spectrum in the energy form given, no response taken, her hand empty.
asSpectrum :: IdentityId -> CardCode -> GameAppT ()
asSpectrum p code = do
  toSpectrum p code
  pass
  clearHand p

data Stats = Stats {thwart :: Natural, attack :: Natural, defense :: Natural}
  deriving stock (Show, Eq)

heroStats :: IdentityId -> GameAppT Stats
heroStats p =
  currentIdentity <$> getPlayer p >>= \case
    HeroSide h -> Stats <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
    AlterEgoSide _ -> pure (Stats 0 0 0)

allyAttackNow, allyThwartNow :: CardCode -> GameAppT Natural
allyAttackNow code = Ally.getModifiedAttack =<< allyIn code
allyThwartNow code = Ally.getModifiedThwart =<< allyIn code

costOf :: IdentityId -> PlayerCard -> GameAppT Int
costOf p card = do
  pl <- getPlayer p
  getModifiedCost pl card Nothing

setAllyLimit :: IdentityId -> Natural -> GameAppT ()
setAllyLimit p n = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentityAllyLimit = n}

-- | Pay with these cards and exhaust these allies, in whatever order the cost asks,
-- finishing the payment when offered; stops at the first other question.
payAll :: [PlayerCard] -> [AllyId] -> GameAppT ()
payAll = go (20 :: Int)
 where
  go 0 _ _ = pure ()
  go n cs xs = do
    chooseIf (isLabelStarting "$label.spendResourceAllocation")
    cards <- filterM (offers . isPayWith) cs
    allies <- filterM (offers . isTargetChoice) xs
    finish <- offers isFinishPayment
    case (cards, allies) of
      (c : _, _) -> choose "pay" (isPayWith c) >> go (n - 1) (filter (/= c) cs) xs
      ([], a : _) -> choose "exhaust an ally" (isTargetChoice a) >> go (n - 1) cs (filter (/= a) xs)
      _ | finish -> choose "finish paying" isFinishPayment >> go (n - 1) cs xs
      _ -> pure ()

spec :: Spec
spec = describe "The Mad Titan's Shadow: Leadership and basic cards" do
  describe "Captain America, Steve Rogers (21011)" do
    it "costs 1 less for each Avenger character you control, your identity in its form included" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        cap <- putInHand p "21011"
        -- Monica Rambeau is a Civilian
        costOf p cap `shouldReturn` 6
        _ <- putIntoPlay p "21019"
        _ <- putIntoPlay p "21012"
        costOf p cap `shouldReturn` 4
        -- Mockingbird is no Avenger
        _ <- putIntoPlay p "01083"
        costOf p cap `shouldReturn` 4
        asSpectrum p "21002"
        cap' <- putInHand p "21011"
        costOf p cap' `shouldReturn` 3

    it "has toughness" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "21011"
        (.allyTough) <$> allyIn "21011" `shouldReturn` True

  describe "Power Man, Victor Alvarez (21012)" do
    it "enters play with 2 chi counters" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "21012"
        (.allyCounters) <$> allyIn "21012" `shouldReturn` 2
        allyAttackNow "21012" `shouldReturn` 0

    it "discards both counters for +4 ATK until the end of the phase, then cannot use it again" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        pm <- putIntoPlay p "21012"
        choose "Power Man" (isResponseOf pm)
        offers (isLabel (countLabel "discardCounters" 1)) `shouldReturn` True
        choose "discard 2" (isLabel (countLabel "discardCounters" 2))
        (.allyCounters) <$> allyIn "21012" `shouldReturn` 0
        allyAttackNow "21012" `shouldReturn` 4
        offers (isResponseOf pm) `shouldReturn` False
        endTurn
        pass
        allyAttackNow "21012" `shouldReturn` 0

    it "discards 1 counter for +2 ATK, and may use the other later" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        pm <- putIntoPlay p "21012"
        choose "Power Man" (isResponseOf pm)
        choose "discard 1" (isLabel (countLabel "discardCounters" 1))
        (.allyCounters) <$> allyIn "21012" `shouldReturn` 1
        allyAttackNow "21012" `shouldReturn` 2
        choose "Power Man again" (isResponseOf pm)
        offers (isLabel (countLabel "discardCounters" 2)) `shouldReturn` False
        choose "discard 1" (isLabel (countLabel "discardCounters" 1))
        allyAttackNow "21012" `shouldReturn` 4

  describe "White Tiger, Ava Ayala (21013)" do
    it "played from hand, draws as many cards as the villain's stage: Rhino I, 1" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        wt <- putInHand p "21013"
        playPaying p wt ["01088", "01089"]
        choose "White Tiger" (isResponseOf wt)
        length <$> handOf p `shouldReturn` 1

    it "against Rhino II draws 2" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainDamaged (toSource p) (toDamage 14 FromAbility))]
        field V.VillainStage <$> getVillain `shouldReturn` 2
        clearHand p
        wt <- putInHand p "21013"
        playPaying p wt ["01088", "01089"]
        choose "White Tiger" (isResponseOf wt)
        length <$> handOf p `shouldReturn` 2

    it "put into play, not played, draws nothing" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        wt <- putIntoPlay p "21013"
        offers (isResponseOf wt) `shouldReturn` False
        length <$> handOf p `shouldReturn` 0

  describe "Kaluu (21014)" do
    it "searches the top 5 cards of the deck for an event as he enters play" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        [_, massAttack] <- stackDeck p ["21021", "21016"]
        kaluu <- putInHand p "21014"
        playPaying p kaluu ["01088"]
        choose "Kaluu" (isResponseOf kaluu)
        choose "Mass Attack" (isTargetChoice massAttack)
        map toCardCode <$> handOf p `shouldReturn` ["21016"]

  describe "Mighty Avengers (21015)" do
    it "gives each ally +1 THW and +1 ATK while each of your characters is an Avenger" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        asSpectrum p "21003"
        _ <- putIntoPlay p "21019"
        _ <- putIntoPlay p "21015"
        allyThwartNow "21019" `shouldReturn` 2
        allyAttackNow "21019" `shouldReturn` 3

    it "gives nothing while your identity is no Avenger (Monica Rambeau)" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "21019"
        _ <- putIntoPlay p "21015"
        allyThwartNow "21019" `shouldReturn` 1
        allyAttackNow "21019" `shouldReturn` 2

    it "gives nothing while an ally you control is no Avenger" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        asSpectrum p "21003"
        _ <- putIntoPlay p "21019"
        _ <- putIntoPlay p "01083"
        _ <- putIntoPlay p "21015"
        allyThwartNow "21019" `shouldReturn` 1
        allyAttackNow "21019" `shouldReturn` 2

    it "is played for 3, at most 1 Team per player" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        ma <- putInHand p "21015"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playOn ma p payment
        length <$> supportsWith "21015" `shouldReturn` 1
        second' <- putInHand p "21015"
        _ <- traverse (putInHand p) ["01088", "01089"]
        offers (isPlay second') `shouldReturn` False

    it "a second goes under another player's control" do
      tg <- testGame ["spectrum", "captain-america"]
      let (p, other) = (player1 tg, player2 tg)
      inGame tg do
        _ <- putIntoPlay p "21015"
        clearHand p
        ma <- putInHand p "21015"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playOn ma other payment
        map (.supportController) <$> supportsWith "21015" >>= (`shouldMatchList` [p, other])

  describe "Mass Attack (21016)" do
    it "deals the total ATK of 3 allies sharing a trait with your hero and your hero's" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        asSpectrum p "21002"
        blade <- allyIdOf <$> putIntoPlay p "21019"
        kaluu <- allyIdOf <$> putIntoPlay p "21014"
        pm <- allyIdOf <$> putIntoPlay p "21012"
        clearHand p
        attack <- putInHand p "21016"
        payment <- traverse (putInHand p) ["01090", "01088"]
        choose "Mass Attack" (isPlay attack)
        payAll payment [blade, kaluu, pm]
        chooseVillain
        pass
        -- Blade 2 + Kaluu 1 + Power Man 0 + Spectrum in Gamma form 3
        villainDamageNow `shouldReturn` 6
        traverse (fmap (.allyExhausted) . allyIn) ["21019", "21014", "21012"] `shouldReturn` [True, True, True]

    it "cannot be played with only 2 allies sharing a trait with your hero" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        asSpectrum p "21002"
        _ <- putIntoPlay p "21019"
        _ <- putIntoPlay p "21014"
        _ <- putIntoPlay p "01083"
        clearHand p
        attack <- putInHand p "21016"
        _ <- traverse (putInHand p) ["01090", "01088"]
        offers (isPlay attack) `shouldReturn` False

    it "does not offer an ally sharing no trait with your hero" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        asSpectrum p "21002"
        setAllyLimit p 4
        blade <- allyIdOf <$> putIntoPlay p "21019"
        _ <- putIntoPlay p "21014"
        _ <- putIntoPlay p "21012"
        mockingbird <- allyIdOf <$> putIntoPlay p "01083"
        clearHand p
        attack <- putInHand p "21016"
        payment <- traverse (putInHand p) ["01090", "01032"]
        choose "Mass Attack" (isPlay attack)
        payAll payment []
        offers (isTargetChoice blade) `shouldReturn` True
        offers (isTargetChoice mockingbird) `shouldReturn` False

  describe "Moxie (21017)" do
    it "answers Energy Transformation's energy form change" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        moxie <- putInHand p "21017"
        toSpectrum p "21002"
        choose "Moxie" (isPlay moxie)
        payWith []
        pass
        -- Spectrum 1/1/1, Gamma +2 ATK, Moxie +1 each
        heroStats p `shouldReturn` Stats 2 4 2

    it "answers the flip to Spectrum, after the energy form's window" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        moxie <- putInHand p "21017"
        toSpectrum p "21002"
        choose "no response to the energy form change" isNoResponse
        choose "Moxie" (isPlay moxie)
        payWith []
        pass
        heroStats p `shouldReturn` Stats 2 4 2

  describe "Band Together (21018)" do
    it "generates nothing with no ally" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        kaluu <- putInHand p "21014"
        _ <- putInHand p "21018"
        offers (isPlay kaluu) `shouldReturn` False

    it "generates a wild resource for each of 2 allies" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "21019"
        _ <- putIntoPlay p "21012"
        clearHand p
        kaluu <- putInHand p "21014"
        playPaying p kaluu ["21018"]
        length <$> alliesWith "21014" `shouldReturn` 1

    it "generates at most 3, with 4 allies" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        setAllyLimit p 4
        traverse_ (putIntoPlay p) ["21019", "21012", "21014", "01083"]
        clearHand p
        -- Avengers Mansion costs 4
        mansion <- putInHand p "21021"
        _ <- putInHand p "21018"
        offers (isPlay mansion) `shouldReturn` False
        clearHand p
        ma <- putInHand p "21015"
        band <- putInHand p "21018"
        playOn ma p [band]
        length <$> supportsWith "21015" `shouldReturn` 1

  describe "Blade, Eric Brooks (21019)" do
    it "after he attacks, a physical resource from hand is spent to keep him" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        blade <- putIntoPlay p "21019"
        strength <- putInHand p "01090"
        bladeAttrs <- allyIn "21019"
        choose "Blade attacks" (isAllyAttack bladeAttrs)
        chooseIf . isTargetChoice =<< villainIdNow
        choose "Blade's forced response" (isResponseOf blade)
        choose "spend a physical resource" (isLabel "$label.spendPhysicalFromHand")
        choose "pay with Strength" (isPayWith strength)
        chooseIf isFinishPayment
        pass
        length <$> alliesWith "21019" `shouldReturn` 1
        map pcCardId <$> discardOf p >>= (`shouldSatisfy` elem (pcCardId strength))

    it "may be discarded instead even with a physical resource in hand" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        blade <- putIntoPlay p "21019"
        _ <- putInHand p "01090"
        bladeAttrs <- allyIn "21019"
        choose "Blade attacks" (isAllyAttack bladeAttrs)
        chooseIf . isTargetChoice =<< villainIdNow
        choose "Blade's forced response" (isResponseOf blade)
        choose "discard Blade" (isLabel "$label.discardBlade")
        pass
        alliesWith "21019" `shouldReturn` []
        length <$> handOf p `shouldReturn` 1

    it "after he thwarts with no physical resource in hand, he is discarded: a resource upgrade in play does not pay" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        asSpectrum p "21003"
        placeMainThreat 3
        physique <- putIntoPlay p "06034"
        blade <- putIntoPlay p "21019"
        bladeAttrs <- allyIn "21019"
        choose "Blade thwarts" (isAllyThwart bladeAttrs)
        chooseIf . isTargetChoice =<< mainSchemeIdNow
        choose "Blade's forced response" (isResponseOf blade)
        offers (isLabel "$label.spendPhysicalFromHand") `shouldReturn` False
        pass
        alliesWith "21019" `shouldReturn` []
        map pcCardId <$> discardOf p >>= (`shouldSatisfy` elem (pcCardId blade))
        upgradeIsExhausted (upgradeIdOf physique) `shouldReturn` False

  describe "Ready to Rumble (21022)" do
    it "after flipping to hero form, is discarded to ready your hero" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        clearHand p
        rtr <- putInHand p "21022"
        energy <- putInHand p "01088"
        playOn rtr p [energy]
        resolve [IdentityMessage p ExhaustedIdentity]
        isExhaustedNow p `shouldReturn` True
        toSpectrum p "21002"
        -- the energy form's window first; it answers the flip's
        choose "no response to the energy form change" isNoResponse
        choose "Ready to Rumble" (isResponseOf rtr)
        pass
        isExhaustedNow p `shouldReturn` False
        length <$> upgradesWith "21022" `shouldReturn` 0
        map pcCardId <$> discardOf p >>= (`shouldSatisfy` elem (pcCardId rtr))

    it "after an energy form change, readies your hero" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        rtr <- putIntoPlay p "21022"
        asSpectrum p "21002"
        resolve [IdentityMessage p ExhaustedIdentity]
        speed <- putInHand p "21010"
        playWith speed []
        photon <- energyForm "21003"
        choose "Photon" (isTargetChoice photon)
        choose "Ready to Rumble" (isResponseOf rtr)
        pass
        isExhaustedNow p `shouldReturn` False
        length <$> upgradesWith "21022" `shouldReturn` 0

    it "is not offered after changing to alter-ego form" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        asSpectrum p "21002"
        rtr <- putIntoPlay p "21022"
        endTurn
        pass
        changeForm p
        chooseIf (isLabeledAbility "Power Down")
        offers (isResponseOf rtr) `shouldReturn` False
        length <$> upgradesWith "21022" `shouldReturn` 1

    it "under another player's control, answers their change of form, not yours" do
      tg <- testGame ["spectrum", "captain-america"]
      let p = player1 tg
          p2 = player2 tg
      inGame tg do
        clearHand p
        rtr <- putInHand p "21022"
        energy <- putInHand p "01088"
        playOn rtr p2 [energy]
        resolve [IdentityMessage p ExhaustedIdentity, IdentityMessage p2 ExhaustedIdentity]
        -- Spectrum's flip and energy form change: not hers to answer
        toSpectrum p "21002"
        offers (isResponseOf rtr) `shouldReturn` False
        pass
        offers (isResponseOf rtr) `shouldReturn` False
        pass
        isExhaustedNow p `shouldReturn` True
        length <$> upgradesWith "21022" `shouldReturn` 1
        -- Captain America flips: it answers him
        runNow [IdentityMessage p2 (ChangedToForm A)]
        choose "Ready to Rumble" (isResponseOf rtr)
        pass
        isExhaustedNow p2 `shouldReturn` False
        isExhaustedNow p `shouldReturn` True
        length <$> upgradesWith "21022" `shouldReturn` 0

    it "is max 1 per player" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "21022"
        clearHand p
        rtr <- putInHand p "21022"
        _ <- putInHand p "01088"
        offers (isPlay rtr) `shouldReturn` False

  describe "reprints in the starter deck" do
    it "lists Avengers Tower, Avengers Mansion, Moxie, Energy, Genius and Strength under their MTS codes" do
      let codes = map fst (starter "spectrum").cards
          reprints = ["21017", "21020", "21021", "21023", "21024", "21025"] :: [CardCode]
      for_ reprints \c -> codes `shouldSatisfy` elem c
      map (canonicalCardCode . lookupPlayerCard) reprints
        `shouldBe` (["12016", "03024", "01091", "01088", "01089", "01090"] :: [CardCode]) :: IO ()

    it "Avengers Tower (21020) and Avengers Mansion (21021) enter play as the originals" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "21020"
        _ <- putIntoPlay p "21021"
        length <$> supportsWith "21020" `shouldReturn` 1
        length <$> supportsWith "21021" `shouldReturn` 1

    it "Energy (21023), Genius (21024) and Strength (21025) each pay 2 resources" do
      tg <- spectrum
      let p = player1 tg
      inGame tg do
        for_ [("21023", "21014"), ("21024", "21019"), ("21025", "21012")] \(resource, ally') -> do
          clearHand p
          card <- putInHand p ally'
          -- Blade costs 1; the others 2 and 3, Strength paying 2 of Power Man's 3
          if ally' == "21012"
            then playPaying p card [resource, "01088"]
            else playPaying p card [resource]
          pass
          length <$> alliesWith ally' `shouldReturn` 1
