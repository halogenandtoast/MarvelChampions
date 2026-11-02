module Marvel.Encounter.BlackPantherShuriNemesisSpec (spec) where
import TestImport
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Query

spec :: Spec
spec = describe "Black Panther Shuri nemesis and Extreme Risk" do
  it "The Scream stuns each controlled character" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.theScreamShuri
      pass
      isStunned p `shouldReturn` True
      damageOf p `shouldReturn` 0

  it "The Scream damages an already stunned identity without unstunning it" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      resolve [IdentityMessage p IdentityStunned]
      _ <- revealNow p Cards.theScreamShuri
      pass
      isStunned p `shouldReturn` True
      damageOf p `shouldReturn` 1

  it "M.U.S.I.C. finds Manipulated M.U.S.I.C. when revealed" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Cards.manipulatedMUSICShuri] []
      _ <- revealNow p Cards.mUSICShuri
      pass
      selectAny (SideSchemeIs Cards.manipulatedMUSICShuri) `shouldReturn` True

  it "Manipulated M.U.S.I.C. finds and engages its captive" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Cards.mUSICShuri] []
      _ <- revealNow p Cards.manipulatedMUSICShuri
      pass
      selectAny (MinionIs Cards.mUSICShuri <> MinionEngagedWith (IdentityWithId p)) `shouldReturn` True

  it "Playing for Keeps increases each identity's hand size" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.playingforKeepsShuri
      pass
      handSizeOf p `shouldReturn` 7

  it "T'Challa's Shadow starts with four doubt counters and remains in play" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      card <- revealNow p Cards.tChallasShadowShuri
      pass
      let oid = ObligationId $ unCardId $ ecCardId card
      obligationsInPlay `shouldReturn` 1
      getCountersOn (ObligationRef oid) `shouldReturn` 4

  it "Klaw receives and resolves an explicit boost card despite lacking Villainous" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      mid <- addMinion p Cards.klawShuri
      _ <- setEncounterDeck [Treacheries.explosion,Treacheries.assault] []
      runNow [MinionMessage mid (MinionAttacks p)]
      choose "Klaw's boost interrupt" (isAbilityOf mid)
      choose "no defender" isNoDefender
      pass
      damageOf p `shouldReturn` 2

  it "Joystick can gain an additional activation boost and let the attacked player draw" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      mid <- addMinion p Cards.joystickShuri
      _ <- setEncounterDeck [Treacheries.explosion,Treacheries.explosion,Treacheries.assault] []
      runNow [MinionMessage mid (MinionAttacks p)]
      choose "Joystick activation interrupt" (isAbilityOf mid)
      choose "extra boost and draw" (isLabel "$label.bpRiskBoost")
      choose "no defender" isNoDefender
      pass
      (length <$> handOf p) `shouldReturn` 1
      damageOf p `shouldReturn` 5

  it "Energy Truncheon's action makes its host attack, then discards it and draws a card" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      card <- revealNow p Cards.energyTruncheonShuri
      pass
      _ <- setEncounterDeck [Treacheries.explosion,Treacheries.assault] []
      let aid = AttachmentId $ unCardId $ ecCardId card
      choose "Energy Truncheon action" (isAbilityOf aid)
      choose "no defender" isNoDefender
      pass
      selectAny (AttachmentWithId aid) `shouldReturn` False
      (length <$> handOf p) `shouldReturn` 1
      damageOf p `shouldReturn` 5

  it "Extreme Risk makes an existing Joystick engage and activate against its revealer" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      mid <- addMinion p Cards.joystickShuri
      _ <- setEncounterDeck [Treacheries.explosion,Treacheries.assault] []
      _ <- revealNow p Cards.extremeRiskShuri
      choose "Joystick activation interrupt" (isAbilityOf mid)
      choose "give Joystick tough" (isLabel "$label.giveTough")
      choose "no defender" isNoDefender
      pass
      selectAny (MinionWithId mid <> MinionEngagedWith (IdentityWithId p)) `shouldReturn` True
      damageOf p `shouldReturn` 3

  it "T'Challa's Shadow increases card costs and enters victory after four basic powers" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      obligation <- revealNow p Cards.tChallasShadowShuri
      pass
      let oid = ObligationId $ unCardId $ ecCardId obligation
      strike <- putInHand p "51003"
      _ <- putInHand p "01088"
      offers (isPlay strike) `shouldReturn` False
      for_ [1..4 :: Int] $ \n -> do
        choose "basic attack" (isHeroAttack p)
        chooseVillain
        choose "remove doubt" (isAbilityOf (ObligationRef oid))
        pass
        when (n < 4) $ resolve [IdentityMessage p ReadiedIdentity]
      obligationsInPlay `shouldReturn` 0
      victory <- getVictoryDisplay
      victory `shouldSatisfy` elem obligation
