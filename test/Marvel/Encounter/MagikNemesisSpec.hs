module Marvel.Encounter.MagikNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as Scheme
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

rulerAttrs :: GameAppT (Attrs Scheme.SideScheme)
rulerAttrs = getsGame (toAttrs . fromMaybe (error "Ruler of Limbo missing") . headMay . filter ((== "45055") . toCardCode . getCardDef) . HashMap.elems . gameSideSchemes)

revealRuler :: IdentityId -> GameAppT EncounterCard
revealRuler p = do
  card <- revealNow p Schemes.rulerOfLimbo
  pass
  pure card

spec :: Spec
spec = describe "Magik nemesis" do
  it "Darkchilde can be removed by exhausting Illyana in alter-ego form" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Obligations.darkchilde
      choose "exhaust Illyana and remove the obligation" (isLabel "$label.magikExhaustIllyanaRemoveDarkchilde")
      isExhaustedPlayer p `shouldReturn` True
      damageOf p `shouldReturn` 0

  it "Darkchilde's other option damages every controlled character and discards itself" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      changeForm p
      triage <- putIntoPlay p "45048"
      card <- revealNow p Obligations.darkchilde
      choose "remain Magik" (isLabel "$label.doNotFlipToAlterEgo")
      choose "damage every controlled character" (isLabel "$label.magikDarkchildeDamageCharacters")
      pass
      damageOf p `shouldReturn` 1
      allyDamage <$> allyIn "45048" `shouldReturn` 1
      encounterDiscard >>= (`shouldSatisfy` elem card)
      toCardCode triage `shouldBe` "45048"

  it "an exhausted Illyana cannot choose Darkchilde's exhaust option" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      resolve [IdentityMessage p ExhaustedIdentity]
      _ <- revealNow p Obligations.darkchilde
      offers (isLabel "$label.magikExhaustIllyanaRemoveDarkchilde") `shouldReturn` False
      choose "take the damage" (isLabel "$label.magikDarkchildeDamageCharacters")
      damageOf p `shouldReturn` 1

  it "Ruler of Limbo finds Limbo in play, holds it facedown, and returns it when defeated" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      limbo <- putIntoPlay p "45032"
      ruler <- revealRuler p
      supportsWith "45032" `shouldReturn` []
      held <- Scheme.sideSchemeHeldCards <$> rulerAttrs
      held `shouldBe` [limbo]
      Scheme.sideSchemeThreat <$> rulerAttrs `shouldReturn` 3
      resolve [SideSchemeMessage (sideSchemeOf ruler) (SideSchemeThreatRemoved (IdentityRef p) 3)]
      [restored] <- supportsWith "45032"
      supportOwner restored `shouldBe` p
      supportExhausted restored `shouldBe` False

  it "Ruler of Limbo finds the physical Limbo card in its owner's deck" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      clearHand p
      [limbo] <- stackDeck p ["45032"]
      _ <- revealRuler p
      Scheme.sideSchemeHeldCards <$> rulerAttrs >>= (`shouldSatisfy` elem limbo)
      unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` notElem limbo)

  it "Ruler prevents all threat removal while Belasco is in play" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      ruler <- revealRuler p
      belasco <- addMinion p Minions.belasco
      resolve [SideSchemeMessage (sideSchemeOf ruler) (SideSchemeThreatRemoved (IdentityRef p) 3)]
      Scheme.sideSchemeThreat <$> rulerAttrs `shouldReturn` 3
      resolve [RemoveFromPlay $ MinionRef belasco]
      resolve [SideSchemeMessage (sideSchemeOf ruler) (SideSchemeThreatRemoved (IdentityRef p) 3)]
      getsGame (HashMap.member (sideSchemeOf ruler) . gameSideSchemes) `shouldReturn` False

  it "Belasco discards three cards after an activation and attaches them to Ruler" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      _ <- revealRuler p
      belasco <- addMinion p Minions.belasco
      cards <- stackDeck p ["01086", "01087", "01093"]
      resolve [MinionMessage belasco MinionSchemes]
      held <- Scheme.sideSchemeHeldCards <$> rulerAttrs
      held `shouldSatisfy` (\xs -> all (`elem` xs) cards)
      discarded <- discardOf p
      discarded `shouldSatisfy` (\xs -> all (`notElem` xs) cards)

  it "Belasco leaves discarded cards in the discard pile without Ruler" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      belasco <- addMinion p Minions.belasco
      cards <- stackDeck p ["01086", "01087", "01093"]
      resolve [MinionMessage belasco MinionSchemes]
      discardOf p >>= (`shouldSatisfy` (\xs -> all (`elem` xs) cards))

  for_ [False, True] \withRuler ->
    it ("S'ym places two threat on " <> if withRuler then "Ruler" else "the main scheme") do
      tg <- testGame ["magik"]
      inGame tg do
        let p = player1 tg
        when withRuler $ void $ revealRuler p
        before <- mainSchemeThreatNow
        _ <- addMinion p Minions.sym
        if withRuler then do
          Scheme.sideSchemeThreat <$> rulerAttrs `shouldReturn` 5
          mainSchemeThreatNow `shouldReturn` before
        else mainSchemeThreatNow `shouldReturn` before + 2

  it "Witchfire's quickstrike attacks a hero as she engages" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      changeForm p
      _ <- addMinion p Minions.witchfire
      damageOf p `shouldReturn` 3

  for_ [False, True] \withRuler ->
    it ("Witchfire places threat after defeating an ally, with Ruler " <> show withRuler) do
      tg <- testGame ["magik"]
      inGame tg do
        let p = player1 tg
        when withRuler $ void $ revealRuler p
        witchfire <- addMinion p Minions.witchfire
        ally <- putIntoPlay p "45048"
        before <- mainSchemeThreatNow
        resolve [MinionMessage witchfire (MinionAttacksAlly p $ allyIdOf ally)]
        alliesWith "45048" `shouldReturn` []
        if withRuler then do
          Scheme.sideSchemeThreat <$> rulerAttrs `shouldReturn` 4
          mainSchemeThreatNow `shouldReturn` before
        else mainSchemeThreatNow `shouldReturn` before + 1

  it "Battle for Limbo activates every Limbo minion against its engaged player" do
    tg <- testGame ["magik", "spider-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      _ <- addMinion p Minions.sym
      _ <- addMinion other Minions.witchfire
      before <- mainSchemeThreatNow
      card <- encounterCard Treacheries.battleForLimbo
      resolve [RevealEncounterCard p card]
      mainSchemeThreatNow `shouldReturn` before + 3

  it "Battle for Limbo surges when no Limbo minion can activate" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      [surged] <- stackEncounterDeck [Minions.hydraRegular]
      card <- encounterCard Treacheries.battleForLimbo
      resolve [RevealEncounterCard p card]
      playerIdentityEncounterCards <$> playerAttrs p >>= (`shouldSatisfy` elem surged)
