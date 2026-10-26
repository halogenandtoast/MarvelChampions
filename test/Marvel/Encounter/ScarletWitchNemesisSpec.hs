module Marvel.Encounter.ScarletWitchNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card.Side (Side (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

scarletWitch :: IO TestGame
scarletWitch = testGame ["scarlet-witch"]

-- | A hero with no power answering villain attacks or boost icons.
thor :: IO TestGame
thor = testGame ["thor"]

sideOf :: IdentityId -> GameAppT Side
sideOf p = (.playerIdentitySide) <$> playerAttrs p

exhaustWanda, discardFive :: Choice -> Bool
exhaustWanda = isLabel "$label.exhaustWandaMaximoffRemoveSlippingSanity"
discardFive = isLabel (countLabel "discardEncounterCardsThreatPerStar" 5)

-- | Luminous in play, and who she is engaged with.
luminousInPlay :: GameAppT [IdentityId]
luminousInPlay =
  getsGame
    ( map minionEngagedIdentity
        . filter ((== "15025") . toCardCode . minionCardDef)
        . map toAttrs
        . HashMap.elems
        . gameMinions
    )

-- | Sonic Boom: a star, no boost icon. Hydra Mercenary: 1 boost icon. Whiplash: 2.
noIcon, oneIcon, twoIcons :: CardDef
noIcon = Treacheries.sonicBoom
oneIcon = Minions.hydraMercenary
twoIcons = Minions.whiplash

-- | The enemy attacks the player, undefended, its forced responses resolved.
attackedBy :: MinionId -> IdentityId -> GameAppT ()
attackedBy m p = do
  start [MinionMessage m (MinionAttacks p)]
  choose "no defender" isNoDefender
  chooseIf isAnyAbility
  pass

spec :: Spec
spec = describe "Scarlet Witch's nemesis set" do
  describe "Slipping Sanity (15023)" do
    it "is given to the Wanda Maximoff player" do
      tg <- testGame ["thor", "scarlet-witch"]
      let p1 = player1 tg
          p2 = player2 tg
      inGame tg do
        _ <- revealNow p1 Obligations.slippingSanity
        fmap fst <$> asked `shouldReturn` Just p2

    it "exhausting Wanda Maximoff removes it from the game" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.slippingSanity
        offers discardFive `shouldReturn` True
        choose "exhaust Wanda" exhaustWanda
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "15023")

    it "a hero may flip to alter-ego to exhaust Wanda Maximoff" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        _ <- revealNow p Obligations.slippingSanity
        choose "flip to alter-ego" (isLabel "$label.changeToB")
        sideOf p `shouldReturn` B
        choose "exhaust Wanda" exhaustWanda
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "15023")

    it "otherwise discards the top 5 encounter cards: 1 threat for each star among them" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        _ <- setEncounterDeck [noIcon, twoIcons, noIcon, oneIcon, oneIcon, twoIcons] []
        before <- mainSchemeThreatNow
        _ <- revealNow p Obligations.slippingSanity
        choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
        -- the hero cannot exhaust Wanda Maximoff: the other option resolves
        pass
        sideOf p `shouldReturn` A
        isExhaustedNow p `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` (before + 2)
        map toCardCode <$> scenarioEncounterDeckNow `shouldReturn` ["01172"]
        encounterDiscardCodes >>= (`shouldSatisfy` elem "15023")
        obligationsInPlay `shouldReturn` 0

    it "an exhausted Wanda Maximoff cannot be exhausted: the cards are discarded" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- setEncounterDeck [noIcon, oneIcon, oneIcon, oneIcon, oneIcon] []
        before <- mainSchemeThreatNow
        _ <- revealNow p Obligations.slippingSanity
        offers exhaustWanda `shouldReturn` False
        pass
        mainSchemeThreatNow `shouldReturn` (before + 1)
        encounterDiscardCodes >>= (`shouldSatisfy` elem "15023")

    it "The Next Evolution does not add stars" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow p SideSchemes.theNextEvolution
        pass
        _ <- setEncounterDeck [noIcon, oneIcon, oneIcon, twoIcons, twoIcons] []
        before <- mainSchemeThreatNow
        _ <- revealNow p Obligations.slippingSanity
        pass
        mainSchemeThreatNow `shouldReturn` (before + 1)

  describe "The Next Evolution (15024)" do
    it "a villain's boost card gives 1 more boost icon" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        vid <- villainIdNow
        _ <- stackEncounterDeck [oneIcon]
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        pass
        without <- damageOf p
        setDamage p 0
        _ <- revealNow p SideSchemes.theNextEvolution
        pass
        _ <- stackEncounterDeck [oneIcon]
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        pass
        damageOf p `shouldReturn` (without + 1)

    it "each card a count discards has 1 more boost icon (Lightning Bolt)" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        _ <- revealNow p SideSchemes.theNextEvolution
        pass
        _ <- stackEncounterDeck [twoIcons, oneIcon]
        _ <- revealNow p Treacheries.lightningBolt
        pass
        damageOf p `shouldReturn` 5

  describe "Luminous (15025)" do
    it "after she attacks you, 2 or more boost icons on the top encounter card deal you an encounter card" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        m <- addMinion p Minions.luminous
        _ <- stackEncounterDeck [twoIcons]
        dealt <- length <$> dealtTo p
        m `attackedBy` p
        damageOf p `shouldReturn` 2
        length <$> dealtTo p `shouldReturn` dealt + 1
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01172")

    it "fewer than 2 boost icons deal nothing" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        m <- addMinion p Minions.luminous
        _ <- stackEncounterDeck [oneIcon]
        dealt <- length <$> dealtTo p
        m `attackedBy` p
        length <$> dealtTo p `shouldReturn` dealt
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01101")

    it "after she schemes, the player she is engaged with is dealt the card" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        m <- addMinion p Minions.luminous
        _ <- stackEncounterDeck [twoIcons]
        dealt <- length <$> dealtTo p
        before <- mainSchemeThreatNow
        resolve [MinionMessage m MinionSchemes]
        mainSchemeThreatNow `shouldReturn` (before + 2)
        length <$> dealtTo p `shouldReturn` dealt + 1

    it "The Next Evolution makes a single boost icon enough" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        _ <- revealNow p SideSchemes.theNextEvolution
        pass
        m <- addMinion p Minions.luminous
        _ <- stackEncounterDeck [oneIcon]
        dealt <- length <$> dealtTo p
        resolve [MinionMessage m MinionSchemes]
        length <$> dealtTo p `shouldReturn` dealt + 1

  describe "Magical Suspension (15026)" do
    it "attaches to your identity: each card you play costs 1 more" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- revealNow p Attachments.magicalSuspension
        pass
        maria <- putInHand p "01067"
        -- Warning generates a single resource: Maria Hill's 2 and 1 more need three
        payment <- traverse (putInHand p) ["09021", "09021"]
        offers (isPlay maria) `shouldReturn` False
        third <- putInHand p "09021"
        playWith maria (third : payment)
        pass
        length <$> alliesWith "01067" `shouldReturn` 1

    it "Hero Action: exhaust your hero to discard it" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- revealNow p Attachments.magicalSuspension
        pass
        let attachment = AttachmentId $ unCardId $ ecCardId card
        -- not in alter-ego form
        offers (isAbilityOf attachment) `shouldReturn` False
        changeForm p >> pass
        choose "discard it" (isAbilityOf attachment)
        pass
        isExhaustedNow p `shouldReturn` True
        encounterDiscardCodes >>= (`shouldSatisfy` elem "15026")
        -- gone: Maria Hill costs her 2 again
        maria <- putInHand p "01067"
        payment <- traverse (putInHand p) ["09021", "09021"]
        playWith maria payment
        pass
        length <$> alliesWith "01067" `shouldReturn` 1

  describe "Chaos Manipulation (15027)" do
    it "puts Luminous into play engaged with you from the discard pile" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [oneIcon, noIcon] [Minions.luminous]
        before <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.chaosManipulation
        pass
        luminousInPlay `shouldReturn` [p]
        -- fewer than 2 boost icons: she does not activate
        mainSchemeThreatNow `shouldReturn` before
        length <$> encounterDiscardNow `shouldReturn` 2

    it "finds Luminous in the encounter deck" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        -- two left: a deck emptied by the discard is refilled from the discard pile
        _ <- setEncounterDeck [Minions.luminous, oneIcon, oneIcon] []
        _ <- revealNow p Treacheries.chaosManipulation
        pass
        luminousInPlay `shouldReturn` [p]
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01101")
        map toCardCode <$> scenarioEncounterDeckNow `shouldReturn` ["01101"]

    it "2 or more boost icons: Luminous already in play attacks your hero" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        _ <- addMinion p Minions.luminous
        -- the search shuffles the deck: every card the same
        _ <- setEncounterDeck [twoIcons, twoIcons, twoIcons] []
        dealt <- length <$> dealtTo p
        _ <- revealNow p Treacheries.chaosManipulation
        choose "no defender" isNoDefender
        chooseIf isAnyAbility
        pass
        luminousInPlay `shouldReturn` [p]
        damageOf p `shouldReturn` 2
        -- and her forced response follows
        length <$> dealtTo p `shouldReturn` dealt + 1

    it "2 or more boost icons: Luminous schemes against your alter-ego" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [twoIcons, twoIcons, twoIcons] [Minions.luminous]
        before <- mainSchemeThreatNow
        dealt <- length <$> dealtTo p
        _ <- revealNow p Treacheries.chaosManipulation
        chooseIf isAnyAbility
        pass
        mainSchemeThreatNow `shouldReturn` (before + 2)
        damageOf p `shouldReturn` 0
        length <$> dealtTo p `shouldReturn` dealt + 1

    it "fewer than 2 boost icons: Luminous in play does not activate" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        _ <- addMinion p Minions.luminous
        _ <- setEncounterDeck [oneIcon, noIcon] []
        before <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.chaosManipulation
        pass
        damageOf p `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` before
