module Marvel.Encounter.NightcrawlerNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Boost qualified as B
import Marvel.Keyword
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

spec :: Spec
spec = describe "Nightcrawler nemesis and Crazy Gang" do
  it "another player's Crisis of Faith reveal is assigned to Kurt Wagner" do
    tg <- testGame ["nightcrawler", "spider-man"]
    inGame tg do
      let kurt = player1 tg
          other = player2 tg
      _ <- revealNow other Obligations.nc48026
      choose "Kurt exhausts to remove his obligation" (isLabel "$label.crisisOfFaithExhaust")
      pass
      isExhaustedPlayer kurt `shouldReturn` True
      isExhaustedPlayer other `shouldReturn` False

  it "Crisis of Faith can be removed by exhausting Kurt Wagner" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Obligations.nc48026
      choose "exhaust Kurt" (isLabel "$label.crisisOfFaithExhaust")
      pass
      isExhaustedPlayer p `shouldReturn` True

  it "Crisis of Faith discards Attack and Defense events but leaves Superpower-only events and upgrades" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      clearHand p
      attack <- putInHand p "48007"
      defense <- putInHand p "48011"
      other <- putInHand p "48010"
      upgrade <- putInHand p "48006"
      card <- revealNow p Obligations.nc48026
      choose "discard Attack and Defense events" (isLabel "$label.crisisOfFaithDiscard")
      pass
      handOf p `shouldReturn` [upgrade,other]
      discarded <- discardOf p
      discarded `shouldSatisfy` (\cs -> attack `elem` cs && defense `elem` cs)
      encounterDiscard >>= (`shouldSatisfy` elem card)

  it "an exhausted Kurt cannot choose the obligation's exhaust option" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      resolve [IdentityMessage p ExhaustedIdentity]
      _ <- revealNow p Obligations.nc48026
      offers (isLabel "$label.crisisOfFaithExhaust") `shouldReturn` False
      choose "discard matching events" (isLabel "$label.crisisOfFaithDiscard")
      pass

  it "Azazel has Quickstrike and cannot have player upgrades attached" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      azazel <- revealNow p Minions.nc48027
      pass
      damageOf p `shouldReturn` 3
      let mid = MinionId $ unCardId $ ecCardId azazel
      getModifiers mid >>= (`shouldSatisfy` elem CannotHaveAttachedPlayerCards)
      bamf <- putInHand p "48006"
      choose "play Bamf" (isPlay bamf)
      offers (isTargetChoice mid) `shouldReturn` False
      chooseVillain
      payWith []
      pass

  it "Azazel's boost star deals the same physical card to the Kurt Wagner player" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      [azazel] <- stackEncounterDeck [Minions.nc48027]
      v <- villainIdNow
      runNow [VillainMessage v (VillainAttacks p)]
      pass
      playerIdentityEncounterCards <$> playerAttrs p >>= (`shouldSatisfy` elem azazel)

  it "Brimstone Dimension has five fixed threat and deals Azazel to the player who defeats it" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      azazels <- findEncounterCards (CardIs Minions.nc48027)
      let azazel = testHead azazels
      scheme <- revealNow p Schemes.nc48028
      pass
      let sid = sideSchemeOf scheme
      sideSchemeThreat sid `shouldReturn` 5
      resolve [SideSchemeMessage sid (SideSchemeThreatRemoved (IdentityRef p) 5)]
      playerIdentityEncounterCards <$> playerAttrs p >>= (`shouldSatisfy` elem azazel)

  it "Azazel's Sword attaches to Azazel, adds ATK and piercing, then discards for a random hand card after he attacks" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      azazel <- addMinion p Minions.nc48027
      sword <- revealNow p Attachments.nc48029
      pass
      getModifiers azazel >>= (`shouldSatisfy` (\ms -> AttackModifier 1 `elem` ms && KeywordModifier Piercing `elem` ms))
      card <- putInHand p "48010"
      runNow [MinionMessage azazel (MinionAttacks p)]
      choose "decline defender" isNoDefender
      choose "discard Sword" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId sword)
      pass
      discardOf p >>= (`shouldSatisfy` elem card)
      encounterDiscard >>= (`shouldSatisfy` elem sword)

  it "Azazel's Sword attaches to the villain when Azazel is absent" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Attachments.nc48029
      pass
      v <- villainIdNow
      getModifiers v >>= (`shouldSatisfy` elem (KeywordModifier Piercing))

  it "Brimstone Strike finds and reveals Azazel and schemes in alter-ego form" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Minions.nc48027] []
      before <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.nc48030
      pass
      mainSchemeThreatNow `shouldReturn` before + 2
      minionCodes >>= (`shouldSatisfy` elem "48027")

  it "Brimstone Strike reveals Azazel in hero form without making him scheme" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- setEncounterDeck [Minions.nc48027] []
      before <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.nc48030
      pass
      mainSchemeThreatNow `shouldReturn` before
      damageOf p `shouldReturn` 3

  it "Queen of Hearts finds The Crazy Gang; a second reveal deals an encounter card instead" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Schemes.nc48033, Treacheries.assault] []
      _ <- revealNow p Minions.nc48034
      pass
      sideSchemeCodes >>= (`shouldSatisfy` elem "48033")
      before <- length . playerIdentityEncounterCards <$> playerAttrs p
      _ <- revealNow p Minions.nc48034
      pass
      length . playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` before + 1

  it "Jester confuses; if already confused, it discards a controlled support" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      support <- putIntoPlay p "48003"
      first <- revealNow p Minions.nc48035
      pass
      playerIdentityConfused <$> playerAttrs p `shouldReturn` True
      resolve [RemoveFromPlay $ MinionRef $ MinionId $ unCardId $ ecCardId first]
      _ <- revealNow p Minions.nc48035
      choose "discard Chapel" (isTargetChoice $ supportOf support)
      pass
      supportsWith "48003" `shouldReturn` []

  it "Tweedledope stuns; if already stunned it discards a controlled upgrade, and has only one boost icon" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      upgrade <- putIntoPlay p "48005"
      first <- revealNow p Minions.nc48037
      pass
      playerIdentityStunned <$> playerAttrs p `shouldReturn` True
      resolve [RemoveFromPlay $ MinionRef $ MinionId $ unCardId $ ecCardId first]
      _ <- revealNow p Minions.nc48037
      choose "discard Tail" (isTargetChoice $ upgradeIdOf upgrade)
      pass
      upgradesWith "48005" `shouldReturn` []
      cdBoostIcons Minions.nc48037 `shouldBe` [B.Boost]

  it "Executioner's reveal attacks a friendly character with the fewest remaining hit points" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      ally <- putIntoPlay p "01083"
      resolve [AllyMessage (allyIdOf ally) (AllyDamaged GameRef $ toDamage 1 FromAbility)]
      executioner <- revealNow p Minions.nc48036
      whenM (offers $ isTargetChoice $ allyIdOf ally) $ choose "attack the weakened ally" (isTargetChoice $ allyIdOf ally)
      pass
      let mid = MinionId $ unCardId $ ecCardId executioner
      whenM (offers $ isAbilityOf mid) $ choose "Executioner removes the defeated ally" (isAbilityOf mid)
      pass
      alliesWith "01083" `shouldReturn` []
      discardOf p >>= (`shouldSatisfy` notElem ally)

  it "The Crazy Gang deals a non-Elite scheming minion as an encounter card and ignores Elite minions" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Schemes.nc48033
      pass
      m <- addMinion p Minions.hydraRegular
      resolve [MinionMessage m MinionSchemes]
      minionCodes >>= (`shouldSatisfy` notElem "04056")
      cards <- playerIdentityEncounterCards <$> playerAttrs p
      cards `shouldSatisfy` any ((== "04056") . toCardCode)

  it "Off With His Head activates all minions or reveals the next discarded minion when none activate" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Treacheries.assault, Minions.hydraRegular] []
      _ <- revealNow p Treacheries.nc48038
      pass
      minionCodes >>= (`shouldSatisfy` elem "04056")
  it "Brimstone Strike finds in-play Azazel without resetting his damage or attached Sword" do
    tg <- testGame ["nightcrawler", "spider-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      azazel <- addMinion p Minions.nc48027
      resolve [MinionMessage azazel (MinionDamaged GameRef $ toDamage 1 FromAbility)]
      sword <- revealNow p Attachments.nc48029
      pass
      _ <- revealNow other Treacheries.nc48030
      pass
      attrs <- getsGame (toAttrs . fromMaybe (error "Azazel disappeared") . HashMap.lookup azazel . gameMinions)
      minionDamage attrs `shouldBe` 1
      minionEngagedIdentity attrs `shouldBe` other
      getModifiers azazel >>= (`shouldSatisfy` elem (KeywordModifier Piercing))
      encounterDiscard >>= (`shouldSatisfy` notElem sword)

  it "Brimstone Dimension finds Azazel already in play and deals his exact card facedown" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      azazel <- addMinion p Minions.nc48027
      scheme <- revealNow p Schemes.nc48028
      pass
      resolve [SideSchemeMessage (sideSchemeOf scheme) (SideSchemeThreatRemoved (IdentityRef p) 5)]
      minionCodes >>= (`shouldSatisfy` notElem "48027")
      dealt <- playerIdentityEncounterCards <$> playerAttrs p
      dealt `shouldSatisfy` any ((== unMinionId azazel) . unCardId . ecCardId)

  it "The Crazy Gang passes the exact dealt minion to the next player at a multiplayer table" do
    tg <- testGame ["nightcrawler", "spider-man"]
    inGame tg do
      let p = player1 tg
          next = player2 tg
      _ <- revealNow p Schemes.nc48033
      pass
      m <- addMinion p Minions.hydraRegular
      resolve [MinionMessage m MinionSchemes]
      first <- playerIdentityEncounterCards <$> playerAttrs p
      second <- playerIdentityEncounterCards <$> playerAttrs next
      first `shouldSatisfy` all ((/= unMinionId m) . unCardId . ecCardId)
      second `shouldSatisfy` any ((== unMinionId m) . unCardId . ecCardId)

  it "The Crazy Gang leaves Elite minions in play after they scheme" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Schemes.nc48033
      pass
      azazel <- addMinion p Minions.nc48027
      resolve [MinionMessage azazel MinionSchemes]
      minionCodes >>= (`shouldSatisfy` elem "48027")

  it "Executioner can attack Kurt Wagner in alter-ego form when he is the only friendly character" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Minions.nc48036
      pass
      damageOf p `shouldReturn` 2

  for_ [False,True] $ \hero ->
    it ("Off With His Head finds a minion when the only activation is replaced by a status: " <> show hero) do
      tg <- testGame ["nightcrawler"]
      inGame tg do
        let p = player1 tg
        when hero $ asHero p
        m <- addMinion p Minions.hydraMercenary
        resolve [GiveStatus (MinionRef m) (if hero then StunnedStatus else ConfusedStatus)]
        _ <- setEncounterDeck [Minions.hydraRegular, Treacheries.assault] []
        _ <- revealNow p Treacheries.nc48038
        pass
        codes <- minionCodes
        codes `shouldSatisfy` elem "04056"
        length codes `shouldBe` 2

  it "Off With His Head finds a minion when Frozen Solid replaces the only activation" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.hydraMercenary
      solid <- putInHand p "46007"
      energy <- putInHand p "01088"
      strength <- putInHand p "01090"
      playOn solid m [energy,strength]
      pass
      _ <- setEncounterDeck [Minions.hydraRegular, Treacheries.assault] []
      _ <- revealNow p Treacheries.nc48038
      choose "Frozen Solid replaces activation" (isAbilityOfCard solid)
      pass
      codes <- minionCodes
      codes `shouldSatisfy` elem "04056"
      length codes `shouldBe` 2

  where
    minionCodes = getsGame $ map (toCardCode . getCardDef) . HashMap.elems . gameMinions
    sideSchemeCodes = getsGame $ map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes
