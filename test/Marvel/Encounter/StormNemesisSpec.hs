module Marvel.Encounter.StormNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Query
import Marvel.Stats
import Marvel.Keyword
import Marvel.Status
import Marvel.Trait qualified as Trait
import TestImport

settle :: GameAppT ()
settle = go (150 :: Int)
 where
  go 0 = error "Storm encounter fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let cs = choicesOf q
          passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i, c) <- zip [0 ..] cs, passing c])
      go (n - 1)
    Nothing -> pure ()

-- Keep encounter fixtures independent of the shuffled setup choice, without
-- resolving a Weather Special or changing Storm's once-per-round ability.
clearSkies :: IdentityId -> GameAppT ()
clearSkies p = do
  cards <- fromMaybe [] . playerIdentityWeatherDeck <$> playerAttrs p
  for_ (headMay [card | card <- cards, toCardCode card == "36002"]) $ \card ->
    resolve [IdentityMessage p (ChooseWeatherSupport card False)]

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

attachmentPresent :: EncounterCard -> GameAppT Bool
attachmentPresent card = getsGame (HashMap.member (attachmentIdOf card) . gameAttachments)

controlledOf :: PlayerCard -> MinionId
controlledOf = MinionId . unCardId . pcCardId

sideSchemesWith :: CardCode -> GameAppT [SideSchemeId]
sideSchemesWith code = getsGame (HashMap.keys . HashMap.filter ((== code) . toCardCode . getCardDef) . gameSideSchemes)

boostNow :: CardDef -> GameAppT ()
boostNow def = do
  vid <- villainIdNow
  card <- encounterCard def
  runNow [RevealBoostCard card (EnemyVillainId vid)]
  settle

spec :: Spec
spec = describe "Storm nemesis and Shadow King modular" do
  describe "Claustrophobia" do
    it "forcibly flips Storm, stays in play, and prevents changing back" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        changeForm p
        pass
        card <- revealNow p Obligations.claustrophobia
        settle
        identityMatches AlterEgoIdentity p `shouldReturn` True
        identityMatches IdentityCanChangeForm p `shouldReturn` False
        offers (\case UseAbility a -> ChangeForm `elem` abilityChoices a && abilitySource a == IdentityRef p; _ -> False) `shouldReturn` False
        getsGame (HashMap.member (ObligationId $ unCardId $ ecCardId card) . gameObligations) `shouldReturn` True
        choose "exhaust Ororo to remove Claustrophobia" (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId card)
        settle
        isExhaustedNow p `shouldReturn` True
        getsGame (HashMap.member (ObligationId $ unCardId $ ecCardId card) . gameObligations) `shouldReturn` False
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "36030")

    it "does not allow another player's alter-ego to remove it" do
      tg <- testGame ["storm", "spider-man"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        card <- revealNow p Obligations.claustrophobia
        settle
        resolve [IdentityMessage p ExhaustedIdentity]
        offers (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId card) `shouldReturn` False

  describe "Callisto" do
    it "quickstrikes a hero as she enters play" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        changeForm p
        pass
        _ <- revealNow p Minions.callisto
        settle
        damageOf p `shouldReturn` 3

    it "does not quickstrike an alter-ego" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        _ <- revealNow (player1 tg) Minions.callisto
        settle
        damageOf (player1 tg) `shouldReturn` 0

    it "gets tough before Knife Fight damages her" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        mid <- addMinion p Minions.callisto
        changeForm p
        pass
        _ <- revealNow p Treacheries.knifeFight
        settle
        damageOf p `shouldReturn` 3
        minionDamageOf mid `shouldReturn` Just 0
        getsGame (fmap (Minion.minionTough . toAttrs) . HashMap.lookup mid . gameMinions) `shouldReturn` Just False

    it "does not gain tough when another treachery is revealed" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        mid <- addMinion p Minions.callisto
        _ <- revealNow p Treacheries.advance
        settle
        getsGame (fmap (Minion.minionTough . toAttrs) . HashMap.lookup mid . gameMinions) `shouldReturn` Just False

  describe "Leader of the Morlocks" do
    it "finds Knife Fight in set-aside when defeated and reveals it for the thwarting player" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        scheme <- revealNow p Schemes.leaderOfTheMorlocks
        settle
        changeForm p
        pass
        before <- villainDamageNow
        resolve [SideSchemeMessage (sideSchemeOf scheme) $ SideSchemeThreatRemoved (IdentityRef p) 2]
        settle
        damageOf p `shouldReturn` 2
        villainDamageNow `shouldReturn` (before + 2)
        encounterDiscardCodes >>= (`shouldSatisfy` elem "36034")

    it "finds Knife Fight in the encounter discard after defeat" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        scheme <- revealNow p Schemes.leaderOfTheMorlocks
        settle
        _ <- setEncounterDeck [Treacheries.advance] [Treacheries.knifeFight]
        changeForm p
        pass
        resolve [SideSchemeMessage (sideSchemeOf scheme) $ SideSchemeThreatRemoved (IdentityRef p) 2]
        settle
        damageOf p `shouldReturn` 2

  describe "Switchblade" do
    it "attaches to the highest printed ATK minion and gives +2 ATK and piercing" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        low <- addMinion p Minions.hydraBomber
        high <- addMinion p Minions.callisto
        card <- revealNow p Attachments.switchblade
        settle
        getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentIdOf card) . gameAttachments) `shouldReturn` Just (Just $ EnemyMinionId high)
        changeForm p
        pass
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        -- Hydra Bomber's reveal already dealt damage in this fixture.
        setDamage p 0
        characterAttackValue (MinionCharacter high) `shouldReturn` 5
        runNow [MinionMessage high $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 5
        minionDamageOf low `shouldReturn` Just 0

    it "keeps its printed +2 ATK but loses piercing when Vivian blanks the attachment" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        mid <- addMinion p Minions.callisto
        blade <- revealNow p Attachments.switchblade
        settle
        changeForm p
        pass
        clearHand p
        vivian <- putInHand p "29024"
        resource <- putInHand p "01088"
        playWith vivian [resource]
        choose "Vivian's response" (isAbilityOfCard vivian)
        choose "blank Switchblade" (isTargetChoice $ attachmentIdOf blade)
        pass
        characterAttackValue (MinionCharacter mid) `shouldReturn` 5
        Just minion <- getsGame (HashMap.lookup mid . gameMinions)
        Minion.getModifiedKeywords (toAttrs minion) >>= (`shouldSatisfy` notElem Piercing)
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 0
        attachmentPresent blade `shouldReturn` True

    it "keeps Switchblade's piercing when Blizzard blanks only the attached minion" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        mid <- addMinion p Minions.hydraMercenary
        _ <- revealNow p Attachments.switchblade
        settle
        changeForm p
        pass
        cards <- fromMaybe [] . playerIdentityWeatherDeck <$> playerAttrs p
        let blizzard = testHead [card | card <- cards, toCardCode card == "36005"]
        resolve [IdentityMessage p (ChooseWeatherSupport blizzard False)]
        sid <- selectJust (SupportNamed "Blizzard")
        resolve [ResolveWeatherSpecial p sid]
        characterAttackValue (MinionCharacter mid) `shouldReturn` 2
        Just minion <- getsGame (HashMap.lookup mid . gameMinions)
        Minion.getModifiedKeywords (toAttrs minion) >>= (`shouldSatisfy` elem Piercing)
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 2

    it "allows the revealing player to choose tied highest minions" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        a <- addMinion p Minions.hydraBomber
        b <- addMinion p Minions.hydraBomber
        card <- revealNow p Attachments.switchblade
        offers (isTargetChoice a) `shouldReturn` True
        offers (isTargetChoice b) `shouldReturn` True
        choose "second bomber" (isTargetChoice b)
        settle
        getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentIdOf card) . gameAttachments) `shouldReturn` Just (Just $ EnemyMinionId b)

    it "surges rather than attaching to the villain when no minions exist" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- length <$> dealtTo p
        card <- revealNow p Attachments.switchblade
        settle
        attachmentPresent card `shouldReturn` False
        (length <$> dealtTo p) `shouldReturn` (before + 1)

  describe "Knife Fight" do
    it "deals nonattack damage equal to highest enemy ATK and strikes back with the hero's ATK" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        mid <- addMinion p Minions.hydraMercenary
        changeForm p
        pass
        _ <- revealNow p Treacheries.knifeFight
        vid <- villainIdNow
        before <- villainDamageNow
        choose "highest ATK enemy" (isTargetChoice vid)
        settle
        damageOf p `shouldReturn` 2
        minionDamageOf mid `shouldReturn` Just 0
        villainDamageNow `shouldReturn` (before + 2)

    it "finishes striking back with the hero's pre-defeat ATK after lethal damage in multiplayer" do
      tg <- testGame ["storm", "spider-man"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        changeForm p
        pass
        setDamage p 9
        before <- villainDamageNow
        vid <- villainIdNow
        _ <- revealNow p Treacheries.knifeFight
        choose "fight the villain" (isTargetChoice vid)
        settle
        identityMatches (IdentityWithId p) p `shouldReturn` False
        villainDamageNow `shouldReturn` (before + 2)
        damageOf (player2 tg) `shouldReturn` 0

    it "surges in alter-ego form" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.knifeFight
        settle
        damageOf p `shouldReturn` 0
        (length <$> dealtTo p) `shouldReturn` (before + 1)

  describe "Possessed" do
    it "takes the lowest THW ally, keeps its damage, blanks it and engages its controller" do
      tg <- testGame ["storm", "spider-man"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        low <- putIntoPlay (player2 tg) "01083"
        high <- putIntoPlay p "01084"
        resolve [AllyMessage (allyIdOf low) $ AllyDamaged (IdentityRef p) $ toDamage 1 FromAbility]
        card <- revealNow p Attachments.possessed
        settle
        let mid = controlledOf low
        minionDamageOf mid `shouldReturn` Just 1
        selectAny (MinionWithId mid <> MinionWithTrait Trait.Controlled <> MinionEngagedWith (IdentityWithId $ player2 tg)) `shouldReturn` True
        alliesWith (toCardCode high) >>= (`shouldSatisfy` notNull)
        getsGame (fmap (Minion.minionScheme . toAttrs) . HashMap.lookup mid . gameMinions) `shouldReturn` Just (Sch 1)
        attachmentPresent card `shouldReturn` True

    it "returns the minion to an ally when Possessed is discarded" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        ally <- putIntoPlay p "01083"
        card <- revealNow p Attachments.possessed
        settle
        resolve [RemoveFromPlay (AttachmentRef $ attachmentIdOf card)]
        settle
        minionDamageOf (controlledOf ally) `shouldReturn` Nothing
        alliesWith "01083" >>= (`shouldSatisfy` notNull)

    it "blanks the converted ally's printed retaliate keyword" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        ally <- putIntoPlay p "33013"
        _ <- revealNow p Attachments.possessed
        settle
        changeForm p
        pass
        clearHand p
        choose "basic attack" (isHeroAttack p)
        choose "attack the possessed ally" (isTargetChoice $ controlledOf ally)
        settle
        damageOf p `shouldReturn` 0
        minionDamageOf (controlledOf ally) `shouldReturn` Just 2

    it "preserves attached encounter cards in both directions of the conversion" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        ally <- putIntoPlay p "01083"
        attached <- revealNow p Attachments.restrained
        settle
        possessed <- revealNow p Attachments.possessed
        settle
        attachmentPresent attached `shouldReturn` True
        getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentIdOf attached) . gameAttachments) `shouldReturn` Just (Just $ EnemyMinionId $ controlledOf ally)
        resolve [RemoveFromPlay (AttachmentRef $ attachmentIdOf possessed)]
        settle
        attachmentPresent attached `shouldReturn` True
        getsGame (fmap (Attachment.attachmentAlly . toAttrs) . HashMap.lookup (attachmentIdOf attached) . gameAttachments) `shouldReturn` Just (Just $ allyIdOf ally)
        alliesWith "01083" >>= (`shouldSatisfy` notNull)

    it "returns a converted player card to its owner's hand and discards its attachments" do
      tg <- testGame ["storm", "spider-man"]
      inGame tg do
        clearSkies (player1 tg)
        let owner = player2 tg
        ally <- putIntoPlay owner "01083"
        attached <- revealNow owner Attachments.restrained
        settle
        possessed <- revealNow owner Attachments.possessed
        settle
        resolve [ReturnToHand (MinionRef $ controlledOf ally)]
        settle
        handOf owner >>= (`shouldSatisfy` any ((== pcCardId ally) . pcCardId))
        handOf (player1 tg) >>= (`shouldSatisfy` all ((/= pcCardId ally) . pcCardId))
        discardOf owner >>= (`shouldSatisfy` all ((/= pcCardId ally) . pcCardId))
        minionDamageOf (controlledOf ally) `shouldReturn` Nothing
        alliesWith "01083" `shouldReturn` []
        attachmentPresent attached `shouldReturn` False
        attachmentPresent possessed `shouldReturn` False
        selectAny (MinionWithId (controlledOf ally) <> MinionEngagedWith (IdentityWithId owner)) `shouldReturn` False

    it "surges if no ally exists" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- length <$> dealtTo p
        card <- revealNow p Attachments.possessed
        settle
        attachmentPresent card `shouldReturn` False
        (length <$> dealtTo p) `shouldReturn` (before + 1)

  describe "The Shadow King" do
    it "searches the encounter deck for Possessed and is invulnerable while a controlled minion exists" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        ally <- putIntoPlay p "01083"
        _ <- setEncounterDeck [Attachments.possessed, Treacheries.advance] []
        king <- revealNow p Minions.theShadowKing
        settle
        selectAny (MinionWithId (controlledOf ally) <> MinionWithTrait Trait.Controlled) `shouldReturn` True
        resolve [MinionMessage (minionOf king) $ MinionDamaged (IdentityRef p) $ toDamage 4 FromAbility]
        minionDamageOf (minionOf king) `shouldReturn` Just 0
        attachments <- selectList (AttachmentIs Attachments.possessed)
        resolve [RemoveFromPlay (AttachmentRef aid) | aid <- attachments]
        resolve [MinionMessage (minionOf king) $ MinionDamaged (IdentityRef p) $ toDamage 4 FromAbility]
        minionDamageOf (minionOf king) `shouldReturn` Just 4

    it "also searches the encounter discard for Possessed" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        ally <- putIntoPlay p "01083"
        _ <- setEncounterDeck [Treacheries.advance] [Attachments.possessed]
        _ <- revealNow p Minions.theShadowKing
        settle
        minionDamageOf (controlledOf ally) `shouldReturn` Just 0

  describe "Ruler of the Astral Plane" do
    it "has six fixed threat and a hazard icon even in multiplayer" do
      tg <- testGame ["storm", "spider-man"]
      inGame tg do
        clearSkies (player1 tg)
        card <- revealNow (player1 tg) Schemes.rulerOfTheAstralPlane
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 6
        cdHazards Schemes.rulerOfTheAstralPlane `shouldBe` 1
        cdAcceleration Schemes.rulerOfTheAstralPlane `shouldBe` 0

    it "discards one Possessed when defeated and restores that ally" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        ally <- putIntoPlay p "01083"
        card <- revealNow p Attachments.possessed
        settle
        scheme <- revealNow p Schemes.rulerOfTheAstralPlane
        settle
        resolve [SideSchemeMessage (sideSchemeOf scheme) $ SideSchemeThreatRemoved (IdentityRef p) 6]
        settle
        attachmentPresent card `shouldReturn` False
        minionDamageOf (controlledOf ally) `shouldReturn` Nothing

    it "reveals as a boost only when the attacked player is engaged with a controlled minion" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        _ <- putIntoPlay p "01083"
        _ <- revealNow p Attachments.possessed
        settle
        boostNow Schemes.rulerOfTheAstralPlane
        sideSchemesWith "36037" >>= (`shouldSatisfy` notNull)

    it "does not reveal as a boost without a controlled minion" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        boostNow Schemes.rulerOfTheAstralPlane
        sideSchemesWith "36037" `shouldReturn` []

  describe "Astral Attack" do
    it "activates every controlled minion against the revealing player, regardless of engagement" do
      tg <- testGame ["storm", "spider-man"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        _ <- putIntoPlay (player2 tg) "01083"
        _ <- revealNow p Attachments.possessed
        settle
        changeForm p
        pass
        _ <- revealNow p Treacheries.astralAttack
        settle
        damageOf p `shouldReturn` 1
        damageOf (player2 tg) `shouldReturn` 0

    it "makes a controlled minion scheme against an alter-ego without consequential damage" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        ally <- putIntoPlay p "01083"
        _ <- revealNow p Attachments.possessed
        settle
        before <- mainSchemeThreat
        _ <- revealNow p Treacheries.astralAttack
        settle
        mainSchemeThreat `shouldReturn` (before + 1)
        minionDamageOf (controlledOf ally) `shouldReturn` Just 0

    it "activates all controlled minions and ignores ordinary minions" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        _ <- putIntoPlay p "01083"
        _ <- revealNow p Attachments.possessed
        settle
        _ <- putIntoPlay p "01083"
        _ <- revealNow p Attachments.possessed
        settle
        _ <- addMinion p Minions.hydraBomber
        before <- mainSchemeThreat
        _ <- revealNow p Treacheries.astralAttack
        settle
        mainSchemeThreat `shouldReturn` (before + 2)

    it "schemes against the revealing alter-ego rather than the minion's engaged hero" do
      tg <- testGame ["storm", "spider-man"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        ally <- putIntoPlay (player2 tg) "01083"
        _ <- revealNow p Attachments.possessed
        settle
        resolve [IdentityMessage (player2 tg) ChooseHeroForm]
        -- The minion remains engaged with Peter, but Astral Attack activates it
        -- against Ororo, who is the player revealing the treachery.
        before <- mainSchemeThreat
        _ <- revealNow p Treacheries.astralAttack
        settle
        mainSchemeThreat `shouldReturn` (before + 1)
        damageOf p `shouldReturn` 0
        damageOf (player2 tg) `shouldReturn` 0
        minionDamageOf (controlledOf ally) `shouldReturn` Just 0

    it "lets the revealing player choose activation order and stops if that player is eliminated" do
      tg <- testGame ["storm", "spider-man"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
            other = player2 tg
        first <- putIntoPlay other "01083"
        _ <- revealNow p Attachments.possessed
        settle
        second <- putIntoPlay other "36022"
        _ <- revealNow p Attachments.possessed
        settle
        changeForm p
        pass
        setDamage p 9
        before <- mainSchemeThreat
        _ <- revealNow p Treacheries.astralAttack
        offers (isTargetChoice $ controlledOf first) `shouldReturn` True
        offers (isTargetChoice $ controlledOf second) `shouldReturn` True
        choose "activate Forge first" (isTargetChoice $ controlledOf second)
        getsGame (maybe False (isJust . Minion.minionAttacking . toAttrs) . HashMap.lookup (controlledOf second) . gameMinions) `shouldReturn` True
        settle
        identityMatches (IdentityWithId p) p `shouldReturn` False
        damageOf other `shouldReturn` 0
        mainSchemeThreat `shouldReturn` before
        getsGame (maybe False (isJust . Minion.minionAttacking . toAttrs) . HashMap.lookup (controlledOf first) . gameMinions) `shouldReturn` False

    it "surges when no controlled minion is in play" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        let p = player1 tg
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.astralAttack
        settle
        (length <$> dealtTo p) `shouldReturn` (before + 1)

    it "shuffles only Shadow King cards out of the encounter discard with its boost" do
      tg <- testGame ["storm"]
      inGame tg do
        clearSkies (player1 tg)
        _ <- setEncounterDeck [Treacheries.advance] [Attachments.possessed, Minions.theShadowKing, Treacheries.assault]
        boostNow Treacheries.astralAttack
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01187")
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "36038")
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "36036")
