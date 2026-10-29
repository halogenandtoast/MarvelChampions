module Marvel.Encounter.ValkyrieNemesisSpec (spec) where

import Data.List (find)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (getAbilities)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet (EncounterSet (ValkyrieNemesis), getNemesisSet)
import Marvel.Hp (HP (..))
import Marvel.Matchers (EnemyMatcher (..), IdentityMatcher (IdentityWithId), identityMatches)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier (Modifier (CannotBeDiscarded))
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types qualified as Obligation
import Marvel.Query (selectList)
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Stats (Atk (..), Sch (..))
import Marvel.Trait (Trait (Android, Condition, Enthralled), HasTraits (getTraits))
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

-- Resolve encounter prompts while declining optional responses/defenses, keeping
-- the player turn as the next decision. Target choices are explicit in tests.
settle :: GameAppT ()
settle = go (100 :: Int)
 where
  go 0 = error "encounter fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (ident, q) -> do
      let cs = choicesOf q
          passing = \case Label l _ -> l `elem` ["$label.noResponse", "$label.noDefender", "$label.declineToDefend"]; _ -> False
      answerQuestion ident (fromMaybe 0 $ headMay [i | (i, c) <- zip [0 ..] cs, passing c])
      go (n - 1)
    Nothing -> pure ()

identityAbility :: IdentityId -> Natural -> Choice -> Bool
identityAbility p idx = \case
  UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == idx
  _ -> False

playGlow :: IsRef enemy => IdentityId -> enemy -> GameAppT PlayerCard
playGlow p enemy = do
  glow <- getSetAsidePlayerCards >>= \cs -> pure $ fromMaybe (error "Death-Glow not set aside") (find ((== "25002") . toCardCode) cs)
  resource <- putInHand p "25024"
  choose "Death Perception" (identityAbility p 1)
  choose "mark the enemy" (isTargetChoice enemy)
  payWith [resource]
  pass
  pure glow

attachmentsOf :: CardDef -> GameAppT [Attrs Attachment.Attachment]
attachmentsOf def = getsGame (filter ((== toCardCode def) . toCardCode . Attachment.attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

obligationsOf :: GameAppT [Attrs Obligation.Obligation]
obligationsOf = getsGame (map toAttrs . HashMap.elems . gameObligations)

obligationAction :: ObligationId -> Choice -> Bool
obligationAction oid = isAbilityOf (ObligationRef oid)

attachmentAction :: AttachmentId -> Choice -> Bool
attachmentAction aid = isAbilityOf (AttachmentRef aid)

revealTrouble :: IdentityId -> GameAppT ObligationId
revealTrouble p = do
  _ <- revealNow p Obligations.troubleInOtherworld
  settle
  obligationsOf >>= \os -> pure $ Obligation.obligationId $ fromMaybe (error "Trouble in Otherworld missing") (find ((== "25028") . toCardCode . Obligation.obligationCardDef) os)

revealEnchantments :: IdentityId -> GameAppT SideSchemeId
revealEnchantments p = do
  card <- revealNow p Schemes.powerfulEnchantments
  settle
  pure $ SideSchemeId $ unCardId $ ecCardId card

spec :: Spec
spec = describe "Valkyrie nemesis" do
  describe "pack registration and printed reprints" do
    it "gathers the five-card nemesis set with the correct printing codes and quantities" do
      getNemesisSet "25001a" `shouldBe` ValkyrieNemesis :: IO ()
      tg <- testGame ["valkyrie"]
      inGame tg do
        cards <- gatherEncounterSet ValkyrieNemesis
        map toCardCode cards `shouldMatchList` ["25029", "25030", "25031", "25032", "25032"]
        for_ [Minions.enchantressValkyrie, Schemes.powerfulEnchantments, Attachments.beguiledValkyrie, Attachments.seducedValkyrie] $ \def ->
          cdEncounterSet def `shouldBe` Just ValkyrieNemesis
        map cdEncounterSetQuantity [Minions.enchantressValkyrie, Attachments.beguiledValkyrie, Attachments.seducedValkyrie] `shouldBe` [Just 1, Just 1, Just 2]

    it "retains behavior aliases without inheriting the original set's traits or quantities" do
      sameCard Minions.enchantressValkyrie Minions.enchantress `shouldBe` True :: IO ()
      sameCard Attachments.beguiledValkyrie Attachments.beguiled `shouldBe` True :: IO ()
      sameCard Attachments.seducedValkyrie Attachments.seduced `shouldBe` True :: IO ()
      cdTraits Attachments.beguiledValkyrie `shouldBe` fromList [Condition] :: IO ()
      -- The inspected25032 printed scan has no trait line;21179 has Condition.
      cdTraits Attachments.seducedValkyrie `shouldBe` mempty :: IO ()
      map (length . cdBoostIcons) [Minions.enchantressValkyrie, Schemes.powerfulEnchantments, Attachments.beguiledValkyrie, Attachments.seducedValkyrie] `shouldBe` [2, 2, 1, 2] :: IO ()

  describe "Trouble in Otherworld" do
    it "is given to Brunnhilde even when another player reveals it, persists, and does not flip or exhaust her" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- revealTrouble (player2 tg)
        [obligation] <- obligationsOf
        Obligation.obligationOwner obligation `shouldBe` Just p
        playerIdentitySide <$> playerAttrs p `shouldReturn` A
        exhaustedNow p `shouldReturn` False
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "25028")

    it "prevents only Valkyrie's attacks against the marked enemy, while allowing unmarked targets and her ally's attack" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        marked <- addMinion p Minions.titania
        other <- addMinion p Minions.hydraMercenary
        changeForm p
        _ <- playGlow p marked
        _ <- revealTrouble p
        heroTargets <- selectList (AttackableBy $ IdentityWithId p)
        heroTargets `shouldSatisfy` notElem (EnemyMinionId marked)
        heroTargets `shouldSatisfy` elem (EnemyMinionId other)
        choose "make a basic attack" (identityAbility p 301)
        offers (isTargetChoice marked) `shouldReturn` False
        choose "attack the unmarked minion" (isTargetChoice other)
        pass
        minionDamageOf other `shouldReturn` Just 2
        ally <- putIntoPlay p "01051"
        choose "attack with the ally" $ \case UseAbility a -> abilitySource a == AllyRef (allyIdOf ally) && abilityIndex a == 301; _ -> False
        choose "the ally attacks the marked enemy" (isTargetChoice marked)
        pass
        minionDamageOf marked `shouldReturn` Just 2

    it "prevents an attack event from targeting the marked enemy but does not prohibit that other hero's attacks" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
          otherPlayer = player2 tg
      inGame tg do
        marked <- addMinion p Minions.titania
        target <- addMinion p Minions.hydraMercenary
        changeForm p
        _ <- playGlow p marked
        _ <- revealTrouble p
        resolve [IdentityMessage otherPlayer $ ChangedToForm A]
        card <- putInHand p "25012"
        energy <- putInHand p "25025"
        single <- putInHand p "25024"
        playWith card [energy, single]
        offers (isTargetChoice marked) `shouldReturn` False
        choose "attack the allowed minion" (isTargetChoice target)
        pass
        othersTargets <- selectList (AttackableBy $ IdentityWithId otherPlayer)
        othersTargets `shouldSatisfy` elem (EnemyMinionId marked)

    it "follows Death-Glow when Brunnhilde detaches it and Valkyrie plays it on a different enemy" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        first <- addMinion p Minions.titania
        second <- addMinion p Minions.hydraMercenary
        changeForm p
        glow <- playGlow p first
        _ <- revealTrouble p
        resolve [IdentityMessage p $ ChangedToForm B]
        choose "detach Death-Glow" (identityAbility p 2)
        pass
        Just detached <- getsGame (HashMap.lookup first . gameMinions)
        minionUpgrades (toAttrs detached) `shouldSatisfy` notElem (upgradeIdOf glow)
        resolve [IdentityMessage p $ ChangedToForm A]
        targets <- selectList (AttackableBy $ IdentityWithId p)
        targets `shouldSatisfy` elem (EnemyMinionId first)
        _ <- playGlow p second
        after <- selectList (AttackableBy $ IdentityWithId p)
        after `shouldSatisfy` elem (EnemyMinionId first)
        after `shouldSatisfy` notElem (EnemyMinionId second)

    it "requires both energy and mental in alter-ego form and removes itself from the game without exhausting" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        oid <- revealTrouble p
        clearHand p
        energy <- putInHand p "25025"
        _ <- putInHand p "25024"
        offers (obligationAction oid) `shouldReturn` False
        mental <- putInHand p "25026"
        offers (obligationAction oid) `shouldReturn` True
        choose "remove Trouble in Otherworld" (obligationAction oid)
        payWith [energy, mental]
        pass
        obligationsOf `shouldReturn` []
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "25028")
        exhaustedNow p `shouldReturn` False

    it "cannot be removed in hero form, but an exhausted alter-ego can still pay the resource cost" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        oid <- revealTrouble p
        energy <- putInHand p "25025"
        mental <- putInHand p "25026"
        offers (obligationAction oid) `shouldReturn` False
        resolve [IdentityMessage p (ChangedToForm B), IdentityMessage p ExhaustedIdentity]
        choose "pay while exhausted" (obligationAction oid)
        payWith [energy, mental]
        pass
        obligationsOf `shouldReturn` []
        exhaustedNow p `shouldReturn` True

  describe "Powerful Enchantments" do
    for_ [1, 2] $ \playersCount ->
      it ("has2 threat per player and adds Hinder1 per player, with players=" <> show playersCount) do
        tg <- testGame (if playersCount == (1 :: Int) then ["valkyrie"] else ["valkyrie", "spider-man"])
        inGame tg do
          sid <- revealEnchantments (player1 tg)
          sideSchemeThreat sid `shouldReturn` fromIntegral (playersCount * 3)

    it "prevents discarding Seduced from a friendly identity until the scheme is defeated" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Attachments.seducedValkyrie
        settle
        [seduced] <- attachmentsOf Attachments.seducedValkyrie
        let aid = Attachment.attachmentId seduced
        sid <- revealEnchantments p
        energy <- putInHand p "25025"
        mental <- putInHand p "25026"
        offers (attachmentAction aid) `shouldReturn` False
        resolve [RemoveFromPlay $ AttachmentRef aid]
        length <$> attachmentsOf Attachments.seducedValkyrie `shouldReturn` 1
        changeForm p
        justice <- putInHand p "01060"
        payment <- putInHand p "25025"
        playWith justice [payment]
        choose "thwart Powerful Enchantments" (isTargetChoice sid)
        pass
        sideSchemeThreat sid `shouldReturn` 0
        resolve [IdentityMessage p $ ChangedToForm B]
        choose "discard Seduced now" (attachmentAction aid)
        payWith [energy, mental]
        pass
        attachmentsOf Attachments.seducedValkyrie `shouldReturn` []

    it "protects an attachment on a friendly ally and lets it leave with that ally" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "01068"
        _ <- revealNow p Attachments.restrained
        settle
        [restrained] <- attachmentsOf Attachments.restrained
        Attachment.attachmentAlly restrained `shouldBe` Just (allyIdOf ally)
        _ <- revealEnchantments p
        let aid = Attachment.attachmentId restrained
        getModifiers (AttachmentRef aid) >>= (`shouldSatisfy` elem CannotBeDiscarded)
        resolve [RemoveFromPlay $ AttachmentRef aid]
        length <$> attachmentsOf Attachments.restrained `shouldReturn` 1
        resolve [ReturnToHand $ AllyRef $ allyIdOf ally]
        attachmentsOf Attachments.restrained `shouldReturn` []
        handOf p >>= (`shouldSatisfy` elem ally)

    it "does not protect an attachment on an enemy" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- revealEnchantments p
        _ <- revealNow p Attachments.enhancedIvoryHorn
        settle
        [horn] <- attachmentsOf Attachments.enhancedIvoryHorn
        changeForm p
        payment <- traverse (putInHand p) ["25024", "25024", "25024"]
        choose "discard Rhino's attachment" (attachmentAction $ Attachment.attachmentId horn)
        payWith payment
        pass
        attachmentsOf Attachments.enhancedIvoryHorn `shouldReturn` []

    it "does not protect Beguiled while its former ally is treated as a hostile minion" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "01068"
        _ <- revealNow p Attachments.beguiledValkyrie
        settle
        _ <- revealEnchantments p
        [beguiled] <- attachmentsOf Attachments.beguiledValkyrie
        let aid = Attachment.attachmentId beguiled
        getModifiers (AttachmentRef aid) >>= (`shouldSatisfy` notElem CannotBeDiscarded)
        resolve [RemoveFromPlay $ AttachmentRef aid]
        attachmentsOf Attachments.beguiledValkyrie `shouldReturn` []
        getsGame (HashMap.member (allyIdOf ally) . gameAllies) `shouldReturn` True

    it "does not prevent discarding player upgrades" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- revealEnchantments p
        upgrade <- putIntoPlay p "25006"
        resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf upgrade]
        discardOf p >>= (`shouldSatisfy` elem upgrade)

  describe "Enchantress reprint" do
    for_ [False, True] $ \associatedInDeck ->
      it ("selects its own associated Seduced when the modular printing is also present in a different zone; associated in deck=" <> show associatedInDeck) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          aside <- getSetAsideEncounterCards
          resolve [RemoveFromGame $ toRef card | card <- aside, toCardCode card == "25032"]
          let inDeck = if associatedInDeck then Attachments.seducedValkyrie else Attachments.seduced
              inDiscard = if associatedInDeck then Attachments.seduced else Attachments.seducedValkyrie
          (deck, discarded) <- setEncounterDeck [inDeck, Treacheries.advance] [inDiscard]
          let modular = if associatedInDeck then head discarded else head deck
          _ <- revealNow p Minions.enchantressValkyrie
          settle
          [attached] <- attachmentsOf Attachments.seducedValkyrie
          Attachment.attachmentIdentity attached `shouldBe` Just p
          attachmentsOf Attachments.seduced `shouldReturn` []
          remaining <- (<>) <$> scenarioEncounterDeckNow <*> encounterDiscard
          remaining `shouldSatisfy` elem modular

    for_ [False, True] $ \chooseValkyriePrinting ->
      it ("the original modular Enchantress retains a generic search across both printings; choose Valkyrie=" <> show chooseValkyriePrinting) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          aside <- getSetAsideEncounterCards
          resolve [RemoveFromGame $ toRef card | card <- aside, toCardCode card == "25032"]
          (deck, discarded) <- setEncounterDeck [Attachments.seduced, Treacheries.advance] [Attachments.seducedValkyrie]
          let chosen = if chooseValkyriePrinting then head discarded else head deck
              expected = if chooseValkyriePrinting then Attachments.seducedValkyrie else Attachments.seduced
          _ <- revealNow p Minions.enchantress
          choose "choose the available Seduced printing" (isTargetChoice chosen)
          settle
          [attached] <- attachmentsOf expected
          Attachment.attachmentIdentity attached `shouldBe` Just p

    for_ ["deck", "discard", "set aside"] $ \zone ->
      it ("finds the Valkyrie printing of Seduced in " <> zone <> " and attaches it to your identity") do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          _ <- setEncounterDeck [Minions.hydraMercenary, Treacheries.advance] []
          case zone of
            "deck" -> void $ stackEncounterDeck [Attachments.seducedValkyrie]
            "discard" -> void $ setEncounterDeck [Minions.hydraMercenary, Treacheries.advance] [Attachments.seducedValkyrie]
            _ -> pure ()
          card <- revealNow p Minions.enchantressValkyrie
          settle
          [seduced] <- attachmentsOf Attachments.seducedValkyrie
          Attachment.attachmentIdentity seduced `shouldBe` Just p
          let mid = MinionId $ unCardId $ ecCardId card
          Just minion <- getsGame (HashMap.lookup mid . gameMinions)
          let a = toAttrs minion
          (minionScheme a, minionAttack a, minionHitPoints a) `shouldBe` (Sch 2, Atk 1, HP 5)
          toCardCode (getCardDef minion) `shouldBe` "25029"

    it "does nothing when no Seduced can be found in any searched zone" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        aside <- getSetAsideEncounterCards
        resolve [RemoveFromGame $ toRef card | card <- aside, toCardCode card == "25032"]
        _ <- setEncounterDeck [Minions.hydraMercenary, Treacheries.advance] []
        _ <- revealNow p Minions.enchantressValkyrie
        settle
        attachmentsOf Attachments.seducedValkyrie `shouldReturn` []

  describe "Beguiled reprint" do
    it "takes the highest-cost ally from any player, blanks it as an Enthralled minion, keeps damage and attached upgrades, and uses printedTHW as SCH" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- putIntoPlay p "01067"
        vision <- putIntoPlay other "01068"
        inspired <- putInHand other "01074"
        resource <- putInHand other "01088"
        endTurn
        playOn inspired (allyIdOf vision) [resource]
        pass
        resolve [AllyMessage (allyIdOf vision) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
        _ <- revealNow other Attachments.beguiledValkyrie
        settle
        let mid = MinionId $ unAllyId $ allyIdOf vision
        Just minion <- getsGame (HashMap.lookup mid . gameMinions)
        let a = toAttrs minion
        minionEngagedIdentity a `shouldBe` other
        (minionScheme a, minionAttack a, minionHitPoints a, minionDamage a) `shouldBe` (Sch 1, Atk 2, HP 3, 1)
        getTraits minion >>= (`shouldSatisfy` member Enthralled)
        getTraits minion >>= (`shouldSatisfy` member Android)
        getAbilities minion `shouldBe` []
        discardOf other >>= (`shouldSatisfy` notElem inspired)
        [kept] <- upgradesWith "01074"
        upgradeAttachedEnemy kept `shouldBe` Just (EnemyMinionId mid)
        runNow [MinionMessage mid $ MinionAttacks other]
        pass
        minionDamageOf mid `shouldReturn` Just 1

    it "the revealing player chooses a tie for highest cost" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        hellcat <- putIntoPlay p "01020"
        hawkeye <- putIntoPlay p "01066"
        _ <- revealNow p Attachments.beguiledValkyrie
        offers (isTargetChoice $ allyIdOf hellcat) `shouldReturn` True
        offers (isTargetChoice $ allyIdOf hawkeye) `shouldReturn` True
        choose "enthrall Hawkeye" (isTargetChoice $ allyIdOf hawkeye)
        settle
        getsGame (HashMap.member (MinionId $ unAllyId $ allyIdOf hawkeye) . gameMinions) `shouldReturn` True
        getsGame (HashMap.member (allyIdOf hellcat) . gameAllies) `shouldReturn` True

    it "gains surge and is discarded when no ally is available" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        deck <- stackEncounterDeck [Minions.hydraMercenary, Treacheries.advance]
        _ <- revealNow p Attachments.beguiledValkyrie
        settle
        dealtTo p >>= (`shouldSatisfy` elem (head deck))
        encounterDiscardCodes >>= (`shouldSatisfy` elem "25031")

    it "defeating the hostile minion discards the ally to its owner and Beguiled to the encounter discard" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        vision <- putIntoPlay p "01068"
        _ <- revealNow p Attachments.beguiledValkyrie
        settle
        changeForm p
        haymaker <- putInHand p "01087"
        energy <- putInHand p "25025"
        playWith haymaker [energy]
        choose "defeat the enthralled ally" (isTargetChoice $ MinionId $ unAllyId $ allyIdOf vision)
        pass
        discardOf p >>= (`shouldSatisfy` any ((== pcCardId vision) . pcCardId))
        encounterDiscardCodes >>= (`shouldSatisfy` elem "25031")

  describe "Seduced reprint" do
    it "prevents basic attacks, including stunned attempts, and Attack events, while allies and basic thwart still work" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Attachments.seducedValkyrie
        settle
        target <- addMinion p Minions.hydraMercenary
        ally <- putIntoPlay p "01051"
        changeForm p
        clearHand p
        attack <- putInHand p "25012"
        _ <- putInHand p "25025"
        _ <- putInHand p "25024"
        placeMainThreat 3
        resolve [GiveStatus (IdentityRef p) StunnedStatus]
        offers (isHeroAttack p) `shouldReturn` False
        offers (isPlay attack) `shouldReturn` False
        offers (isHeroThwart p) `shouldReturn` True
        choose "attack with the ally" $ \case UseAbility a -> abilitySource a == AllyRef (allyIdOf ally) && abilityIndex a == 301; _ -> False
        choose "attack the minion" (isTargetChoice target)
        pass
        minionDamageOf target `shouldReturn` Just 2
        playerIdentityStunned <$> playerAttrs p `shouldReturn` True

    it "can be discarded in alter-ego form by spending exactly the required resource types" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Attachments.seducedValkyrie
        settle
        [seduced] <- attachmentsOf Attachments.seducedValkyrie
        let aid = Attachment.attachmentId seduced
        clearHand p
        energy <- putInHand p "25025"
        _ <- putInHand p "25024"
        offers (attachmentAction aid) `shouldReturn` False
        mental <- putInHand p "25026"
        choose "discard Seduced" (attachmentAction aid)
        payWith [energy, mental]
        pass
        attachmentsOf Attachments.seducedValkyrie `shouldReturn` []
        encounterDiscardCodes >>= (`shouldSatisfy` elem "25032")
        exhaustedNow p `shouldReturn` False

    it "its discard action is unavailable in hero form" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Attachments.seducedValkyrie
        settle
        [seduced] <- attachmentsOf Attachments.seducedValkyrie
        changeForm p
        _ <- putInHand p "25025"
        _ <- putInHand p "25026"
        offers (attachmentAction $ Attachment.attachmentId seduced) `shouldReturn` False
