-- | Brotherhood of Badoon (The Galaxy's Most Wanted, scenario 1): Drang, his main schemes
-- and encounter set, and the Band of Badoon modular set, each card played as a table would.
module Marvel.Encounter.BrotherhoodOfBadoonSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Public (publicGame)
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attachment, Attrs (..))
import Marvel.EncounterSet (EncounterSet (BandOfBadoon))
import Marvel.Environment.Cards qualified as EnvironmentCards
import Marvel.Environment.Types (Attrs (..))
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.Support.Cards qualified as SupportCards
import Marvel.Treachery.Cards qualified as TreacheryCards
import Marvel.Hp (unHp)
import Marvel.Villain.Types (villainBoostCards, villainHp, villainIsTough, villainStunned)
import TestImport

drangGame :: [Text] -> IO TestGame
drangGame = testGameAgainst "16058"

-- | Expert: Drang (II) and (III).
expertGame :: [Text] -> IO TestGame
expertGame heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "16058" [BandOfBadoon] Expert) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

-- | The Badoon Ship's id.
badoonShip :: GameAppT EnvironmentId
badoonShip =
  getsGame (filter ((== toCardCode EnvironmentCards.badoonShip) . toCardCode . getCardDef) . HashMap.elems . gameEnvironments) >>= \case
    [e] -> pure (toId e)
    es -> error $ "expected the Badoon Ship, found " <> show (length es)

barrageNow :: GameAppT Natural
barrageNow = do
  ship <- badoonShip
  getsGame (maybe 0 (environmentCounters . toAttrs) . HashMap.lookup ship . gameEnvironments)

chargeUpNow :: GameAppT ()
chargeUpNow = badoonShip >>= \ship -> resolve [CardSpecific (EnvironmentRef ship) "chargeUp" (toJSON ())]

villainCode :: GameAppT CardCode
villainCode = toCardCode . getCardDef <$> getVillain

mainSchemeCode :: GameAppT CardCode
mainSchemeCode = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMainSchemes) >>= \case
  [c] -> pure c
  _ -> error "expected one main scheme"

attachmentsWith :: CardCode -> GameAppT [Attachment]
attachmentsWith code = getsGame (filter ((== code) . toCardCode . getCardDef) . HashMap.elems . gameAttachments)

milanoExhausted :: GameAppT Bool
milanoExhausted = any (.supportExhausted) <$> supportsWith (toCardCode SupportCards.milano)

-- | Answer until the turn comes back: no defending and no responses, anything else (a
-- forced ability, a When Revealed) with its first choice.
settle :: GameAppT ()
settle = go (100 :: Int)
 where
  go 0 = pure ()
  go n =
    openQuestion >>= \case
      Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
      Just (ident, q) -> do
        let cs = choicesOf q
            passing = \case
              Label l _ -> l `elem` ["$label.noResponse", "$label.noDefender", "$label.declineToDefend"]
              _ -> False
        answerQuestion ident (fromMaybe 0 $ headMay [i | (i, c) <- zip [0 ..] cs, passing c])
        go (n - 1)
      Nothing -> pure ()

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

-- | Drang's attack on the player, answered by no one.
drangAttacks :: IdentityId -> GameAppT ()
drangAttacks p = do
  vid <- villainIdNow
  start [VillainMessage vid (VillainAttacks p)]
  settle

defeatDrang :: IdentityId -> GameAppT ()
defeatDrang p = do
  vid <- villainIdNow
  hp <- fromIntegral . max 0 . unHp . villainHp . toAttrs <$> getVillain
  start [VillainMessage vid (VillainDamaged (toSource p) (toDamage hp FromAbility))]
  settle

spec :: Spec
spec = describe "Brotherhood of Badoon" do
  scenarioSpec
  villainSpec
  encounterSpec
  bandOfBadoonSpec

scenarioSpec :: Spec
scenarioSpec = describe "the scenario" do
  it "waits on side A for Setup, then puts the ship into play and starts side B with its initial threat" do
    (app, _) <- dealtGameBeforeSetup id (ScenarioSetup "16058" [BandOfBadoon] Normal) [starter "spider-man"] 1 (const $ pure ())
    runGameApp app do
      let shownSchemeCodes = do
            view <- liftIO . publicGame =<< getGame
            pure [code | Object o <- [view], Just (Array schemes) <- [KeyMap.lookup "mainSchemes" o], Object scheme <- toList schemes, Just (String code) <- [KeyMap.lookup "code" scheme]]
      shownSchemeCodes `shouldReturn` ["16061a"]
      schemeId <- mainSchemeIdNow
      view <- liftIO . publicGame =<< getGame
      let setupButtons =
            [ (KeyMap.lookup "label" choice, KeyMap.lookup "target" choice)
            | Object o <- [view]
            , Just (Object questions) <- [KeyMap.lookup "question" o]
            , Object question <- KeyMap.elems questions
            , Just (Array choices) <- [KeyMap.lookup "choices" question]
            , Object choice <- toList choices
            ]
      setupButtons `shouldBe` [(Just $ String "$label.setupMainScheme", Just $ toJSON schemeId)]
      shipsBefore <- getsGame (HashMap.size . gameEnvironments)
      threatBefore <- mainSchemeThreatNow
      asked >>= \case
        Just (you, [TargetLabel (MainSchemeRef _) _]) -> do
          liftIO $ (shipsBefore, threatBefore) `shouldBe` (0, 0)
          answerQuestion you 0
        other -> error $ "expected the setup button, got " <> show other
      _ <- badoonShip
      threatAfter <- mainSchemeThreatNow
      shownSchemeCodes `shouldReturn` ["16061"]
      liftIO $ threatAfter `shouldBe` 2

  it "sets up Drang (I), Terrestrial Invasion with 2 threat per player, the Badoon Ship and the Milano" do
    tg <- drangGame ["spider-man", "captain-marvel"]
    inGame tg do
      code <- villainCode
      scheme <- mainSchemeCode
      threat <- mainSchemeThreatNow
      _ <- badoonShip
      milano <- supportsWith (toCardCode SupportCards.milano)
      deck <- map toCardCode <$> scenarioEncounterDeckNow
      liftIO do
        (code, scheme, threat, length milano) `shouldBe` ("16058", "16061", 4, 1)
        -- Brotherhood of Badoon, Band of Badoon and Ship Command are shuffled in
        all (`elem` deck) ["16065", "16118", "16145"] `shouldBe` True
        toCardCode EnvironmentCards.badoonShip `elem` deck `shouldBe` False

  it "expert: Drang (II) is the first villain, and finds Drang's Spear as he is revealed" do
    tg <- expertGame ["spider-man"]
    inGame tg do
      code <- villainCode
      spears <- attachmentsWith (toCardCode AttachmentCards.drangsSpear)
      vid <- villainIdNow
      liftIO do
        code `shouldBe` "16059"
        map (attachmentEnemy . toAttrs) spears `shouldBe` [Just (EnemyVillainId vid)]

  it "Charge Up: a barrage counter each time, and at 4, 2 indirect damage to each player and the counters removed" do
    tg <- drangGame ["spider-man", "captain-marvel"]
    inGame tg do
      chargeUpNow >> chargeUpNow >> chargeUpNow
      three <- barrageNow
      d1 <- damageOf (player1 tg)
      chargeUpNow
      four <- barrageNow
      d1' <- damageOf (player1 tg)
      d2' <- damageOf (player2 tg)
      liftIO $ (three, d1, four, d1', d2') `shouldBe` (3, 0, 0, 2, 2)

  it "Terrestrial Invasion: after step one of the villain phase, Charge Up" do
    tg <- drangGame ["spider-man"]
    inGame tg do
      before <- mainSchemeThreatNow
      resolve [AccelerateMainScheme]
      after <- mainSchemeThreatNow
      barrage <- barrageNow
      liftIO $ (after - before, barrage) `shouldBe` (2, 1)

  it "Terrestrial Invasion: the first player exhausts the Milano to remove 3 threat, a crisis icon notwithstanding" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      placeMainThreat 3
      _ <- revealNow p SideSchemeCards.blockade
      settle
      m <- mainSchemeIdNow
      -- Blockade's crisis icon: no thwarting of the main scheme by player cards
      asHero p
      choose "thwart" (isHeroThwart p)
      mainThwartable <- offers (isTargetChoice m)
      liftIO $ mainThwartable `shouldBe` False
      settle
      before <- mainSchemeThreatNow
      choose "Terrestrial Invasion's action" (isAbilityOf m)
      after <- mainSchemeThreatNow
      exhausted <- milanoExhausted
      again <- offers (isAbilityOf m)
      liftIO $ (before - after, exhausted, again) `shouldBe` (3, True, False)

  it "Terrestrial Invasion's action is the first player's only" do
    tg <- drangGame ["spider-man", "captain-marvel"]
    inGame tg do
      m <- mainSchemeIdNow
      firstOffered <- offers (isAbilityOf m)
      endTurn
      secondOffered <- offers (isAbilityOf m)
      liftIO $ (firstOffered, secondOffered) `shouldBe` (True, False)

  it "Terrestrial Invasion completed: Protect the Planet, with 4 threat per player, Charges Up as it is revealed" do
    tg <- drangGame ["spider-man"]
    inGame tg do
      placeMainThreat 6
      settle
      scheme <- mainSchemeCode
      threat <- mainSchemeThreatNow
      barrage <- barrageNow
      liftIO $ (scheme, threat, barrage) `shouldBe` ("16062", 4, 1)

  it "Protect the Planet: the Milano removes 3 threat or deals 3 damage to a minion" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      placeMainThreat 6
      settle
      m <- mainSchemeIdNow
      -- with no minion in play, the threat is removed
      choose "Protect the Planet's action" (isAbilityOf m)
      settle
      threat <- mainSchemeThreatNow
      liftIO $ threat `shouldBe` 1
      supportsWith (toCardCode SupportCards.milano) >>= traverse_ \s -> resolve [SupportMessage (supportId s) ReadiedSupport]
      sentry <- addMinion p MinionCards.badoonSentry
      choose "Protect the Planet's action" (isAbilityOf m)
      choose "damage a minion" (isLabel (countLabel "dealDamageToAMinion" 3))
      choose "the Sentry" (isTargetChoice sentry)
      settle
      dmg <- minionDamageOf sentry
      liftIO $ dmg `shouldBe` Just 3

  it "Protect the Planet completed, the players lose" do
    tg <- drangGame ["spider-man"]
    inGame tg do
      placeMainThreat 6
      settle
      placeMainThreat 4
      getsGame gameState `shouldReturn` Finished Lost

villainSpec :: Spec
villainSpec = describe "Drang" do
  it "(I): after he schemes, Charge Up; confused, he does not scheme, and nothing" do
    tg <- drangGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      schemed <- barrageNow
      resolve [GiveStatus (VillainRef vid) ConfusedStatus, VillainMessage vid VillainSchemes]
      confused <- barrageNow
      liftIO $ (schemed, confused) `shouldBe` (1, 1)

  it "(I): his attack is no Charge Up" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      drangAttacks p
      barrageNow `shouldReturn` 0

  it "(II): revealed, finds Drang's Spear and gains stalwart from it" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      defeatDrang p
      code <- villainCode
      vid <- villainIdNow
      spears <- attachmentsWith (toCardCode AttachmentCards.drangsSpear)
      resolve [GiveStatus (VillainRef vid) StunnedStatus]
      stunned <- villainStunnedNow
      liftIO do
        code `shouldBe` "16059"
        map (attachmentEnemy . toAttrs) spears `shouldBe` [Just (EnemyVillainId vid)]
        stunned `shouldBe` False

  it "(II): revealed with Drang's Spear in play, he is given a facedown boost card instead" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p AttachmentCards.drangsSpear
      settle
      defeatDrang p
      boosts <- length . villainBoostCards . toAttrs <$> getVillain
      spears <- attachmentsWith (toCardCode AttachmentCards.drangsSpear)
      liftIO $ (boosts, length spears) `shouldBe` (1, 1)

  it "(II) is the last stage in standard" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      defeatDrang p
      defeatDrang p
      getsGame gameState `shouldReturn` Finished Won

  it "(III), expert: the top 4 per player encounter cards discarded, each minion engaged with the player with the fewest" do
    tg <- expertGame ["spider-man", "captain-marvel"]
    let p1 = player1 tg
        p2 = player2 tg
    inGame tg do
      _ <- addMinion p1 MinionCards.badoonSentry
      let filler = TreacheryCards.hullBreach
      _ <- stackEncounterDeck ([MinionCards.badoonWarlord, filler, MinionCards.badoonEngineer] <> replicate 5 filler)
      defeatDrang p1
      code <- villainCode
      engaged <- getsGame (HashMap.fromList . map ((\a -> (toCardCode a.minionCardDef, a.minionEngagedIdentity)) . toAttrs) . HashMap.elems . gameMinions)
      liftIO do
        code `shouldBe` "16060"
        -- the Warlord to the second player, who had none; then one each, the first player
        -- breaking the tie (for himself)
        lookup (toCardCode MinionCards.badoonWarlord) engaged `shouldBe` Just p2
        lookup (toCardCode MinionCards.badoonEngineer) engaged `shouldBe` Just p1

  it "(III): each minion is put into play as it is discarded, before the next card is: a Badoon Grunt's dealt card comes off the deck between" do
    tg <- expertGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      let filler = TreacheryCards.hullBreach
      _ <- stackEncounterDeck ([MinionCards.badoonGrunt, filler, filler, filler, TreacheryCards.powerSiphon] <> replicate 5 filler)
      dealt <- length <$> dealtTo p
      defeatDrang p
      cards <- dealtTo p
      discard <- encounterDiscardCodes
      liftIO do
        length cards `shouldBe` dealt + 1
        -- the Grunt dealt the card after it, the discard took the next 3, Power Siphon last
        map toCardCode cards `shouldSatisfy` notElem (toCardCode TreacheryCards.powerSiphon)
        toCardCode TreacheryCards.powerSiphon `elem` discard `shouldBe` True

  it "(III): the encounter deck emptied by the discard, it is not continued with the new deck" do
    tg <- expertGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      let filler = TreacheryCards.hullBreach
      _ <- setEncounterDeck [filler, filler] [MinionCards.badoonWarlord, filler, filler, filler]
      defeatDrang p
      minions <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMinions)
      liftIO $ toCardCode MinionCards.badoonWarlord `elem` minions `shouldBe` False

  it "(III): after he attacks or schemes, Charge Up" do
    tg <- expertGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- stackEncounterDeck (replicate 4 TreacheryCards.hullBreach)
      defeatDrang p
      vid <- villainIdNow
      start0 <- barrageNow
      asHero p
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      drangAttacks p
      attacked <- barrageNow
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      schemed <- barrageNow
      liftIO $ (attacked - start0, schemed - attacked) `shouldBe` (1, 1)
 where
  villainStunnedNow = villainStunned . toAttrs <$> getVillain

encounterSpec :: Spec
encounterSpec = describe "the Brotherhood of Badoon set" do
  it "Drang's Spear: a hero spends [mental][physical][physical] to discard it" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p AttachmentCards.drangsSpear
      settle
      [spear] <- attachmentsWith (toCardCode AttachmentCards.drangsSpear)
      let sid = toId spear
      alterEgo <- offers (isAbilityOf sid)
      asHero p
      clearHand p
      payment <- traverse (putInHand p) ["01089", "01090"]
      choose "discard the Spear" (isAbilityOf sid)
      payWith payment
      settle
      left <- attachmentsWith (toCardCode AttachmentCards.drangsSpear)
      liftIO $ (alterEgo, length left) `shouldBe` (False, 0)

  it "Badoon Engineer: Charge Up as it engages you, after it schemes, and as its boost" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      m <- addMinion p MinionCards.badoonEngineer
      engaged <- barrageNow
      resolve [MinionMessage m MinionSchemes]
      schemed <- barrageNow
      vid <- villainIdNow
      card <- encounterCard MinionCards.badoonEngineer
      resolve [RevealBoostCard card (EnemyVillainId vid), ClearBoosts]
      boosted <- barrageNow
      liftIO $ (engaged, schemed, boosted) `shouldBe` (1, 2, 3)

  it "Badoon Engineer: Charge Up after it attacks" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      m <- addMinion p MinionCards.badoonEngineer
      asHero p
      start [MinionMessage m (MinionAttacks p)]
      settle
      barrageNow `shouldReturn` 2

  it "Blockade: Hinder 2 per player on its 2, and the Milano removes 3" do
    tg <- drangGame ["spider-man", "captain-marvel"]
    inGame tg do
      sid <- sideSchemeOf <$> revealNow (player1 tg) SideSchemeCards.blockade
      settle
      threat <- sideSchemeThreat sid
      choose "Blockade's action" (isAbilityOf sid)
      settle
      after <- sideSchemeThreat sid
      liftIO $ (threat, after) `shouldBe` (6, 3)

  it "Oppressive Armada: Hinder 3 per player on its 1; the Milano's 3 defeats it from 4" do
    tg <- drangGame ["spider-man"]
    inGame tg do
      sid <- sideSchemeOf <$> revealNow (player1 tg) SideSchemeCards.oppressiveArmada
      settle
      threat <- sideSchemeThreat sid
      choose "Oppressive Armada's action" (isAbilityOf sid)
      settle
      after <- sideSchemeThreat sid
      liftIO $ (threat, after) `shouldBe` (4, 1)

  it "Bombardment: 3 threat per player, and after step one of the villain phase, Charge Up" do
    tg <- drangGame ["spider-man", "captain-marvel"]
    inGame tg do
      sid <- sideSchemeOf <$> revealNow (player1 tg) SideSchemeCards.bombardment
      settle
      threat <- sideSchemeThreat sid
      resolve [AccelerateMainScheme]
      -- its own and Terrestrial Invasion's
      barrage <- barrageNow
      liftIO $ (threat, barrage) `shouldBe` (6, 2)

  it "Spatial Positioning: 3 threat per player, and its amplify icon adds a boost icon to each boost card" do
    tg <- drangGame ["spider-man"]
    inGame tg do
      sid <- sideSchemeOf <$> revealNow (player1 tg) SideSchemeCards.spatialPositioning
      settle
      threat <- sideSchemeThreat sid
      vid <- villainIdNow
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      before <- mainSchemeThreatNow
      resolve [VillainMessage vid VillainSchemes]
      after <- mainSchemeThreatNow
      -- Drang's 1 SCH, the boost card's 1 icon and the amplify icon's 1
      liftIO $ (threat, after - before) `shouldBe` (3, 3)

bandOfBadoonSpec :: Spec
bandOfBadoonSpec = describe "Band of Badoon" do
  it "Badoon Assassin: engaging your hero, it attacks you with +2 ATK; the +2 is for that attack only" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      card <- encounterCard MinionCards.badoonAssassin
      start [RevealEncounterCard p card]
      settle
      engagedAttack <- damageOf p
      start [MinionMessage (minionOf card) (MinionAttacks p)]
      settle
      later <- damageOf p
      liftIO $ (engagedAttack, later - engagedAttack) `shouldBe` (3, 1)

  it "Badoon Assassin: engaging Peter Parker, no attack" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- addMinion p MinionCards.badoonAssassin
      damageOf p `shouldReturn` 0

  it "Badoon Assassin's boost: the villain's attack gains piercing (and overkill and ranged)" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      resolve [IdentityMessage p IdentityBecomeTough]
      _ <- stackEncounterDeck [MinionCards.badoonAssassin]
      drangAttacks p
      -- the tough status card discarded, Drang's 2 ATK dealt
      damageOf p `shouldReturn` 2

  it "Badoon Grunt: engaging you alone, it deals you a facedown encounter card; with another minion, none" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      before <- length <$> dealtTo p
      _ <- addMinion p MinionCards.badoonGrunt
      alone <- length <$> dealtTo p
      _ <- addMinion p MinionCards.badoonGrunt
      withOther <- length <$> dealtTo p
      liftIO $ (alone - before, withOther - alone) `shouldBe` (1, 0)

  it "Badoon Grunt's boost puts it into play engaged with you" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard MinionCards.badoonGrunt
      resolve [RevealBoostCard card (EnemyVillainId vid), ClearBoosts]
      engaged <- getsGame (fmap (minionEngagedIdentity . toAttrs) . HashMap.lookup (minionOf card) . gameMinions)
      liftIO $ engaged `shouldBe` Just p

  it "Badoon Lieutenant: patrol keeps you from thwarting the main scheme" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      free <- offers (isHeroThwart p)
      _ <- addMinion p MinionCards.badoonLieutenant
      patrolled <- offers (isHeroThwart p)
      liftIO $ (free, patrolled) `shouldBe` (True, False)

  it "Badoon Lieutenant's boost: +2 boost icons for a scheme, none more for an attack" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      vid <- villainIdNow
      _ <- stackEncounterDeck [MinionCards.badoonLieutenant]
      before <- mainSchemeThreatNow
      resolve [VillainMessage vid VillainSchemes]
      after <- mainSchemeThreatNow
      asHero p
      _ <- stackEncounterDeck [MinionCards.badoonLieutenant]
      drangAttacks p
      dmg <- damageOf p
      -- 1 SCH, 1 icon, 2 more; 2 ATK and 1 icon
      liftIO $ (after - before, dmg) `shouldBe` (4, 3)

  it "Badoon Sentry: retaliate 1" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      m <- addMinion p MinionCards.badoonSentry
      asHero p
      choose "attack" (isHeroAttack p)
      choose "the Sentry" (isTargetChoice m)
      settle
      damageOf p `shouldReturn` 1

  it "Badoon Sentry's boost: the villain is made tough; already tough, +2 boost icons" do
    -- two players, so the scheme's threshold of 8 per player is not reached
    tg <- drangGame ["spider-man", "captain-marvel"]
    inGame tg do
      vid <- villainIdNow
      _ <- stackEncounterDeck [MinionCards.badoonSentry]
      t0 <- mainSchemeThreatNow
      resolve [VillainMessage vid VillainSchemes]
      t1 <- mainSchemeThreatNow
      tough <- villainIsTough <$> getVillain
      _ <- stackEncounterDeck [MinionCards.badoonSentry]
      resolve [VillainMessage vid VillainSchemes]
      t2 <- mainSchemeThreatNow
      liftIO $ (t1 - t0, tough, t2 - t1) `shouldBe` (2, True, 4)

  it "Badoon Warlord: its attack gains overkill, past a defending ally to the hero" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      m <- addMinion p MinionCards.badoonWarlord
      asHero p
      _ <- putIntoPlay p "01002"
      pass
      cat <- allyIn "01002"
      start [MinionMessage m (MinionAttacks p)]
      chooseIf isNoResponse
      choose "Black Cat defends" \case
        AllyDefend a _ -> a == allyId cat
        _ -> False
      settle
      -- 3 ATK on Black Cat's 2 hit points
      damageOf p `shouldReturn` 1

  it "Badoon Warlord's boost: +2 boost icons for an attack, none for a scheme" do
    tg <- drangGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      vid <- villainIdNow
      _ <- stackEncounterDeck [MinionCards.badoonWarlord]
      before <- mainSchemeThreatNow
      resolve [VillainMessage vid VillainSchemes]
      after <- mainSchemeThreatNow
      asHero p
      _ <- stackEncounterDeck [MinionCards.badoonWarlord]
      drangAttacks p
      dmg <- damageOf p
      liftIO $ (after - before, dmg) `shouldBe` (1, 4)
