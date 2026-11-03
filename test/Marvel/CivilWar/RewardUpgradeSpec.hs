module Marvel.CivilWar.RewardUpgradeSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
import Data.List (nub)
import Marvel.Ally.Types qualified as A
import Marvel.CivilWar.Helpers qualified
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf,leaderHp)
import Marvel.Area (DeckKey (..))
import Marvel.Hp (HP (..))
import Marvel.Attack qualified as Attack
import Marvel.Villain.Types qualified as V
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Count (selectCount)
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Trait qualified as T
import TestImport

leaders :: TestGame -> CardCode -> GameAppT (VillainId,VillainId)
leaders tg ownCode = do
  enemy <- withLeader "56059"
  own <- addSecondBoard tg
  resolve [ReplaceVillain own ownCode Swapped,VillainMessage own SetVillainHp]
  pure (own,enemy)

recruit :: TestGame -> GameAppT (PlayerCard,PlayerCard,EncounterCard)
recruit tg = do
  let you = player1 tg
  _ <- leaders tg "56092"
  energy <- encounterCard C.cw56098
  resolve [RevealEncounterCard (player2 tg) energy]
  ally <- putIntoPlay you "01050"
  L.runQueueT (L.capture $ L.damageCharacter (allyIdOf ally) GameRef $ toDamage 3 FromAbility) >>= resolve
  card <- putInHand you "56135"
  playOn card (allyIdOf ally) []
  pass
  pure (card,ally,energy)

allyDamageOf :: AllyId -> GameAppT (Maybe Natural)
allyDamageOf aid = getsGame $ fmap (A.allyDamage . toAttrs) . lookup aid . gameAllies

jsonDifferences :: Value -> Value -> [Text]
jsonDifferences left right | left == right = []
jsonDifferences (Object left) (Object right) = concat
  [case (KeyMap.lookup key left,KeyMap.lookup key right) of
    (Just a,Just b) -> map ((Key.toText key <> ".") <>) $ jsonDifferences a b
    _ -> [Key.toText key]
  | key <- nub $ KeyMap.keys left <> KeyMap.keys right]
jsonDifferences (Array left) (Array right)
  | length left /= length right = ["array length"]
  | otherwise = concat [map ((tshow i <> ".") <>) $ jsonDifferences a b | (i,(a,b)) <- zip [0 :: Int ..] $ zip (toList left) (toList right)]
jsonDifferences _ _ = ["value"]

spec :: Spec
spec = describe "Civil War leader reward upgrades" do
  for_ [0,2] $ \armor ->
    it ("Provoked Response redirects the actual boosted attack and caps damage after armor: " <> show armor) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        (own,enemy) <- leaders tg "56092"
        -- Keep both attacks on one stage so advancement cannot reset the hit-point dial.
        withGame_ $ entitiesL . villainsL . ix own %~ (\(V.Villain v) -> V.Villain $ V.overVillainAttrs (\a -> a {V.villainHp = HP 100,V.villainMaxHp = HP 100}) v)
        card <- putIntoPlay you "56133"
        L.runQueueT (L.capture $ do
          L.lastingModifiers GameRef (SourceEntity $ VillainRef enemy) [AttackModifier 8] False
          L.lastingModifiers GameRef (SourceEntity $ VillainRef own) [DamageReduction armor] False) >>= resolve
        _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
        changeForm you
        before <- leaderHp own
        start [VillainMessage enemy $ VillainAttacks you]
        choose "redirect the attack to Captain Marvel" (isAbilityOfCard card)
        pass
        damageOf you `shouldReturn` 0
        leaderHp own `shouldReturn` (before - 3)
        map pcCardId <$> discardOf you >>= (`shouldSatisfy` elem (pcCardId card))
        getEnemyAttack (EnemyVillainId enemy) `shouldReturn` Nothing
        L.runQueueT (L.capture $ L.villainAttacksCharacter enemy you own) >>= start
        pass
        leaderHp own >>= (`shouldSatisfy` (< before - 6))

  it "Provoked Response does not spend itself when stun cancels the attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (_,enemy) <- leaders tg "56092"
      card <- putIntoPlay you "56133"
      changeForm you
      resolve [GiveStatus (VillainRef enemy) StunnedStatus]
      start [VillainMessage enemy $ VillainAttacks you]
      offers (isAbilityOfCard card) `shouldReturn` False
      map upgradeId <$> upgradesWith "56133" `shouldReturn` [upgradeOf card]
      damageOf you `shouldReturn` 0

  for_ [False,True] $ \tough ->
    it ("an enemy attack cap tracks actual cumulative damage and preserves tough: " <> show tough) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        (own,enemy) <- leaders tg "56092"
        let current = Attack.attack (EnemyVillainId enemy) (VillainCharacter own) 8
        withGame_ $ entitiesL . villainsL . ix enemy %~ (\(V.Villain v) -> V.Villain $ V.overVillainAttrs (\a -> a {V.villainAttacking = Just current}) v)
        L.runQueueT (L.capture $ L.capAttackDamageToEnemy (EnemyVillainId enemy) (EnemyVillainId own) 3) >>= resolve
        when tough $ resolve [GiveStatus (VillainRef own) ToughStatus]
        before <- leaderHp own
        resolve [VillainMessage own $ VillainDamaged (VillainRef enemy) (toDamage 2 FromAttack)]
        leaderHp own `shouldReturn` (before - if tough then 0 else 2)
        resolve [VillainMessage own $ VillainDamaged (VillainRef enemy) (toDamage 8 FromAttack)]
        leaderHp own `shouldReturn` (before - 3)
        resolve [VillainMessage own $ VillainDamaged (VillainRef enemy) (toDamage 8 FromAttack)]
        leaderHp own `shouldReturn` (before - 3)
        -- Damage outside that attack does not consume or inherit its budget.
        resolve [VillainMessage own $ VillainDamaged GameRef (toDamage 2 FromAbility)]
        leaderHp own `shouldReturn` (before - 5)

  it "No, You Move! reattaches an in-play shield to the friendly Captain America before resolving the attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,enemy) <- leaders tg "56137"
      shield <- encounterCard C.cw56143
      resolve [RevealEncounterCard (player2 tg) shield]
      L.runQueueT (L.capture $ L.attachToCharacter (attachmentIdOf shield) you) >>= resolve
      resolve [RemoveStatus (VillainRef own) ToughStatus]
      card <- putIntoPlay you "56207"
      changeForm you
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      before <- leaderHp own
      start [VillainMessage enemy $ VillainAttacks you]
      choose "redirect the attack and recover Cap's shield" (isAbilityOfCard card)
      choose "give Captain America tough for the returned shield" (isTargetChoice own)
      pass
      damageOf you `shouldReturn` 0
      leaderHp own `shouldReturn` before
      getAttachedCharacter (attachmentIdOf shield) `shouldReturn` Just (VillainCharacter own)
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 0
      map pcCardId <$> discardOf you >>= (`shouldSatisfy` elem (pcCardId card))

  for_ [("56133","56092"),("56207","56137")] $ \(code,family) ->
    it ("plays the zero-cost leader redirection reward from hand: " <> show code) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        _ <- leaders tg family
        card <- putInHand you code
        playWith card []
        map upgradeId <$> upgradesWith code `shouldReturn` [upgradeOf card]

  it "No, You Move! finds the Shield in the friendly leader's encounter deck" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,enemy) <- leaders tg "56137"
      shield <- encounterCard C.cw56143
      filler <- encounterCard M.shocker
      resolve [CreateDeck (VillainDeckKey own) [shield,filler]]
      card <- putIntoPlay you "56207"
      changeForm you
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      before <- leaderHp own
      start [VillainMessage enemy $ VillainAttacks you]
      choose "recover the Shield from the friendly leader's deck" (isAbilityOfCard card)
      choose "give Captain America tough after finding the Shield" (isTargetChoice own)
      pass
      getAttachedCharacter (attachmentIdOf shield) `shouldReturn` Just (VillainCharacter own)
      leaderHp own `shouldReturn` before
      damageOf you `shouldReturn` 0

  it "No, You Move! does not trigger another Shield attachment response when it is already on Captain America" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,enemy) <- leaders tg "56137"
      shield <- encounterCard C.cw56143
      resolve [RevealEncounterCard (player2 tg) shield,RemoveStatus (VillainRef own) ToughStatus]
      card <- putIntoPlay you "56207"
      changeForm you
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      before <- leaderHp own
      start [VillainMessage enemy $ VillainAttacks you]
      choose "resolve the attack against Captain America" (isAbilityOfCard card)
      pass
      leaderHp own `shouldReturn` (before - 2)
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 0

  it "does not consume another Provoked Response after the first copy has redirected the attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (_,enemy) <- leaders tg "56092"
      first <- putIntoPlay you "56133"
      second <- putIntoPlay you "56133"
      changeForm you
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      start [VillainMessage enemy $ VillainAttacks you]
      choose "use one redirection" (isAbilityOfCard first)
      offers (isAbilityOfCard second) `shouldReturn` False
      pass
      map upgradeId <$> upgradesWith "56133" `shouldReturn` [upgradeOf second]

  for_ [("56133","56092"),("56207","56137")] $ \(code,family) -> do
    it ("does not redirect a minion attack with " <> show code) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        _ <- leaders tg family
        card <- putIntoPlay you code
        minion <- encounterCard M.hydraMercenary
        resolve [RevealEncounterCard you minion]
        changeForm you
        let mid = MinionId $ unCardId $ ecCardId minion
        start [MinionMessage mid $ MinionAttacks you]
        offers (isAbilityOfCard card) `shouldReturn` False
        pass
        map upgradeId <$> upgradesWith code `shouldReturn` [upgradeOf card]
        damageOf you >>= (`shouldSatisfy` (> 0))

    it ("suppresses the printed redirection while blanked: " <> show code) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        (_,enemy) <- leaders tg family
        card <- putIntoPlay you code
        L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ UpgradeRef $ upgradeOf card) [Blank] False) >>= resolve
        changeForm you
        _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
        start [VillainMessage enemy $ VillainAttacks you]
        offers (isAbilityOfCard card) `shouldReturn` False
        pass
        map upgradeId <$> upgradesWith code `shouldReturn` [upgradeOf card]
        damageOf you >>= (`shouldSatisfy` (> 0))

  it "preserves attack enemy damage caps in saves and defaults older attacks to no cap" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (own,enemy) <- leaders tg "56092"
      let current = (Attack.attack (EnemyVillainId enemy) (VillainCharacter own) 8)
            {Attack.attackEnemyDamageCaps = [(EnemyVillainId own,3,2)]}
      fromJSON (toJSON current) `shouldBe` Success current
      case toJSON current of
        Object obj -> case fromJSON (Object $ KeyMap.delete "attackEnemyDamageCaps" obj) of
          Success restored -> Attack.attackEnemyDamageCaps restored `shouldBe` []
          Error err -> error err
        _ -> error "attack must serialize as an object"

  it "High-Tech Suit attaches to the friendly Iron Man and counts as exactly one Tech attachment" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,enemy) <- leaders tg "56059"
      card <- putIntoPlay you "56131"
      let UpgradeId uuid = upgradeOf card
          aid = AttachmentId uuid
      getUpgradeAttachedEnemy (upgradeOf card) `shouldReturn` Just (EnemyVillainId own)
      selectList (AttachmentOnEnemy (EnemyVillainId own) <> AttachmentWithTrait T.Tech) `shouldReturn` [aid]
      selectList (AttachmentOnEnemy (EnemyVillainId enemy) <> AttachmentWithTrait T.Tech) `shouldReturn` []
      getAttachedCharacter aid `shouldReturn` Just (VillainCharacter own)
      getModifiers own >>= (`shouldSatisfy` elem (SchemeModifier 1))
      getModifiers (upgradeOf card) >>= (`shouldSatisfy` elem (TraitModifier T.Tech))
      getsGame (\g -> toCardCode <$> abilityCardDef g (AttachmentRef aid)) `shouldReturn` Just "56131"
      getsGame (length . gameUpgrades) `shouldReturn` 1
      getsGame (length . gameAttachments) `shouldReturn` 0

  it "High-Tech Suit loses its attachment classification and scheme bonus while blanked" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (own,_) <- leaders tg "56059"
      card <- putIntoPlay (player1 tg) "56131"
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ UpgradeRef $ upgradeOf card) [Blank] False) >>= resolve
      selectList (AttachmentOnEnemy (EnemyVillainId own) <> AttachmentWithTrait T.Tech) `shouldReturn` []
      getModifiers own >>= (`shouldSatisfy` notElem (SchemeModifier 1))
      getModifiers (upgradeOf card) >>= (`shouldSatisfy` notElem (TraitModifier T.Tech))

  it "High-Tech Suit is selectable by attachment-removal effects and discards the original player card" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56059"
      card <- putIntoPlay you "56131"
      let UpgradeId uuid = upgradeOf card
          aid = AttachmentId uuid
      selectList (AttachmentOnEnemyWithHeroAbility $ EnemyVillainId own) `shouldReturn` [aid]
      inAnyArea (selectList AttachmentWithPrintedHeroActionOrResponse) `shouldReturn` [aid]
      resolve [RemoveFromPlay $ AttachmentRef aid]
      map pcCardId <$> discardOf you >>= (`shouldSatisfy` elem (pcCardId card))
      getsGame (length . gameUpgrades) `shouldReturn` 0
      selectList (AttachmentWithId aid) `shouldReturn` []
      let def = lookupPlayerCard "56131"
      cdTraits def `shouldBe` mempty
      cdCardType def `shouldBe` UpgradeType

  for_ [False,True] $ \opponent ->
    it ("High-Tech Suit accepts its energy and mental Hero Action payment from " <> if opponent then "an opposing player" else "its owner") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
            actor = if opponent then player2 tg else you
        _ <- leaders tg "56059"
        card <- putIntoPlay you "56131"
        start [SetActivePlayer actor,IdentityMessage actor BeginTurn]
        changeForm actor
        energy <- putInHand actor "01088"
        mental <- putInHand actor "01089"
        choose "discard High-Tech Suit with its printed Hero Action" (isAbilityOfCard card)
        payWith [energy,mental]
        pass
        map pcCardId <$> discardOf you >>= (`shouldSatisfy` elem (pcCardId card))
        when opponent $ map pcCardId <$> discardOf actor >>= (`shouldSatisfy` notElem (pcCardId card))
        upgradesWith "56131" `shouldReturn` []

  it "plays High-Tech Suit on the friendly leader before paying its cost" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56059"
      card <- putInHand you "56131"
      resource <- putInHand you "01088"
      playOn card own [resource]
      getUpgradeAttachedEnemy (upgradeOf card) `shouldReturn` Just (EnemyVillainId own)
      selectList (UpgradeWithTrait T.Tech <> UpgradeWithId (upgradeOf card)) `shouldReturn` [upgradeOf card]

  it "Electromagnetic Blast can discard an opposing player’s High-Tech Suit from the local leader" do
    tg <- competitive ["spider-man","magneto"]
    inGame tg do
      let owner = player1 tg
          actor = player2 tg
      _ <- leaders tg "56059"
      suit <- putIntoPlay owner "56131"
      start [SetActivePlayer actor,IdentityMessage actor BeginTurn]
      changeForm actor
      area <- getAreaOf actor
      schemes <- getMainSchemesIn (areaId area)
      let scheme = SchemeMainSchemeId $ testHead schemes
      threat <- selectCount SchemeThreat (SchemeWithId scheme)
      when (threat > 0) $ L.runQueueT (L.capture $ L.removeThreat actor GameRef threat $ SchemeWithId scheme) >>= resolve
      L.runQueueT (L.capture $ L.placeThreatOn GameRef scheme 3) >>= resolve
      blast <- putInHand actor "49008"
      resource <- putInHand actor "01089"
      playWith blast [resource]
      choose "remove the last threat" (isTargetChoice $ schemeRef scheme)
      let UpgradeId uuid = upgradeOf suit
      choose "discard the attached suit" (isTargetChoice $ AttachmentRef $ AttachmentId uuid)
      pass
      map pcCardId <$> discardOf owner >>= (`shouldSatisfy` elem (pcCardId suit))
      upgradesWith "56131" `shouldReturn` []

  it "High-Tech Suit satisfies Supersonic Punch’s Tech condition on its attached leader’s board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- leaders tg "56059"
      _ <- putIntoPlay (player1 tg) "56131"
      let you = player2 tg
      resolve [SetActivePlayer you]
      _ <- stackEncounterDeck [M.hydraMercenary]
      punch <- encounterCard C.cw56071
      resolve [RevealEncounterCard you punch]
      isConfused you `shouldReturn` True

  it "caps Iron Man’s bonus at three Tech attachments including High-Tech Suit" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (own,_) <- leaders tg "56059"
      sequence_ $ replicate 4 $ putIntoPlay (player1 tg) "56131"
      getModifiers own >>= (`shouldSatisfy` elem (SchemeModifier 3))
      length <$> selectList (AttachmentOnEnemy (EnemyVillainId own) <> AttachmentWithTrait T.Tech) `shouldReturn` 4

  it "preserves one High-Tech Suit entity, its host and physical ownership after saving" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (own,_) <- leaders tg "56059"
      card <- putIntoPlay (player1 tg) "56131"
      fixture <- getsGame gameScenario
      nativeScenario <- case fromJSON (toJSON fixture) of
        Success value -> pure value
        Error err -> error err
      withGame_ $ scenarioL .~ nativeScenario
      saved <- getsGame toJSON
      restored <- case fromJSON saved of
        Success value -> pure value
        Error err -> error err
      jsonDifferences saved (toJSON (restored :: Game)) `shouldBe` []
      withGame_ $ const restored
      let UpgradeId uuid = upgradeOf card
          aid = AttachmentId uuid
      getAttachedCharacter aid `shouldReturn` Just (VillainCharacter own)
      getsGame (\g -> pcOwner <$> (cardInPlay g (AttachmentRef aid) >>= \case PlayerCard pc -> Just pc; _ -> Nothing)) `shouldReturn` Just (Just $ player1 tg)
      getsGame (length . gameUpgrades) `shouldReturn` 1
      getsGame (length . gameAttachments) `shouldReturn` 0

  it "does not offer High-Tech Suit’s Hero Action to an alter-ego" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56059"
      card <- putIntoPlay you "56131"
      _ <- putInHand you "01088"
      _ <- putInHand you "01089"
      offers (isAbilityOfCard card) `shouldReturn` False

  for_ [False,True] $ \removed ->
    it (if removed then "removes a counted attachment from the game as its original upgrade" else "returns a counted attachment to its original owner's hand") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        _ <- leaders tg "56059"
        card <- putIntoPlay you "56131"
        let UpgradeId uuid = upgradeOf card
            target = AttachmentRef $ AttachmentId uuid
        resolve [if removed then RemoveFromGame target else ReturnToHand target]
        inAnyArea (selectList $ UpgradeWithId $ upgradeOf card) `shouldReturn` []
        map pcCardId <$> handOf you >>= (`shouldSatisfy` (if removed then notElem else elem) (pcCardId card))
        map pcCardId <$> discardOf you >>= (`shouldSatisfy` notElem (pcCardId card))

  it "does not put High-Tech Suit into play without a friendly Iron Man" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- leaders tg "56092"
      let you = player1 tg
      card <- putInHand you "56131"
      resolve [PutCardIntoPlay you card NoPayment Nothing True]
      map pcCardId <$> handOf you >>= (`shouldSatisfy` elem (pcCardId card))
      getsGame (length . gameUpgrades) `shouldReturn` 0

  it "Alpha Flight Recruit heals once after any number of energy counters are placed on the friendly leader's attachment" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (card,ally,energy) <- recruit tg
      start [PlaceCounters (AttachmentRef $ attachmentIdOf energy) 2]
      choose "heal the attached ally" (isAbilityOfCard card)
      pass
      allyDamageOf (allyIdOf ally) `shouldReturn` Just 2
      getCountersOn (attachmentIdOf energy) `shouldReturn` 2

  it "lets the controller decline Alpha Flight Recruit's response" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (card,ally,energy) <- recruit tg
      start [PlaceCounters (AttachmentRef $ attachmentIdOf energy) 2]
      offers (isAbilityOfCard card) `shouldReturn` True
      pass
      allyDamageOf (allyIdOf ally) `shouldReturn` Just 3

  it "does not offer the heal for zero counters or counter removal" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (card,ally,energy) <- recruit tg
      start [PlaceCounters (AttachmentRef $ attachmentIdOf energy) 0]
      offers (isAbilityOfCard card) `shouldReturn` False
      start [RemoveCounters (AttachmentRef $ attachmentIdOf energy) 1]
      offers (isAbilityOfCard card) `shouldReturn` False
      allyDamageOf (allyIdOf ally) `shouldReturn` Just 3

  it "reacts to counters placed by Raw Power during payment" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (card,ally,energy) <- recruit tg
      changeForm you
      resource <- putInHand you "56136"
      purchase <- putInHand you "01093"
      playWith purchase [resource]
      choose "resolve Raw Power" (isLabel "$label.cwRawPower")
      choose "heal after the resource places its counters" (isAbilityOfCard card)
      pass
      allyDamageOf (allyIdOf ally) `shouldReturn` Just 2
      getCountersOn (attachmentIdOf energy) `shouldReturn` 2
      length <$> upgradesWith "01093" `shouldReturn` 1

  it "reacts after Energy Channel gains a counter from a leader attacking that Captain Marvel" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (card,ally,energy) <- recruit tg
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      attack <- encounterCard C.cw56126
      start [RevealEncounterCard you attack]
      choose "resolve Energy Channel after the attack" (isAbilityOf $ attachmentIdOf energy)
      choose "heal the attached ally after the energy counter" (isAbilityOfCard card)
      pass
      allyDamageOf (allyIdOf ally) `shouldReturn` Just 2
      getCountersOn (attachmentIdOf energy) `shouldReturn` 1

  it "Hard to Hit attaches to the friendly Spider-Woman and gives her tough" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,enemy) <- leaders tg "56168"
      card <- putInHand you "56213"
      payment <- putInHand you "01088"
      playOn card own [payment]
      pass
      getUpgradeAttachedEnemy (upgradeOf card) `shouldReturn` Just (EnemyVillainId own)
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1
      getCharacterStatusCount (VillainCharacter enemy) ToughStatus `shouldReturn` 0

  it "Hard to Hit refills two Finesse counters after the attached leader attacks and then discards" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,enemy) <- leaders tg "56168"
      finesse <- encounterCard C.cw56174
      resolve [RevealEncounterCard (player2 tg) finesse,PlaceCounters (AttachmentRef $ attachmentIdOf finesse) 2]
      card <- putInHand you "56213"
      payment <- putInHand you "01088"
      playOn card own [payment]
      pass
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      blast <- putInHand you "56212"
      resource <- putInHand you "01089"
      playWith blast [resource]
      choose "spend Spider-Woman's Finesse counters for this attack" (isAbilityOf own)
      pass
      getCharacterStatusCount (VillainCharacter enemy) ConfusedStatus `shouldReturn` 0
      choose "resolve Hard to Hit after her attack" (isAbilityOfCard card)
      pass
      getCountersOn (attachmentIdOf finesse) `shouldReturn` 2
      upgradesWith "56213" `shouldReturn` []
      getCharacterStatusCount (VillainCharacter enemy) ConfusedStatus `shouldReturn` 1

  it "does not offer Alpha Flight Recruit when its attached ally has no damage" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (card,ally,energy) <- recruit tg
      L.runQueueT (L.capture $ L.heal (allyIdOf ally) 3) >>= resolve
      start [PlaceCounters (AttachmentRef $ attachmentIdOf energy) 2]
      offers (isAbilityOfCard card) `shouldReturn` False
      allyDamageOf (allyIdOf ally) `shouldReturn` Just 0

  it "does not heal for counters on a hero's Energy Channel upgrade" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (card,ally,_) <- recruit tg
      heroChannel <- putIntoPlay (player2 tg) "01018"
      start [PlaceCounters (UpgradeRef $ upgradeOf heroChannel) 2]
      offers (isAbilityOfCard card) `shouldReturn` False
      allyDamageOf (allyIdOf ally) `shouldReturn` Just 3

  it "preserves a pending Alpha Flight Recruit response through a Game JSON round trip" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (card,ally,energy) <- recruit tg
      -- The competition fixture carries setup fields in an ad-hoc scenario
      -- wrapper. Production game state already stores that setup in competition.
      fixture <- getsGame gameScenario
      nativeScenario <- case fromJSON (toJSON fixture) of
        Success value -> pure value
        Error err -> error err
      withGame_ $ scenarioL .~ nativeScenario
      start [PlaceCounters (AttachmentRef $ attachmentIdOf energy) 2]
      offers (isAbilityOfCard card) `shouldReturn` True
      saved <- getsGame toJSON
      restored <- case fromJSON saved of
        Success value -> pure value
        Error err -> error err
      jsonDifferences saved (toJSON (restored :: Game)) `shouldBe` []
      withGame_ $ const restored
      choose "resolve the saved heal response" (isAbilityOfCard card)
      pass
      allyDamageOf (allyIdOf ally) `shouldReturn` Just 2

  it "discards Alpha Flight Recruit with its ally and cannot respond afterward" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      (card,ally,energy) <- recruit tg
      L.runQueueT (L.capture $ L.removeFromPlay $ allyIdOf ally) >>= resolve
      start [PlaceCounters (AttachmentRef $ attachmentIdOf energy) 2]
      offers (isAbilityOfCard card) `shouldReturn` False
      upgradesWith "56135" `shouldReturn` []
      discardOf (player1 tg) >>= (`shouldSatisfy` any ((== pcCardId card) . pcCardId))

  it "Hard to Hit discards after its leader's attack even when Finesse is absent" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56168"
      card <- putInHand you "56213"
      payment <- putInHand you "01088"
      playOn card own [payment]
      pass
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      blast <- putInHand you "56212"
      resource <- putInHand you "01089"
      playWith blast [resource]
      choose "resolve Spider-Woman's attack interrupt" (isAbilityOf own)
      pass
      choose "discard Hard to Hit after the attack" (isAbilityOfCard card)
      pass
      upgradesWith "56213" `shouldReturn` []

  it "Hard to Hit stays in play when its leader's attack is replaced by stun" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56168"
      card <- putInHand you "56213"
      payment <- putInHand you "01088"
      playOn card own [payment]
      pass
      resolve [GiveStatus (VillainRef own) StunnedStatus]
      blast <- putInHand you "56212"
      resource <- putInHand you "01089"
      playWith blast [resource]
      pass
      offers (isAbilityOfCard card) `shouldReturn` False
      length <$> upgradesWith "56213" `shouldReturn` 1
      getCharacterStatusCount (VillainCharacter own) StunnedStatus `shouldReturn` 0

  it "does not offer Hard to Hit when Spider-Woman is only the enemy leader" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "56168"
      own <- addSecondBoard tg
      resolve [ReplaceVillain own "56059" Swapped,VillainMessage own SetVillainHp]
      card <- putInHand you "56213"
      _ <- putInHand you "01088"
      offers (isPlay card) `shouldReturn` False

  it "puts Hard to Hit into play attached to the friendly Spider-Woman without paying its play cost" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56168"
      card <- putIntoPlay you "56213"
      getUpgradeAttachedEnemy (upgradeOf card) `shouldReturn` Just (EnemyVillainId own)
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1

  it "leaves Hard to Hit in its prior zone when put into play has no valid attach target" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56059"
      card <- putInHand you "56213"
      L.runQueueT (L.capture $ L.putIntoPlay you card) >>= resolve
      upgradesWith "56213" `shouldReturn` []
      handOf you >>= (`shouldSatisfy` any ((== pcCardId card) . pcCardId))
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 0

  it "leaves Alpha Flight Recruit in hand when put into play has no ally to attach to" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56092"
      card <- putInHand you "56135"
      L.runQueueT (L.capture $ L.putIntoPlay you card) >>= resolve
      upgradesWith "56135" `shouldReturn` []
      handOf you >>= (`shouldSatisfy` any ((== pcCardId card) . pcCardId))

  it "Make Me Angry attaches only to the friendly She-Hulk leader" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "57001"
      card <- putInHand you "57034"
      payment <- putInHand you "01088"
      playOn card own [payment]
      pass
      getsGame (fmap (upgradeAttachedEnemy . toAttrs) . lookup (upgradeIdOf card) . gameUpgrades) `shouldReturn` Just (Just $ EnemyVillainId own)

  it "Make Me Angry is not playable with a different friendly leader" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56137"
      card <- putInHand you "57034"
      _ <- putInHand you "01088"
      offers (isPlay card) `shouldReturn` False

  for_ [False,True] $ \stunned ->
    it ("Make Me Angry answers a leader attack and discards only after a completed counterattack: stunned " <> show stunned) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        (own,enemy) <- leaders tg "57001"
        zeros <- traverse encounterCard $ replicate 8 C.cw57036
        resolve [CreateDeck (VillainDeckKey own) $ take 4 zeros,CreateDeck (VillainDeckKey enemy) $ drop 4 zeros]
        card <- putInHand you "57034"
        payment <- putInHand you "01088"
        playOn card own [payment]
        pass
        when stunned $ resolve [GiveStatus (VillainRef own) StunnedStatus]
        before <- leaderHp enemy
        L.runQueueT (L.capture $ Marvel.CivilWar.Helpers.leaderAttacksLeader enemy own) >>= resolve
        leaderHp enemy `shouldReturn` (before - if stunned then 0 else 2)
        getsGame (isJust . lookup (upgradeIdOf card) . gameUpgrades) `shouldReturn` stunned
        getCharacterStatusCount (VillainCharacter own) StunnedStatus `shouldReturn` 0
        when stunned do
          L.runQueueT (L.capture $ Marvel.CivilWar.Helpers.leaderAttacksLeader enemy own) >>= resolve
          leaderHp enemy `shouldReturn` (before - 2)
          getsGame (isJust . lookup (upgradeIdOf card) . gameUpgrades) `shouldReturn` False

  it "Make Me Angry counterattacks the hero who attacked She-Hulk" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      (own,_) <- leaders tg "57001"
      zeros <- traverse encounterCard $ replicate 4 C.cw57036
      resolve [CreateDeck (VillainDeckKey own) zeros]
      card <- putInHand you "57034"
      payment <- putInHand you "01088"
      playOn card own [payment]
      pass
      start [SetActivePlayer opponent,IdentityMessage opponent BeginTurn]
      changeForm opponent
      before <- damageOf opponent
      L.runQueueT (L.capture $ L.basicAttackOn opponent (EnemyVillainId own) 0) >>= resolve
      damageOf opponent >>= (`shouldSatisfy` (> before))
      getsGame (isJust . lookup (upgradeIdOf card) . gameUpgrades) `shouldReturn` False
      discardOf you >>= (`shouldSatisfy` elem card)

  it "Make Me Angry does not answer an ally attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      (own,_) <- leaders tg "57001"
      -- The temporary Klaw used to create the second board can reveal a guard.
      -- Keep the leader available so this tests the reward's ally-attack trigger.
      minions <- getsGame $ map fst . toList . gameMinions
      resolve [RemoveFromPlay $ MinionRef mid | mid <- minions]
      card <- putInHand you "57034"
      payment <- putInHand you "01088"
      playOn card own [payment]
      pass
      ally <- putIntoPlay opponent "01002"
      before <- damageOf opponent
      start [SetActivePlayer opponent,AllyMessage (allyIdOf ally) AllyAttacked]
      choose "Black Cat attacks She-Hulk" (isTargetChoice own)
      pass
      damageOf opponent `shouldReturn` before
      getsGame (isJust . lookup (upgradeIdOf card) . gameUpgrades) `shouldReturn` True

  it "Make Me Angry is not offered when She-Hulk cannot receive player attachments" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "57001"
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ VillainRef own) [CannotHaveAttachedPlayerCards] False) >>= resolve
      card <- putInHand you "57034"
      _ <- putInHand you "01088"
      offers (isPlay card) `shouldReturn` False
