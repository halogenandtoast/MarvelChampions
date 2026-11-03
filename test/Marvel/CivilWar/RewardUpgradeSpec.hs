module Marvel.CivilWar.RewardUpgradeSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
import Data.List (nub)
import Marvel.Ally.Types qualified as A
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf)
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
