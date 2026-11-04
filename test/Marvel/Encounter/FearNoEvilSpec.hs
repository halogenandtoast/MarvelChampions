module Marvel.Encounter.FearNoEvilSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Message.Lifted qualified as L
import Marvel.Modifier
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.SideScheme.Cards qualified as SchemeCards
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Types qualified as Side
import Marvel.Trait qualified as Trait
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as Set
import Marvel.Shadowland.Cards qualified as Shadow
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import Marvel.Phase (Phase(VillainPhase))
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W
import Marvel.Ref
import TestImport

heldAt :: SideSchemeId -> GameAppT [EncounterCard]
heldAt sid = getsGame $ Side.sideSchemeHeldEncounterCards . toAttrs . (HashMap.! sid) . gameSideSchemes

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

attachmentDamageNow :: AttachmentId -> GameAppT Natural
attachmentDamageNow aid = getsGame $ Attachment.attachmentDamage . toAttrs . (HashMap.! aid) . gameAttachments

equipStreetVehicle :: CharacterId -> CardDef -> GameAppT ()
equipStreetVehicle character def = do
  card <- encounterCard def
  resolve =<< L.runQueueT (L.capture $ L.attachEncounterToCharacter card character)

trafficThreat :: SideSchemeId -> GameAppT Natural
trafficThreat sid = getsGame $ Side.sideSchemeThreat . toAttrs . (HashMap.! sid) . gameSideSchemes

spec :: Spec
spec = describe "Fear No Evil encounter cards" do
  it "attributes hero upgrades to their hero and keeps allies and supports distinct" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      ally <- putIntoPlay you "01002"
      support <- putIntoPlay you "01006"
      upgrade <- putIntoPlay you "01081"
      threatSourceCharacter (AllyRef $ allyIdOf ally) `shouldReturn` Just (AllyCharacter $ allyIdOf ally)
      threatSourceCharacter (SupportRef $ supportIdOf support) `shouldReturn` Nothing
      threatSourceCharacter (UpgradeRef $ upgradeIdOf upgrade) `shouldReturn` Just (IdentityCharacter you)
      inspired <- putInHand you "01074"
      payment <- putInHand you "01089"
      playOn inspired (allyIdOf ally) [payment]
      pass
      threatSourceCharacter (UpgradeRef $ upgradeIdOf inspired) `shouldReturn` Just (AllyCharacter $ allyIdOf ally)

  it "contains five physical cards" do
    tg <- testGame ["spider-man"]
    inGame tg $ length <$> gatherEncounterSet Set.FneDisasters >>= (`shouldBe` 5)

  it "a disaster starts with two civilians and leaves after the last is rescued" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Cards.street60177
      pass
      eid <- selectJust (EnvironmentIs Cards.street60177)
      getCountersOn eid `shouldReturn` 2
      resolve [RemoveCounters (EnvironmentRef eid) 1]
      getCountersOn eid `shouldReturn` 1
      resolve [RemoveCounters (EnvironmentRef eid) 1]
      selectAny (EnvironmentIs Cards.street60177) `shouldReturn` False

  it "Bystanders in Peril adds a civilian to a chosen disaster" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Cards.street60178
      pass
      eid <- selectJust (EnvironmentIs Cards.street60178)
      _ <- revealNow (player1 tg) Cards.street60181
      choose "the disaster" (isTargetChoice eid)
      pass
      getCountersOn eid `shouldReturn` 3

  it "Cops has five cards, including both ordinary cops" do
    tg <- testGame ["spider-man"]
    inGame tg do
      cards <- gatherEncounterSet Set.FneCops
      length cards `shouldBe` 5
      length (filter ((== "60182") . toCardCode) cards) `shouldBe` 2

  it "a cop schemes by attacking the villain instead" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60182
      before <- mainSchemeThreatNow
      resolve [MinionMessage mid MinionSchemes]
      mainSchemeThreatNow `shouldReturn` before

  it "Police Cordon gives Police minions guard and patrol" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60183
      _ <- revealNow (player1 tg) Cards.street60185
      pass
      minionMatches (MinionWithKeyword Guard <> MinionWithKeyword Patrol) mid `shouldReturn` True

  it "The Owl and Tombstone each contain five physical encounter cards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      length <$> gatherEncounterSet Set.FneTheOwl `shouldReturn` 5
      length <$> gatherEncounterSet Set.FneTombstone `shouldReturn` 5

  it "Flight Serum attaches to an enemy without Aerial and increases its basic powers" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard Cards.street60191
      resolve [RevealEncounterCard (player1 tg) card]
      getModifiers vid >>= (`shouldSatisfy` elem (AttackModifier 1))
      getModifiers vid >>= (`shouldSatisfy` elem (SchemeModifier 1))
      enemyMatches (EnemyWithTrait Trait.Aerial) (EnemyVillainId vid) `shouldReturn` True

  it "The Owl has piercing even when his quickstrike does not trigger" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60193
      minionMatches (MinionWithKeyword Piercing) mid `shouldReturn` True
      minionMatches (MinionWithKeyword Quickstrike) mid `shouldReturn` True

  it "Mister Fish engages the player with the fewest remaining hit points" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      setDamage (player2 tg) 8
      mid <- addMinion (player1 tg) Cards.street60192
      minionMatches (MinionEngagedWith $ IdentityWithId $ player2 tg) mid `shouldReturn` True
      minionMatches (MinionWithKeyword Ranged <> MinionWithKeyword Piercing) mid `shouldReturn` True

  it "Cold as Ice toughens the highest printed-HP minion and confuses the first player after it schemes" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60198
      card <- encounterCard Cards.street60195
      resolve [RevealEncounterCard (player1 tg) card]
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 1
      getModifiers mid >>= (`shouldSatisfy` elem (SchemeModifier 2))
      resolve [MinionMessage mid MinionSchemes]
      identityMatches ConfusedIdentity (player1 tg) `shouldReturn` True
      selectAny (AttachmentIs Cards.street60195) `shouldReturn` False

  it "Hard as Marble stuns the character the attached minion attacked and discards itself" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.street60198
      card <- encounterCard Cards.street60196
      resolve [RevealEncounterCard p card]
      getModifiers mid >>= (`shouldSatisfy` elem (AttackModifier 2))
      resolve [MinionMessage mid $ MinionAttacks p]
      identityMatches StunnedIdentity p `shouldReturn` True
      selectAny (AttachmentIs Cards.street60196) `shouldReturn` False

  it "Beetle chooses a minion attachment and attaches it to himself without its original host-selection instruction" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- addMinion (player1 tg) Cards.street60198
      _ <- setEncounterDeck [Cards.street60195] []
      mid <- addMinion (player1 tg) Cards.street60197
      getModifiers mid >>= (`shouldSatisfy` elem (SchemeModifier 2))
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 0
      getEncounterDeck `shouldReturn` []

  it "Swoop Down gives Aerial enemies two extra scheme during their activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60192
      flight <- encounterCard Cards.street60191
      resolve =<< L.runQueueT (L.capture $ L.attachEncounterCard flight mid)
      _ <- setEncounterDeck [Cards.street60193,Cards.street60191] []
      before <- mainSchemeThreatNow
      card <- encounterCard Cards.street60194
      resolve [RevealEncounterCard (player1 tg) card]
      mainSchemeThreatNow `shouldReturn` before + 5
      a <- getsGame $ toAttrs . (HashMap.! mid) . gameMinions
      Minion.minionBoost a `shouldBe` 0

  it "Tracksuit Mafia contains seven cards including three Tracksuit Bros" do
    tg <- testGame ["spider-man"]
    inGame tg do
      cards <- gatherEncounterSet Set.FneTracksuitMafia
      length cards `shouldBe` 7
      length (filter ((== "60203") . toCardCode) cards) `shouldBe` 3

  it "Ivan's acceleration tokens increase when he schemes and leave play with him" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60200
      getAccelerationTokenCount `shouldReturn` 1
      getAccelerationCount `shouldReturn` 1
      resolve [MinionMessage mid MinionSchemes]
      getAccelerationTokenCount `shouldReturn` 2
      getAccelerationCount `shouldReturn` 2
      resolve [RemoveFromPlay $ MinionRef mid]
      getAccelerationTokenCount `shouldReturn` 0

  it "The Clown retains a facedown boost card after scheming for his next activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [Cards.street60191,Cards.street60191,Cards.street60191] []
      mid <- addMinion (player1 tg) Cards.street60201
      resolve [MinionMessage mid MinionSchemes]
      cards <- getsGame $ Minion.minionBoostCards . toAttrs . (HashMap.! mid) . gameMinions
      length cards `shouldBe` 1

  it "a defeated Tracksuit Bro shuffles back when the side scheme is absent" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60203
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 3 FromAbility]
      selectAny (MinionWithId mid) `shouldReturn` False
      deck <- getEncounterDeck
      length (filter ((== CardId (unMinionId mid)) . ecCardId) deck) `shouldBe` 1
      discard <- getEncounterDiscard
      liftIO $ map ecCardId discard `shouldNotContain` [CardId (unMinionId mid)]

  it "Tracksuit Mafia tucks the actual discarded minion, persists it, and discards it when leaving play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [Cards.street60191,Cards.street60203,Cards.street60191] []
      card <- encounterCard Cards.street60204
      resolve [RevealEncounterCard (player1 tg) card]
      sid <- selectJust (SideSchemeIs Cards.street60204)
      held <- heldAt sid
      map toCardCode held `shouldBe` ["60203"]
      mainSchemeThreatNow `shouldReturn` 0
      game <- getGame
      case fromJSON @Game (toJSON game) of
        Error err -> liftIO $ expectationFailure err
        Success saved -> toJSON saved `shouldBe` toJSON game
      discard <- getEncounterDiscard
      liftIO $ map ecCardId discard `shouldNotContain` map ecCardId held
      resolve [RemoveFromPlay $ SideSchemeRef sid]
      after <- getEncounterDiscard
      liftIO $ map ecCardId after `shouldContain` map ecCardId held

  it "a defeated Tracksuit minion tucks under Tracksuit Mafia with its physical card id" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [Cards.street60200,Cards.street60191] []
      card <- encounterCard Cards.street60204
      resolve [RevealEncounterCard (player1 tg) card]
      sid <- selectJust (SideSchemeIs Cards.street60204)
      broCard <- encounterCard Cards.street60203
      resolve [RevealEncounterCard (player1 tg) broCard]
      let mid = minionOf broCard
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 3 FromAbility]
      selectAny (MinionWithId mid) `shouldReturn` False
      held <- heldAt sid
      liftIO $ map ecCardId held `shouldContain` [CardId $ unMinionId mid]
      removed <- getRemovedFromGameEncounterCards
      liftIO $ map ecCardId removed `shouldNotContain` [CardId $ unMinionId mid]

  it "revealing a Tracksuit from the deck releases one tucked minion without recursively releasing another" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [Cards.street60200,Cards.street60191] []
      card <- encounterCard Cards.street60204
      resolve [RevealEncounterCard (player1 tg) card]
      sid <- selectJust (SideSchemeIs Cards.street60204)
      bro <- encounterCard Cards.street60203
      resolve [RevealEncounterCardFrom W.RevealedFromSetAside (player1 tg) bro]
      let mid = minionOf bro
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 3 FromAbility]
      length <$> heldAt sid `shouldReturn` 2
      newBro <- encounterCard Cards.street60203
      resolve [RevealEncounterCard (player1 tg) newBro]
      length <$> heldAt sid `shouldReturn` 1

  it "Tracksuit Mafioso stuns and confuses the actual ally that defeated him when no Mafia scheme is present" do
    tg <- testGame ["spider-man"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01067"
      let aid = allyIdOf ally
      mid <- addMinion (player1 tg) Cards.street60202
      resolve [MinionMessage mid $ MinionDamaged (AllyRef aid) $ toDamage 7 $ FromAllyAttack aid]
      getCharacterStatusCount (AllyCharacter aid) StunnedStatus `shouldReturn` 1
      getCharacterStatusCount (AllyCharacter aid) ConfusedStatus `shouldReturn` 1

  it "Hit List removes one threat per player for an ally actually defeated by its indirect damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01067"
      let aid = allyIdOf ally
      card <- encounterCard Cards.street60199
      resolve [RevealEncounterCard (player1 tg) card]
      sid <- selectJust (SideSchemeIs Cards.street60199)
      runNow [CheckWindows [W.Window W.After $ W.PhaseBegan VillainPhase]]
      whenM (offers $ isAbilityOf sid) $ choose "Hit List's forced response" (isAbilityOf sid)
      choose "assign the first damage to Black Cat" (isTargetChoice aid)
      choose "assign lethal damage to Black Cat" (isTargetChoice aid)
      pass
      whenM (offers $ isTargetChoice sid) $ choose "remove threat from Hit List" (isTargetChoice sid)
      pass
      selectAny (AllyWithId aid) `shouldReturn` False
      getsGame (Side.sideSchemeThreat . toAttrs . (HashMap.! sid) . gameSideSchemes) `shouldReturn` 2

  it "Tombstone's boost gives the villain tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard Cards.street60198
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1

  it "Swoop Down's extra attack does not carry over when stun cancels the Aerial minion's activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.street60192
      flight <- encounterCard Cards.street60191
      resolve =<< L.runQueueT (L.capture $ L.attachEncounterCard flight mid >> L.giveStatus mid StunnedStatus)
      resolve [IdentityMessage p $ ChangedToForm A]
      _ <- setEncounterDeck [Cards.street60193,Cards.street60191] []
      card <- encounterCard Cards.street60194
      resolve [RevealEncounterCard p card]
      getCharacterStatusCount (MinionCharacter mid) StunnedStatus `shouldReturn` 0
      a <- getsGame $ toAttrs . (HashMap.! mid) . gameMinions
      Minion.minionBoost a `shouldBe` 0
      before <- damageOf p
      resolve [MinionMessage mid $ MinionAttacks p]
      damageOf p `shouldReturn` before + 3

  it "Sports Car chooses the strongest enemy among those without a vehicle" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      first <- encounterCard Cards.street60186
      resolve [RevealEncounterCard (player1 tg) first]
      getModifiers vid >>= (`shouldSatisfy` elem (AttackModifier 1))
      mid <- addMinion (player1 tg) Cards.street60198
      second <- encounterCard Cards.street60186
      resolve [RevealEncounterCard (player1 tg) second]
      getAttachedCharacter (attachmentOf second) `shouldReturn` Just (MinionCharacter mid)

  it "Motorcycle attaches to the highest-SCH eligible enemy and takes three damage in its place" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60198
      card <- encounterCard Cards.street60187
      resolve [RevealEncounterCard (player1 tg) card]
      getAttachedCharacter (attachmentOf card) `shouldReturn` Just (MinionCharacter mid)
      getModifiers mid >>= (`shouldSatisfy` elem (SchemeModifier 1))
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 3 FromAbility]
      minionDamageOf mid `shouldReturn` Just 0
      selectAny (AttachmentWithId $ attachmentOf card) `shouldReturn` False

  it "Utility Van chooses the least remaining HP and absorbs an entire packet without spilling excess" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60192
      card <- encounterCard Cards.street60188
      resolve [RevealEncounterCard (player1 tg) card]
      getAttachedCharacter (attachmentOf card) `shouldReturn` Just (MinionCharacter mid)
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 4 FromAbility]
      attachmentDamageNow (attachmentOf card) `shouldReturn` 4
      minionDamageOf mid `shouldReturn` Just 0
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 3 FromAbility]
      selectAny (AttachmentWithId $ attachmentOf card) `shouldReturn` False
      minionDamageOf mid `shouldReturn` Just 0
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 1 FromAbility]
      minionDamageOf mid `shouldReturn` Just 1

  it "a vehicle is not damaged when tough prevents its host taking damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60192
      card <- encounterCard Cards.street60186
      resolve =<< L.runQueueT (L.capture $ L.attachEncounterCard card mid >> L.giveStatus mid ToughStatus)
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 2 FromAbility]
      attachmentDamageNow (attachmentOf card) `shouldReturn` 0
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 0

  it "piercing discards the host's tough before the vehicle replaces taking the damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60192
      card <- encounterCard Cards.street60186
      resolve =<< L.runQueueT (L.capture $ L.attachEncounterCard card mid >> L.giveStatus mid ToughStatus)
      resolve [MinionMessage mid $ MinionDamaged GameRef $ withPiercing $ toDamage 3 FromAttack]
      attachmentDamageNow (attachmentOf card) `shouldReturn` 3
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 0
      minionDamageOf mid `shouldReturn` Just 0

  it "a vehicle replaces unpreventable damage rather than preventing it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      card <- encounterCard Cards.street60188
      resolve =<< L.runQueueT (L.capture $ L.attachEncounterToCharacter card $ IdentityCharacter p)
      resolve =<< L.runQueueT (L.capture $ L.damageIdentity p GameRef $ withUnpreventable $ toDamage 1 FromAbility)
      damageOf p `shouldReturn` 0
      attachmentDamageNow (attachmentOf card) `shouldReturn` 1

  it "an unattachable vehicle leaves play and surges" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      first <- encounterCard Cards.street60186
      resolve [RevealEncounterCard p first]
      _ <- setEncounterDeck [Cards.street60180] []
      second <- encounterCard Cards.street60187
      resolve [RevealEncounterCard p second]
      selectAny (AttachmentWithId $ attachmentOf second) `shouldReturn` False
      map toCardCode <$> dealtTo p `shouldReturn` ["60180"]

  it "Carjacking can be paid with three matching resources to attach directly to the identity" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      clearHand p
      energy <- putInHand p "01088"
      haymaker <- putInHand p "01087"
      (deck,_) <- setEncounterDeck [Cards.street60187,Cards.street60195] []
      _ <- revealNow p Cards.street60190
      pass
      choose "take the vehicle" (isLabel "$label.fneTakeVehicle")
      payWith [energy,haymaker]
      pass
      let car = fromMaybe (error "missing vehicle") $ headMay deck
      getAttachedCharacter (attachmentOf car) `shouldReturn` Just (IdentityCharacter p)
      getModifiers p >>= (`shouldSatisfy` elem (ThwartModifier 1))
      getEncounterDiscard >>= (liftIO . (`shouldNotContain` [car]))

  it "Carjacking offers no payment option without three matching resources" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      resolve [IdentityMessage p $ ChangedToForm A]
      clearHand p
      _ <- putInHand p "01089"
      _ <- putInHand p "01087"
      _ <- setEncounterDeck [Cards.street60188,Cards.street60195] []
      _ <- revealNow p Cards.street60190
      pass
      offers (isLabel "$label.fneTakeVehicle") `shouldReturn` False
      choose "reveal the vehicle" (isLabel "$label.fneRevealVehicle")
      pass
      vid <- villainIdNow
      choose "attach the vehicle to the villain" (isTargetChoice vid)
      pass
      vehicle <- selectJust (AttachmentIs Cards.street60188)
      getAttachedCharacter vehicle `shouldReturn` Just (VillainCharacter vid)

  for_ [Cards.street60195,Cards.street60196] $ \def ->
    it (show (toCardCode def) <> " leaves play and surges when no minion is available") do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- setEncounterDeck [Cards.street60180] []
        card <- encounterCard def
        resolve [RevealEncounterCard (player1 tg) card]
        selectAny (AttachmentWithId $ attachmentOf card) `shouldReturn` False
        length <$> dealtTo (player1 tg) `shouldReturn` 1

  it "does not put a vehicle into play attached to a character that has left play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01067"
      let aid = allyIdOf ally
      resolve [RemoveFromPlay $ AllyRef aid]
      card <- encounterCard Cards.street60188
      resolve =<< L.runQueueT (L.capture $ L.attachEncounterToCharacter card $ AllyCharacter aid)
      selectAny (AttachmentWithId $ attachmentOf card) `shouldReturn` False
      getEncounterDiscard >>= (liftIO . (`shouldContain` [card]))

  it "Traffic Jam starts with its fixed two threat plus hinder per player" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      _ <- revealNow (player1 tg) Cards.street60189
      pass
      sid <- selectJust (SideSchemeIs Cards.street60189)
      trafficThreat sid `shouldReturn` 6

  it "Traffic Jam redirects a vehicle-equipped villain's scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Cards.street60189
      pass
      sid <- selectJust (SideSchemeIs Cards.street60189)
      vid <- villainIdNow
      equipStreetVehicle (VillainCharacter vid) Cards.street60187
      _ <- setEncounterDeck [Cards.street60188,Cards.street60188] []
      before <- mainSchemeThreatNow
      resolve [VillainMessage vid $ VillainSchemesAgainst you]
      pass
      mainSchemeThreatNow `shouldReturn` before
      trafficThreat sid `shouldReturn` 6

  it "Traffic Jam redirects a vehicle-equipped minion's scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Cards.street60189
      pass
      sid <- selectJust (SideSchemeIs Cards.street60189)
      mid <- addMinion you Cards.street60198
      equipStreetVehicle (MinionCharacter mid) Cards.street60187
      before <- mainSchemeThreatNow
      resolve [MinionMessage mid MinionSchemes]
      mainSchemeThreatNow `shouldReturn` before
      trafficThreat sid `shouldReturn` 7

  it "Traffic Jam redirects a hero's basic thwart after a vehicle is stolen" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- revealNow you Cards.street60189
      pass
      sid <- selectJust (SideSchemeIs Cards.street60189)
      equipStreetVehicle (IdentityCharacter you) Cards.street60187
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
      choose "basic thwart" (isHeroThwart you)
      chooseMainScheme
      choose "resolve Traffic Jam" (\case UseAbility ab -> abilitySource ab == SideSchemeRef sid; _ -> False)
      pass
      mainSchemeThreatNow `shouldReturn` 3
      trafficThreat sid `shouldReturn` 2

  it "Traffic Jam redirects an ally's basic thwart" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Cards.street60189
      pass
      sid <- selectJust (SideSchemeIs Cards.street60189)
      ally <- putIntoPlay you "01002"
      let aid = allyIdOf ally
      equipStreetVehicle (AllyCharacter aid) Cards.street60187
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
      attrs <- getsGame $ toAttrs . (HashMap.! aid) . gameAllies
      choose "ally thwarts" (isAllyThwart attrs)
      chooseMainScheme
      choose "resolve Traffic Jam" (\case UseAbility ab -> abilitySource ab == SideSchemeRef sid; _ -> False)
      pass
      mainSchemeThreatNow `shouldReturn` 3
      trafficThreat sid `shouldReturn` 2

  it "Traffic Jam redirects a hero's event thwart" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- revealNow you Cards.street60189
      pass
      sid <- selectJust (SideSchemeIs Cards.street60189)
      equipStreetVehicle (IdentityCharacter you) Cards.street60188
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
      event <- putInHand you "01060"
      payment <- putInHand you "01089"
      playWith event [payment]
      chooseMainScheme
      choose "resolve Traffic Jam" (\case UseAbility ab -> abilitySource ab == SideSchemeRef sid; _ -> False)
      pass
      mainSchemeThreatNow `shouldReturn` 3
      trafficThreat sid `shouldReturn` 1

  it "Traffic Jam redirects removal that is not a thwart, capped by the original imminent removal" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Cards.street60189
      pass
      sid <- selectJust (SideSchemeIs Cards.street60189)
      equipStreetVehicle (IdentityCharacter you) Cards.street60188
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 1]
      resolve [MainSchemeMessage main $ MainSchemeThreatRemoved (IdentityRef you) 3]
      mainSchemeThreatNow `shouldReturn` 1
      trafficThreat sid `shouldReturn` 3

  it "Traffic Jam does not replace a zero removal or a character without a vehicle" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Cards.street60189
      pass
      sid <- selectJust (SideSchemeIs Cards.street60189)
      main <- mainSchemeIdNow
      equipStreetVehicle (IdentityCharacter you) Cards.street60188
      resolve [MainSchemeMessage main $ MainSchemeThreatRemoved (IdentityRef you) 3]
      trafficThreat sid `shouldReturn` 4
      vid <- villainIdNow
      resolve =<< L.runQueueT (L.capture $ L.placeThreatOn vid main 2)
      mainSchemeThreatNow `shouldReturn` 2
      trafficThreat sid `shouldReturn` 4

  it "Traffic Jam does not replace a removal prohibited on the original scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Cards.street60189
      pass
      traffic <- selectJust (SideSchemeIs Cards.street60189)
      _ <- addMinion you Shadow.street65030
      _ <- revealNow you Shadow.street65035
      pass
      locked <- selectJust (SideSchemeIs Shadow.street65035)
      before <- trafficThreat locked
      equipStreetVehicle (IdentityCharacter you) Cards.street60188
      resolve [SideSchemeMessage locked $ SideSchemeThreatRemoved (IdentityRef you) 1]
      trafficThreat locked `shouldReturn` before
      trafficThreat traffic `shouldReturn` 4

  it "Traffic Jam redirects Dark Phoenix's custom scheme destination" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Cards.street60189
      _ <- revealNow you SchemeCards.consumeTheWorld
      pass
      traffic <- selectJust (SideSchemeIs Cards.street60189)
      consume <- selectJust (SideSchemeIs SchemeCards.consumeTheWorld)
      mid <- addMinion you MinionCards.darkPhoenix
      equipStreetVehicle (MinionCharacter mid) Cards.street60187
      before <- trafficThreat consume
      resolve [MinionMessage mid MinionSchemed]
      trafficThreat consume `shouldReturn` before
      trafficThreat traffic `shouldReturn` 7

  it "Everything is a Weapon deals two indirect damage in alter ego" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- revealNow you Cards.street60074
      choose "assign the first indirect damage" (isTargetChoice you)
      choose "assign the second indirect damage" (isTargetChoice you)
      pass
      damageOf you `shouldReturn` 2

  it "Everything is a Weapon can remove a controlled ally from the game" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      ally <- putIntoPlay you "01002"
      _ <- revealNow you Cards.street60074
      pass
      choose "remove the ally" (isTargetChoice $ allyIdOf ally)
      pass
      selectAny (AllyWithId $ allyIdOf ally) `shouldReturn` False
      discardOf you >>= (liftIO . (`shouldNotContain` [ally]))

  it "Unerring Desire offers only Persona supports controlled by its revealer" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      own <- putIntoPlay you "01006"
      other <- putIntoPlay (player2 tg) "01033"
      _ <- revealNow you Cards.street60075
      offers (isTargetChoice $ supportIdOf own) `shouldReturn` True
      offers (isTargetChoice $ supportIdOf other) `shouldReturn` False
      choose "remove Aunt May" (isTargetChoice $ supportIdOf own)
      pass
      getsGame (HashMap.member (supportIdOf own) . gameSupports) `shouldReturn` False
      getsGame (HashMap.member (supportIdOf other) . gameSupports) `shouldReturn` True
      discardOf you >>= (liftIO . (`shouldNotContain` [own]))

  it "Unerring Desire becomes a facedown boost without changing its physical card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- revealNow (player1 tg) Cards.street60075
      choose "give the card as a boost" (isTargetChoice $ enemyRef $ EnemyVillainId vid)
      pass
      Villain.villainBoostCards . toAttrs <$> getVillain `shouldReturn` [card]
      getsGame gameRemovedFromGameEncounterCards >>= (liftIO . (`shouldNotContain` [card]))
      getEncounterDiscard >>= (liftIO . (`shouldNotContain` [card]))
      _ <- setEncounterDeck [Cards.street60188,Cards.street60188] []
      resolve [VillainMessage vid $ VillainAttacks $ player1 tg]
      pass
      Villain.villainBoostCards . toAttrs <$> getVillain `shouldReturn` []
      filter ((== ecCardId card) . ecCardId) <$> getEncounterDiscard `shouldReturn` [card]

  it "Deranged Bloodlust adds an amplify icon and four fixed threat" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      _ <- revealNow (player1 tg) Cards.street60073
      pass
      sid <- selectJust (SideSchemeIs Cards.street60073)
      trafficThreat sid `shouldReturn` 4
      getAmplifyCount `shouldReturn` 1

  it "Deranged Bloodlust's boost reveals it after the activation, so its amplify does not boost that activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      (cards,_) <- setEncounterDeck [Cards.street60073,Cards.street60188,Cards.street60188] []
      resolve [VillainMessage vid $ VillainSchemesAgainst you]
      mainSchemeThreatNow `shouldReturn` 1
      sid <- selectJust (SideSchemeIs Cards.street60073)
      trafficThreat sid `shouldReturn` 4
      getEncounterDiscard >>= (liftIO . (`shouldNotContain` take 1 cards))
      resolve [VillainMessage vid $ VillainSchemesAgainst you]
      mainSchemeThreatNow `shouldReturn` 3
