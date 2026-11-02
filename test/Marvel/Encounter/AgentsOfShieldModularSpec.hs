{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Encounter.AgentsOfShieldModularSpec (spec) where

import Data.HashMap.Strict qualified as HM
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Environment.Types qualified as Environment
import Marvel.Keyword qualified as K
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Types qualified as Side
import Marvel.Trait qualified as T
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Window qualified as W
import TestImport

lifted :: L.QueueT GameAppT () -> GameAppT ()
lifted body = resolve =<< L.runQueueT (L.capture body)

putEncounter :: IdentityId -> CardDef -> GameAppT EncounterCard
putEncounter you def = do
  card <- encounterCard def
  resolve [PutEncounterCardIntoPlay you card]
  pure card

spec :: Spec
spec = describe "Agents of S.H.I.E.L.D. modular encounters" do
  for_ [Cards.aos50083,Cards.aos50093,Cards.aos50094,Cards.aos50098,Cards.aos50125,Cards.aos50126,Cards.aos50172,Cards.aos50178] $ \def ->
    for_ [StunnedStatus,ConfusedStatus] $ \status ->
      it ("discards vulnerable " <> show (toCardCode def) <> " when given " <> show status <> ", without defeating it") do
        tg <- testGame ["spider-man"]
        let p=player1 tg
        inGame tg do
          mid <- addMinion p def
          victory <- getVictoryDisplay
          threat <- mainSchemeThreatNow
          lifted $ L.giveStatus mid status
          selectAny (MinionWithId mid) `shouldReturn` False
          getVictoryDisplay `shouldReturn` victory
          mainSchemeThreatNow `shouldReturn` threat

  it "A.I.M. Scientist is protected only while another minion is engaged with its player" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      scientist <- addMinion p Cards.aos50083
      getModifiers (MinionRef scientist) >>= (`shouldSatisfy` notElem CannotBeAttacked)
      soldier <- addMinion p Cards.aos50084
      getModifiers (MinionRef scientist) >>= (`shouldSatisfy` elem CannotBeAttacked)
      lifted $ L.removeFromPlay soldier
      getModifiers (MinionRef scientist) >>= (`shouldSatisfy` notElem CannotBeAttacked)

  it "A.I.M. Soldier finds Scientist, but places a main-scheme threat if Scientist was already in play" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      scientist <- encounterCard Cards.aos50083
      resolve [SetAside [EncounterCard scientist]]
      before <- mainSchemeThreatNow
      soldierCard <- encounterCard Cards.aos50084
      resolve [RevealEncounterCard p soldierCard]
      selectListCount (MinionIs Cards.aos50083) `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` before
      secondSoldierCard <- encounterCard Cards.aos50084
      resolve [RevealEncounterCard p secondSoldierCard]
      selectListCount (MinionIs Cards.aos50083) `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` (before+1)

  it "Mad Science gives every A.I.M. minion its own acceleration icon" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      mid <- addMinion p Cards.aos50084
      scheme <- encounterCard Cards.aos50085
      resolve [RevealEncounterCard p scheme]
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` elem (AccelerationIconModifier 1))
      lifted $ L.removeThreat p p 99 (SchemeWithId $ SchemeSideSchemeId $ sideSchemeOf scheme)
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` notElem (AccelerationIconModifier 1))

  it "Monica has villainous only while Scientist Supreme is in the victory display" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      monica <- addMinion p Cards.aos50126
      getEnemyKeywords (EnemyMinionId monica) >>= (`shouldSatisfy` notElem K.Villainous)
      supreme <- encounterCard Cards.aos50125
      resolve [AddCardToVictoryDisplay supreme]
      getEnemyKeywords (EnemyMinionId monica) >>= (`shouldSatisfy` elem K.Villainous)
      getVictoryPoints `shouldReturn` (-1)

  it "Scientist Supreme attacks are piercing and ranged" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.aos50125
      keywords <- getEnemyKeywords (EnemyMinionId mid)
      keywords `shouldSatisfy` elem K.Piercing
      keywords `shouldSatisfy` elem K.Ranged

  it "Diplomatic Immunity adds an acceleration token for each A.I.M. minion in victory" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      for_ [Cards.aos50125,Cards.aos50126] $ \def -> encounterCard def >>= \c -> resolve [AddCardToVictoryDisplay c]
      card <- encounterCard Cards.aos50127
      resolve [RevealEncounterCard p card]
      sid <- selectJust (SideSchemeIs Cards.aos50127)
      getsGame (maybe 0 Side.sideSchemeAccelerationTokens . fmap toAttrs . HM.lookup sid . gameSideSchemes) `shouldReturn` 2

  it "Diplomatic Sanctions discards a card for each defeated A.I.M. minion" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      clearHand p
      _ <- putInHand p "01089"
      _ <- putInHand p "01090"
      supreme <- encounterCard Cards.aos50125
      resolve [AddCardToVictoryDisplay supreme]
      card <- encounterCard Cards.aos50128
      resolve [RevealEncounterCard p card]
      length <$> handOf p >>= (`shouldBe` 1)

  it "Moonstone becomes tough after an activation" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      moonstone <- addMinion p Cards.aos50139
      resolve [MinionMessage moonstone MinionSchemes]
      getCharacterStatusCount (MinionCharacter moonstone) ToughStatus `shouldReturn` 1

  it "Rule the Skies buffs Aerial characters and leaves grounded characters unchanged" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      aerial <- addMinion p Cards.aos50139
      grounded <- addMinion p Cards.aos50161
      _ <- putEncounter p Cards.aos50140
      getModifiers (MinionRef aerial) >>= (`shouldSatisfy` elem (AttackModifier 1))
      getModifiers (MinionRef grounded) >>= (`shouldSatisfy` notElem (AttackModifier 1))

  it "Find effects do not retrieve Thunderbolts from the victory display" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      moonstone <- encounterCard Cards.aos50139
      resolve [AddCardToVictoryDisplay moonstone]
      _ <- setEncounterDeck [Treacheries.advance,Treacheries.advance,Treacheries.advance] []
      card <- encounterCard Cards.aos50141
      resolve [RevealEncounterCard p card]
      selectAny (MinionIs Cards.aos50139) `shouldReturn` False
      getVictoryDisplay `shouldReturn` [moonstone]

  it "Hard Sound finds Songbird from set-aside and activates her" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      songbird <- encounterCard Cards.aos50143
      resolve [SetAside [EncounterCard songbird]]
      before <- mainSchemeThreatNow
      card <- encounterCard Cards.aos50147
      resolve [RevealEncounterCard p card]
      selectAny (MinionIs Cards.aos50143) `shouldReturn` True
      mainSchemeThreatNow >>= (`shouldSatisfy` (>=before))

  it "Solid Sound Constructs removes stalwart and replaces a status card with discarding the attachment" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      songbird <- addMinion p Cards.aos50143
      card <- encounterCard Cards.aos50144
      resolve [RevealEncounterCard p card]
      lifted $ L.giveStatus songbird StunnedStatus
      getCharacterStatusCount (MinionCharacter songbird) StunnedStatus `shouldReturn` 0
      getsGame (HM.member (AttachmentId $ unCardId $ ecCardId card) . gameAttachments) `shouldReturn` False

  it "Sonic Bubble prevents the whole damage instance by removing an equal amount of threat" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      bubble <- encounterCard Cards.aos50146
      resolve [RevealEncounterCard p bubble]
      before <- villainDamageNow
      vid <- villainIdNow
      lifted $ L.dealDamage vid p (toDamage 2 FromAbility)
      villainDamageNow `shouldReturn` before
      sideSchemeThreat (sideSchemeOf bubble) `shouldReturn` 1

  it "Pride of the Red Room counts Preparation cards already in play" do
    tg <- testGame ["black-widow"]
    let p=player1 tg
    inGame tg do
      _ <- putIntoPlay p "08010"
      card <- encounterCard Cards.aos50150
      resolve [RevealEncounterCard p card]
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 5

  it "Radioactive Man damages every character controlled by the player after activating" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      ally <- putIntoPlay p "01083"
      radioactive <- addMinion p Cards.aos50152
      resolve [MinionMessage radioactive MinionSchemes]
      damageOf p `shouldReturn` 1
      getsGame (maybe 0 allyDamage . fmap toAttrs . HM.lookup (allyIdOf ally) . gameAllies) `shouldReturn` 1

  it "Runaway Nuclear Reaction adds threat equal to Radioactive Man's damage and explodes at ten" do
    tg <- testGame ["captain-marvel"]
    let p=player1 tg
    inGame tg do
      radioactive <- addMinion p Cards.aos50152
      reaction <- encounterCard Cards.aos50154
      resolve [RevealEncounterCard p reaction]
      lifted $ L.dealDamage radioactive p (toDamage 2 FromAbility)
      sideSchemeThreat (sideSchemeOf reaction) `shouldReturn` 7
      lifted $ L.dealDamage radioactive p (toDamage 3 FromAbility)
      selectAny (SideSchemeIs Cards.aos50154) `shouldReturn` False
      damageOf p `shouldReturn` 10

  it "Blasters attaches to MACH-IV and grants overkill and ranged" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      mach <- addMinion p Cards.aos50156
      card <- encounterCard Cards.aos50157
      resolve [RevealEncounterCard p card]
      keywords <- getEnemyKeywords (EnemyMinionId mach)
      keywords `shouldSatisfy` elem K.Overkill
      keywords `shouldSatisfy` elem K.Ranged

  it "Heat-Seeking Missiles enters with four counters and spends one after an attack" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      mach <- addMinion p Cards.aos50156
      card <- encounterCard Cards.aos50158
      resolve [RevealEncounterCard p card]
      let aid=AttachmentId $ unCardId $ ecCardId card
      getCountersOn aid `shouldReturn` 4
      changeForm p
      clearHand p
      resolve [MinionMessage mach (MinionAttacks p)]
      getCountersOn aid `shouldReturn` 3

  it "Batroc the Leaper makes the player discard on engagement, including when moving between players" do
    tg <- testGame ["spider-man","captain-marvel"]
    let p=player1 tg; other=player2 tg
    inGame tg do
      clearHand p
      clearHand other
      _ <- putInHand p "01090"
      _ <- putInHand other "01090"
      batroc <- addMinion p Cards.aos50161
      length <$> handOf p >>= (`shouldBe` 0)
      lifted $ L.engageMinion batroc other
      length <$> handOf other >>= (`shouldBe` 0)

  it "Parcours du Combattant rotates all engagements simultaneously" do
    tg <- testGame ["spider-man","captain-marvel"]
    let p=player1 tg; other=player2 tg
    inGame tg do
      a <- addMinion p Cards.aos50084
      b <- addMinion other Cards.aos50084
      card <- encounterCard Cards.aos50164
      resolve [RevealEncounterCard p card]
      minionMatches (MinionEngagedWith $ IdentityWithId other) a `shouldReturn` True
      minionMatches (MinionEngagedWith $ IdentityWithId p) b `shouldReturn` True

  it "Disavowed raises the cost of S.H.I.E.L.D. cards and counts them on reveal" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      _ <- putIntoPlay p "01092"
      card <- encounterCard Cards.aos50180
      resolve [RevealEncounterCard p card]
      getModifiers (IdentityRef p) >>= (`shouldSatisfy` elem (AdditionalResourceCost (CardWithTrait T.Shield) 1))

  it "A.I.M. Abductor tucks the ally with the most remaining hit points and finds its scheme" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      cat <- putIntoPlay p "01002"
      fury <- putIntoPlay p "01084"
      scheme <- encounterCard Cards.aos50081
      resolve [SetAside [EncounterCard scheme]]
      _ <- addMinion p Cards.aos50080
      selectAny (AllyWithId $ allyIdOf fury) `shouldReturn` False
      selectAny (AllyWithId $ allyIdOf cat) `shouldReturn` True
      sid <- selectJust (SideSchemeIs Cards.aos50081)
      held <- L.heldCardsAt sid
      map pcCardId held `shouldBe` [pcCardId fury]
      sideSchemeThreat sid `shouldReturn` 6

  it "Abduct Superhumans captures a departing ally and returns it when defeated" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      fury <- putIntoPlay p "01084"
      _ <- putEncounter p Cards.aos50081
      resolve [RemoveFromPlay $ AllyRef $ allyIdOf fury]
      selectAny (AllyWithId $ allyIdOf fury) `shouldReturn` False
      sid <- selectJust (SideSchemeIs Cards.aos50081)
      sideSchemeThreat sid `shouldReturn` 6
      lifted $ L.removeThreat p p 99 (SchemeWithId $ SchemeSideSchemeId sid)
      selectAny (AllyWithId $ allyIdOf fury) `shouldReturn` True

  it "Nabbed stops at the first discarded ally and adds its cost and an acceleration token" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      _ <- putEncounter p Cards.aos50081
      _ <- stackDeck p ["01090","01083","01089"]
      card <- encounterCard Cards.aos50082
      resolve [RevealEncounterCard p card]
      sid <- selectJust (SideSchemeIs Cards.aos50081)
      held <- L.heldCardsAt sid
      map toCardCode held `shouldBe` ["01083"]
      sideSchemeThreat sid `shouldReturn` 5
      deck <- unDeck . playerIdentityDeck . toAttrs <$> getPlayer p
      map toCardCode (take 1 deck) `shouldBe` ["01089"]

  it "Machete shuffles back when defeated but does not when vulnerable discards it" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      machete <- addMinion p Cards.aos50098
      lifted $ L.dealDamage machete p (toDamage 3 FromAbility)
      deck <- getEncounterDeck
      any (cardMatch $ CardIs Cards.aos50098) deck `shouldBe` True

  it "Rapido's reveal and boost each damage every controlled character" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      ally <- putIntoPlay p "01083"
      _ <- addMinion p Cards.aos50099
      damageOf p `shouldReturn` 1
      card <- encounterCard Cards.aos50099
      vid <- villainIdNow
      resolve [RevealBoostCard card $ EnemyVillainId vid]
      damageOf p `shouldReturn` 2
      getsGame (maybe 0 allyDamage . fmap toAttrs . HM.lookup (allyIdOf ally) . gameAllies) `shouldReturn` 2

  it "Zaran tucks a Weapon upgrade and gets attack equal to its printed cost" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      weapon <- putIntoPlay p "01046"
      zaran <- addMinion p Cards.aos50100
      selectAny (UpgradeWithId $ upgradeOf weapon) `shouldReturn` False
      held <- L.heldCardsAt zaran
      map pcCardId held `shouldBe` [pcCardId weapon]
      getModifiers (MinionRef zaran) >>= (`shouldSatisfy` elem (AttackModifier 2))

  it "Batroc's Brigade gives existing enemies tough and minions entering later toughness" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      card <- encounterCard Cards.aos50101
      resolve [RevealEncounterCard p card]
      vid <- villainIdNow
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1
      soldier <- addMinion p Cards.aos50084
      getCharacterStatusCount (MinionCharacter soldier) ToughStatus `shouldReturn` 1

  it "Radiation Exposure penalizes normal identities but improves a Gamma identity's ATK" do
    for_ [("spider-man",AttackPenalty 1),("hulk",AttackModifier 1)] $ \(hero,expected) -> do
      tg <- testGame [hero]
      let p=player1 tg
      inGame tg do
        changeForm p
        card <- encounterCard Cards.aos50153
        resolve [RevealEncounterCard p card]
        mods <- getModifiers (IdentityRef p)
        mods `shouldSatisfy` elem expected
        mods `shouldSatisfy` elem (ThwartPenalty 1)

  it "Down but Not Out explicitly returns a Thunderbolt from victory with five remaining hit points" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      moonstone <- encounterCard Cards.aos50139
      resolve [AddCardToVictoryDisplay moonstone]
      card <- encounterCard Cards.aos50137
      resolve [RevealEncounterCard p card]
      mid <- selectJust (MinionIs Cards.aos50139)
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 5
      getVictoryDisplay `shouldReturn` []
      discard <- encounterDiscard
      map ecCardId discard `shouldSatisfy` notElem (ecCardId card)

  it "Runaway Nuclear Reaction counts damage dealt even when tough prevents Radioactive Man taking it" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      radioactive <- addMinion p Cards.aos50152
      reaction <- encounterCard Cards.aos50154
      resolve [RevealEncounterCard p reaction]
      lifted $ L.giveStatus radioactive ToughStatus
      lifted $ L.dealDamage radioactive p (toDamage 2 FromAbility)
      minionDamageOf radioactive `shouldReturn` Just 0
      sideSchemeThreat (sideSchemeOf reaction) `shouldReturn` 7

  it "Widow's Bite stuns and deals one damage, then deals two when the identity is already stunned" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      first <- encounterCard Cards.aos50079
      resolve [RevealEncounterCard p first]
      isStunned p `shouldReturn` True
      damageOf p `shouldReturn` 1
      second <- encounterCard Cards.aos50079
      resolve [RevealEncounterCard p second]
      isStunned p `shouldReturn` True
      damageOf p `shouldReturn` 3

  it "Security Cameras lowers Alert Level in alter-ego form" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      alert <- encounterCard Cards.aos50090a
      resolve [PutEncounterCardIntoPlay p alert]
      eid <- selectJust (EnvironmentWithTitle "Alert Level")
      lifted $ L.placeCounters eid 2
      lifted $ L.changeToAlterEgo p
      card <- encounterCard Cards.aos50097
      resolve [RevealEncounterCard p card]
      getCountersOn (EnvironmentRef eid) `shouldReturn` 1

  it "Psychological Manipulation uses the hero's ATK against the friendly character with the fewest remaining hit points" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.changeToHeroForm p
      ally <- putIntoPlay p "01083"
      card <- encounterCard Cards.aos50142
      resolve [RevealEncounterCard p card]
      getsGame (maybe 0 allyDamage . fmap toAttrs . HM.lookup (allyIdOf ally) . gameAllies) `shouldReturn` 2
      damageOf p `shouldReturn` 0

  it "Psychological Manipulation discards an ally or support in alter-ego form" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      support <- putIntoPlay p "01092"
      lifted $ L.changeToAlterEgo p
      card <- encounterCard Cards.aos50142
      resolve [RevealEncounterCard p card]
      getsGame (HM.member (supportIdOf support) . gameSupports) `shouldReturn` False

  for_ [(Cards.aos50141,Cards.aos50139),(Cards.aos50147,Cards.aos50143),(Cards.aos50151,Cards.aos50148),(Cards.aos50155,Cards.aos50152),(Cards.aos50160,Cards.aos50156),(Cards.aos50163,Cards.aos50161)] $ \(treachery,minion) ->
    it ("finds and activates " <> show (toCardCode minion) <> " with " <> show (toCardCode treachery)) do
      tg <- testGame ["spider-man"]
      let p=player1 tg
      inGame tg do
        lifted $ L.changeToAlterEgo p
        found <- encounterCard minion
        resolve [SetAside [EncounterCard found]]
        _ <- setEncounterDeck [Cards.aos50124,Cards.aos50124,Cards.aos50124,Cards.aos50124] []
        before <- mainSchemeThreatNow
        card <- encounterCard treachery
        resolve [RevealEncounterCard p card]
        mid <- selectJust (MinionIs minion)
        getMinionEngagedPlayer mid `shouldReturn` Just p
        mainSchemeThreatNow >>= (`shouldSatisfy` (>before))

  it "Hard Sound moves an existing Songbird to the revealing player before activating" do
    tg <- testGame ["spider-man","captain-marvel"]
    let p=player1 tg; other=player2 tg
    inGame tg do
      songbird <- addMinion other Cards.aos50143
      lifted $ L.changeToAlterEgo p
      _ <- setEncounterDeck [Cards.aos50124,Cards.aos50124,Cards.aos50124,Cards.aos50124] []
      before <- mainSchemeThreatNow
      card <- encounterCard Cards.aos50147
      resolve [RevealEncounterCard p card]
      selectListCount (MinionIs Cards.aos50143) `shouldReturn` 1
      getMinionEngagedPlayer songbird `shouldReturn` Just p
      mainSchemeThreatNow >>= (`shouldSatisfy` (>before))

  it "Tap In engages the eligible Thunderbolt with the least damage and activates it" do
    tg <- testGame ["spider-man","captain-marvel"]
    let p=player1 tg; other=player2 tg
    inGame tg do
      fresh <- addMinion other Cards.aos50139
      damaged <- addMinion other Cards.aos50143
      lifted $ L.dealDamage damaged p (toDamage 1 FromAbility)
      lifted $ L.changeToAlterEgo p
      _ <- setEncounterDeck [Cards.aos50124,Cards.aos50124,Cards.aos50124,Cards.aos50124] []
      before <- mainSchemeThreatNow
      card <- encounterCard Cards.aos50138
      resolve [RevealEncounterCard p card]
      getMinionEngagedPlayer fresh `shouldReturn` Just p
      getMinionEngagedPlayer damaged `shouldReturn` Just other
      mainSchemeThreatNow >>= (`shouldSatisfy` (>before))

  it "Tap In falls back to the villain when the selected Thunderbolt's activation is stopped by confused" do
    tg <- testGame ["spider-man","captain-marvel"]
    let p=player1 tg; other=player2 tg
    inGame tg do
      moonstone <- addMinion other Cards.aos50139
      lifted $ L.giveStatus moonstone ConfusedStatus
      lifted $ L.changeToAlterEgo p
      _ <- setEncounterDeck [Cards.aos50124,Cards.aos50124,Cards.aos50124,Cards.aos50124] []
      before <- mainSchemeThreatNow
      card <- encounterCard Cards.aos50138
      resolve [RevealEncounterCard p card]
      getMinionEngagedPlayer moonstone `shouldReturn` Just p
      getCharacterStatusCount (MinionCharacter moonstone) ConfusedStatus `shouldReturn` 0
      mainSchemeThreatNow >>= (`shouldSatisfy` (>before))

  it "Arrest Warrant persists until its alter-ego action exhausts the identity and reveals a S.H.I.E.L.D. minion" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      _ <- setEncounterDeck [Cards.aos50172,Treacheries.advance,Treacheries.advance] []
      card <- encounterCard Cards.aos50179
      resolve [RevealEncounterCard p card]
      let oid=ObligationId $ unCardId $ ecCardId card
      getsGame (HM.member oid . gameObligations) `shouldReturn` True
      choose "discard Arrest Warrant" (isAbilityOf $ ObligationRef oid)
      pressWhenRevealed
      isExhaustedNow p `shouldReturn` True
      getsGame (HM.member oid . gameObligations) `shouldReturn` False
      selectAny (MinionIs Cards.aos50172) `shouldReturn` True

  it "Gravitational Pull's boost exhausts a controlled character" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard Cards.aos50141
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      isExhaustedNow p `shouldReturn` True

  it "Pale Little Spider's boost confuses, adding one threat only when already confused" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      vid <- villainIdNow
      before <- mainSchemeThreatNow
      first <- encounterCard Cards.aos50151
      resolve [RevealBoostCard first (EnemyVillainId vid)]
      isConfused p `shouldReturn` True
      mainSchemeThreatNow `shouldReturn` before
      second <- encounterCard Cards.aos50151
      resolve [RevealBoostCard second (EnemyVillainId vid)]
      mainSchemeThreatNow `shouldReturn` (before+1)

  it "Power of the Atom's boost deals two indirect damage" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard Cards.aos50155
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      damageOf p `shouldReturn` 2

  it "Batroc the Leaper's boost discards exactly one card from hand" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      clearHand p
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      vid <- villainIdNow
      card <- encounterCard Cards.aos50163
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      length <$> handOf p `shouldReturn` 1

  it "S.H.I.E.L.D. Trooper places two threat when its engaged player cannot discard a S.H.I.E.L.D. ally or support" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      trooper <- addMinion p Cards.aos50178
      before <- mainSchemeThreatNow
      lifted $ L.dealDamage trooper p (toDamage 4 FromAbility)
      selectAny (MinionWithId trooper) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` (before+2)

  it "S.H.I.E.L.D. Trooper's defeat discards a S.H.I.E.L.D. support instead of placing threat" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      support <- putIntoPlay p "50008"
      trooper <- addMinion p Cards.aos50178
      before <- mainSchemeThreatNow
      lifted $ L.dealDamage trooper p (toDamage 4 FromAbility)
      getsGame (HM.member (supportIdOf support) . gameSupports) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` before

  it "Jolt places three threat when defeated" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      jolt <- addMinion p Cards.aos50133
      before <- mainSchemeThreatNow
      lifted $ L.dealDamage jolt p (toDamage 99 FromAbility)
      mainSchemeThreatNow `shouldReturn` (before+3)

  it "Innocent Bystanders starts with four counters, adds threat after each attack when no resource is spent, and discards on its fourth use" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.changeToHeroForm p
      clearHand p
      card <- encounterCard Cards.aos50134
      resolve [RevealEncounterCard p card]
      let oid=ObligationId $ unCardId $ ecCardId card
      getCountersOn (ObligationRef oid) `shouldReturn` 4
      vid <- villainIdNow
      before <- mainSchemeThreatNow
      for_ [1..4 :: Natural] $ \n -> do
        lifted $ L.attackEnemyWithoutDamage p vid p
        getCountersOn (ObligationRef oid) `shouldReturn` (4-n)
        mainSchemeThreatNow `shouldReturn` (before+n)
      getsGame (HM.member oid . gameObligations) `shouldReturn` False

  it "Jolt can be parleyed out of the game with three hero exhaustions without her defeat threat" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.changeToHeroForm p
      jolt <- addMinion p Cards.aos50133
      before <- mainSchemeThreatNow
      for_ [1..3 :: Natural] $ \n -> do
        choose "parley with Jolt" (isAbilityOf jolt)
        isExhaustedNow p `shouldReturn` True
        when (n<3) do
          getCountersOn jolt `shouldReturn` n
          lifted $ L.ready p
      selectAny (MinionWithId jolt) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` before
      getVictoryDisplay `shouldReturn` []

  it "Hard Sound Bindings replaces a basic hero attack with discarding itself and stunning the hero" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      changeForm p
      _ <- revealNow p Cards.aos50145
      pass
      choose "attempt basic attack" (isHeroAttack p)
      pass
      selectAny (AttachmentIs Cards.aos50145) `shouldReturn` False
      isStunned p `shouldReturn` True
      isExhaustedNow p `shouldReturn` True
      villainDamageNow `shouldReturn` 0

  it "an existing stunned status replaces the attack before Hard Sound Bindings can be discarded" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      changeForm p
      _ <- revealNow p Cards.aos50145
      pass
      lifted $ L.giveStatus p StunnedStatus
      choose "clear stun with basic attack" (isHeroAttack p)
      pass
      selectAny (AttachmentIs Cards.aos50145) `shouldReturn` True
      isStunned p `shouldReturn` False
      villainDamageNow `shouldReturn` 0

  it "Dance of Death resolves only the available different characters when the player controls no allies" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      card <- encounterCard Cards.aos50078
      resolve [RevealEncounterCard p card]
      damageOf p `shouldReturn` 1

  for_ [False,True] $ \prevented ->
    it ("Blasters offers its paid response only after the hero attacks and damages its attached enemy, prevented=" <> show prevented) do
      tg <- testGame ["spider-man"]
      let p=player1 tg
      inGame tg do
        changeForm p
        mach <- addMinion p Cards.aos50156
        card <- encounterCard Cards.aos50157
        resolve [RevealEncounterCard p card]
        energy <- putInHand p "01088"
        when prevented $ lifted $ L.giveStatus mach ToughStatus
        choose "attack" (isHeroAttack p)
        choose "attack MACH-IV" (isTargetChoice mach)
        let attachment=AttachmentId $ unCardId $ ecCardId card
        offers (isAbilityOf $ AttachmentRef attachment) `shouldReturn` not prevented
        unless prevented do
          choose "discard Blasters" (isAbilityOf $ AttachmentRef attachment)
          payWith [energy]
          selectAny (AttachmentIs Cards.aos50157) `shouldReturn` False

  it "Parcours du Combattant's boost does not move a minion after a scheme activation" do
    tg <- testGame ["spider-man","captain-marvel"]
    let p=player1 tg; other=player2 tg
    inGame tg do
      minion <- addMinion other Cards.aos50084
      _ <- setEncounterDeck [Cards.aos50164,Treacheries.advance,Treacheries.advance] []
      vid <- villainIdNow
      lifted $ L.villainSchemesAgainst vid p
      getMinionEngagedPlayer minion `shouldReturn` Just other
