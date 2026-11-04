{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Treachery.Treacheries.AgentsOfShield where

import Marvel.Prelude
import Marvel.AgentsOfShield.Board
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.AgentsOfShield.Helpers
import Marvel.Card
import Marvel.Deck (unDeck)
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Attack (Attack (..))
import Marvel.Minion.Types (Field (MinionDamage))
import Marvel.Projection
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hood.Helpers
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Deck
import Marvel.Message.Lifted hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype AosTreachery = AosTreachery (Attrs Treachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery AosTreachery where
  treacheryModifiersFor _ (VillainRef vid) (AosTreachery a) | toCardCode a == "50096" = do
    batroc <- enemyMatches (EnemyWithTitle "Batroc") (EnemyVillainId vid)
    pure [KeywordModifier Overkill | batroc]
  treacheryModifiersFor _ _ _ = pure []

aos50076 :: TreacheryCard AosTreachery
aos50076 = treachery AosTreachery Cards.aos50076

aos50077 :: TreacheryCard AosTreachery
aos50077 = treachery AosTreachery Cards.aos50077

aos50078 :: TreacheryCard AosTreachery
aos50078 = treachery AosTreachery Cards.aos50078

aos50079 :: TreacheryCard AosTreachery
aos50079 = treachery AosTreachery Cards.aos50079

aos50082 :: TreacheryCard AosNabbed
aos50082 = treachery (AosNabbed . (`With` 0)) Cards.aos50082

aos50096 :: TreacheryCard AosTreachery
aos50096 = treachery AosTreachery Cards.aos50096

aos50097 :: TreacheryCard AosTreachery
aos50097 = treachery AosTreachery Cards.aos50097

aos50102 :: TreacheryCard AosTreachery
aos50102 = treachery AosTreachery Cards.aos50102

aos50123 :: TreacheryCard AosTreachery
aos50123 = treachery AosTreachery Cards.aos50123

aos50124 :: TreacheryCard AosTreachery
aos50124 = treachery AosTreachery Cards.aos50124

aos50128 :: TreacheryCard AosTreachery
aos50128 = treachery AosTreachery Cards.aos50128

aos50137 :: TreacheryCard AosTreachery
aos50137 = treachery AosTreachery Cards.aos50137

aos50138 :: TreacheryCard AosTreachery
aos50138 = treachery AosTreachery Cards.aos50138

aos50141 :: TreacheryCard AosTreachery
aos50141 = treachery AosTreachery Cards.aos50141

aos50142 :: TreacheryCard AosTreachery
aos50142 = treachery AosTreachery Cards.aos50142

aos50147 :: TreacheryCard AosTreachery
aos50147 = treachery AosTreachery Cards.aos50147

aos50151 :: TreacheryCard AosTreachery
aos50151 = treachery AosTreachery Cards.aos50151

aos50155 :: TreacheryCard AosTreachery
aos50155 = treachery AosTreachery Cards.aos50155

aos50160 :: TreacheryCard AosTreachery
aos50160 = treachery AosTreachery Cards.aos50160

aos50163 :: TreacheryCard AosTreachery
aos50163 = treachery AosTreachery Cards.aos50163

aos50164 :: TreacheryCard AosTreachery
aos50164 = treachery AosTreachery Cards.aos50164

aos50175 :: TreacheryCard AosTreachery
aos50175 = treachery AosTreachery Cards.aos50175

aos50176 :: TreacheryCard AosMightMakesRight
aos50176 = treachery (AosMightMakesRight . (`With` False)) Cards.aos50176

aos50177 :: TreacheryCard AosTreachery
aos50177 = treachery AosTreachery Cards.aos50177

aos50184a :: TreacheryCard AosTreachery
aos50184a = treachery AosTreachery Cards.aos50184a

aos50184b :: TreacheryCard AosTreachery
aos50184b = treachery AosTreachery Cards.aos50184b

aos50184c :: TreacheryCard AosTreachery
aos50184c = treachery AosTreachery Cards.aos50184c

instance RunMessage AosTreachery where
  runMessage msg e@(AosTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      revealEffect you
      AosTreachery <$> runMessage msg a
    Boost inner@(CardSpecific target _ _) | target == toRef a -> runMessage inner e
    CardSpecific (isTarget a -> True) "aosGainSurge" _ -> e <$ gainSurge a
    CardSpecific (isTarget a -> True) "aosActivateFound" (fromJSON -> Success (you,def)) -> do
      activated <- activateFound you def
      unless activated $ gainSurge a
      pure e
    CardSpecific (isTarget a -> True) "aosNabbed" (fromJSON -> Success you) -> do
      discardFromDeck you 1 a
      pure e
    WithDiscarded (isTarget a -> True) FromDeck cards | toCardCode a == "50082" -> do
      for_ (headMay [c | PlayerCard c <- cards]) $ \card -> do
        if cardMatch (CardWithType AllyType) card then
          selectOne (SideSchemeIs Cards.aos50081) >>= traverse_ (\sid -> do
            placePlayerCardUnder sid card
            placeThreatOn a sid $ fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card
            placeSideSchemeAccelerationToken sid)
        else for_ (pcOwner card) $ \you -> do
          deck <- projectMap PlayerIdentityDeck unDeck you
          unless (null deck) $ cardSpecific a "aosNabbed" you
      pure e
    CardSpecific (isTarget a -> True) "aosDance" (fromJSON -> Success (you,amount :: Natural,used :: [CharacterId])) -> do
      chars <- filter (`notElem` used) <$> controlledCharacters you
      chooseEffect you [targeting (characterRef c) do
        damageCharacter c a $ toDamage amount FromAbility
        when (amount < 3) $ cardSpecific a "aosDance" (you,amount+1,c:used)
        | c <- chars]
      pure e
    CardSpecific (isTarget a -> True) "aosSecurityCameras" (fromJSON -> Success (you,chars :: [CharacterId])) -> do
      case chars of
        [] -> pure ()
        c:rest -> do
          exhausted <- case c of
            IdentityCharacter who -> identityMatches ExhaustedIdentity who
            AllyCharacter aid -> allyMatches ExhaustedAlly aid
            _ -> pure False
          high <- highAlert
          chooseEffect you $ [targeting (characterRef c) (case c of IdentityCharacter who -> exhaust who; AllyCharacter aid -> exhaust aid; _ -> pure ()) | not exhausted]
            <> [labeled "$label.aosRaiseAlert" (changeAlert $ if high then 2 else 1)]
          cardSpecific a "aosSecurityCameras" (you,rest)
      pure e
    CardSpecific (isTarget a -> True) "aosPlaceInterference" (fromJSON -> Success (you,prevented :: Natural,targets :: [Target])) -> do
      case targets of
        [] -> pure ()
        t:rest -> chooseEffect you $
          [labeled "$label.aosPreventSecret" (cardSpecific a "aosPlaceInterference" (you,prevented-1,rest)) | prevented > 0]
            <> [targeting t do
              placeCounters t 1
              cardSpecific a "aosPlaceInterference" (you,prevented,rest)
              | fromIntegral (length rest) >= prevented]
      pure e
    CardSpecific (isTarget a -> True) "aosMercenaryEntered" (fromJSON -> Success (you,before :: [MinionId])) -> do
      after <- selectList (MinionEngagedWith $ IdentityWithId you)
      unless (any (`notElem` before) after) $ gainSurge a
      pure e
    CardSpecific (isTarget a -> True) "aosAdaptoidEntered" (fromJSON -> Success (you,before :: [MinionId])) -> do
      after <- selectList (MinionEngagedWith $ IdentityWithId you)
      unless (any (`notElem` before) after) $ dealEncounterCard you
      pure e
    CardSpecific (isTarget a -> True) "attackCompleted" (fromJSON -> Success (details :: Maybe Attack)) | toCardCode a == "50176" -> do
      when (maybe True ((==0) . attackDamage) details) do
        first <- selectJust FirstPlayer
        removeSecrets first 3
      pure e
    CardSpecific (isTarget a -> True) "aosFindTapIn" (fromJSON -> Success you) -> do
      mids <- selectList (MinionWithTrait Trait.Thunderbolt <> NotMinion (MinionEngagedWith $ IdentityWithId you))
      scored <- traverse (\mid -> (mid,) <$> getMinionDamageInPlay mid) mids
      if null scored then currentVillainActivates you
      else chooseEffect you [targeting mid do
        engageMinion mid you
        cardSpecific a "aosTapInActivate" (you,mid)
        | (mid,d) <- scored, d == minimum (map snd scored)]
      pure e
    CardSpecific (isTarget a -> True) "aosTapInActivate" (fromJSON -> Success (you,mid)) -> do
      active <- activateMinion you mid
      unless active $ currentVillainActivates you
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "50102" -> do
          payable <- canAffordCost you (toSource a) (ResourceCost Nothing)
          chooseEffect you $
            [labeled "$label.aosSpendResource" (payForTreachery you $ ResourceCost Nothing) | payable]
            <> [labeled "$label.aosAddThreeBoost" (enemyActivationBonus enemy 3)]
        "50124" -> pure ()
        "50128" -> pure ()
        "50141" -> exhaustCharacterChoice you
        "50145" -> pure ()
        "50147" -> dealBoost enemy
        "50151" -> do
          already <- identityMatches ConfusedIdentity you
          confuseIdentity you
          when already $ placeThreatOnEach a 1 MainScheme
        "50155" -> dealIndirectDamage you a 2
        "50160" -> whenM (isJust <$> getEnemyAttack enemy) $ attackGains enemy Overkill >> attackGains enemy Ranged
        "50163" -> discardFromHand you 1
        "50164" -> whenM (isJust <$> getEnemyAttack enemy) $ enemyActivationAfterCurrent enemy do
          mids <- selectList (NotMinion $ MinionEngagedWith $ IdentityWithId you)
          chooseEffect you [targeting mid (engageMinion mid you) | mid <- mids]
        "50184a" -> interference you Energy
        "50184b" -> interference you Mental
        "50184c" -> interference you Physical
        _ -> pure ()
      pure e
    CheckWindows windows | toCardCode a == "50175" -> do
      for_ [(scheme,amount) | W.Window W.Would (W.ThreatPlaced W.ThreatFromVillain scheme amount) <- windows] $ \(scheme,amount) -> do
        you <- maybe getActivePlayerId pure (treacheryResolver a)
        payable <- filterM (\n -> canAffordCost you (toSource a) $ MultiResourceCost $ replicate (fromIntegral n) $ Just Mental) [1..amount]
        chooseEffect you $ [labeled (countLabel "aosPreventThreat" n)
          (payThen you a (MultiResourceCost $ replicate (fromIntegral n) $ Just Mental) $ cardSpecific a "aosPreventThreat" (you,scheme,n)) | n <- payable]
          <> [labeled "$label.done" $ pure ()]
      pure e
    CardSpecific (isTarget a -> True) "aosPreventThreat" (fromJSON -> Success (you,scheme,n :: Natural)) -> do
      replaceMatchingMessage (\case
        MainSchemeMessage sid (MainSchemePlaceThreat amount) -> [MainSchemeMessage sid (MainSchemePlaceThreat $ subtractNatural n amount)]
        other -> [other]) (\case
          MainSchemeMessage sid MainSchemePlaceThreat{} -> scheme == SchemeMainSchemeId sid
          _ -> False)
      removeSecrets you n
      pure e
    CheckWindows windows | toCardCode a == "50124" -> do
      let damaged = [IdentityCharacter you | W.Window W.After (W.IdentityTakeDamageFrom you source d) <- windows, source == toSource a, damageAmount d > 0]
            <> [AllyCharacter aid | W.Window W.After (W.AllyTakeDamageFrom aid source d) <- windows, source == toSource a, damageAmount d > 0]
      traverse_ (`giveStatus` ConfusedStatus) damaged
      pure e
    Boost (CheckWindows windows) | toCardCode a == "50128" -> do
      n <- aimVictory
      for_ [card | W.Window _ (W.CountingBoostIcons card) <- windows, toCardCode card == toCardCode a] $ \card -> changeBoostIconCount card (+n)
      pure e
    _ -> AosTreachery <$> runMessage msg a
   where
    currentVillainActivates you = withTheVillain you (`villainActivatesAgainst` you)
    highAlert = selectAny (EnvironmentIs Cards.aos50090b)
    changeAlert n = selectOne (EnvironmentWithTitle "Alert Level") >>= traverse_ (`placeCounters` n)
    aimVictory = fromIntegral . length . filter (cardMatch $ CardWithTrait Trait.Aim) <$> getVictoryDisplay
    interference you resource = do
      targets <- boardMembers
      payable <- filterM (\n -> canAffordCost you (toSource a) $ MultiResourceCost $ replicate (fromIntegral n) $ Just resource) [1..fromIntegral $ length targets]
      chooseEffect you $ [labeled (countLabel "aosPreventSecrets" n)
        (payThen you a (MultiResourceCost $ replicate (fromIntegral n) $ Just resource) $ cardSpecific a "aosPlaceInterference" (you,n,targets)) | n <- payable]
        <> [labeled "$label.done" $ cardSpecific a "aosPlaceInterference" (you,0 :: Natural,targets)]
    revealEffect you = case toCardCode a of
      "50076" -> do
        whenM (identityMatches AlterEgoIdentity you) $ changeToHeroForm you
        currentVillainAttacksWithAdditionalBoostCards you 1
      "50077" -> confuseIdentity you >> currentVillainSchemes you
      "50078" -> cardSpecific a "aosDance" (you,1 :: Natural,[] :: [CharacterId])
      "50079" -> do
        already <- identityMatches StunnedIdentity you
        stunIdentity you
        damageIdentity you a $ toDamage (if already then 2 else 1) FromAbility
      "50082" -> do
        absent <- selectNone (SideSchemeIs Cards.aos50081)
        when absent $ searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Cards.aos50081)
        cardSpecific a "aosNabbed" you
      "50096" -> do
        hero <- identityMatches HeroIdentity you
        if not hero then currentVillainSchemes you else do
          allies <- selectList AnyAlly
          if null allies then currentVillainAttacks you
          else chooseCharacterWithMostHP you (map AllyCharacter allies) $ \case
            AllyCharacter aid -> do
              controller <- fromMaybe you <$> getAllyControllerId aid
              withTheVillain controller $ \vid -> do
                villainStep vid $ VillainAttacksAlly controller aid
            _ -> pure ()
      "50097" -> do
        hero <- identityMatches HeroIdentity you
        if hero then controlledCharacters you >>= \chars -> cardSpecific a "aosSecurityCameras" (you,chars)
        else do
          selectOne (EnvironmentWithTitle "Alert Level") >>= traverse_ (`removeCounters` 1)
          gainSurge a
      "50102" -> do
        let cost = MultiResourceCost $ replicate 3 Nothing
        payable <- canAffordCost you (toSource a) cost
        chooseEffect you $ [labeled "$label.aosSpendThree" (payForTreachery you cost) | payable]
          <> [labeled "$label.aosFindMercenary" do
            before <- selectList (MinionEngagedWith $ IdentityWithId you)
            searchEverywhereAndRevealEncounterCard you (CardWithType MinionType <> CardWithTrait Trait.Mercenary)
            cardSpecific a "aosMercenaryEntered" (you,before)]
      "50123" -> do
        players <- getPlayers
        for_ players $ \p -> do
          before <- selectList (MinionEngagedWith $ IdentityWithId p)
          searchAndRevealEncounterCard p (CardWithTrait Trait.Adaptoid <> CardWithType MinionType)
          cardSpecific a "aosAdaptoidEntered" (p,before)
      "50124" -> do
        hero <- identityMatches HeroIdentity you
        if hero then do
          villains <- selectList (EnemyWithTitle "M.O.D.O.K.")
          for_ villains $ \enemy -> do
            n <- getEnemyScheme enemy
            dealIndirectDamage you a n
        else confuseIdentity you >> currentVillainSchemes you
      "50128" -> aimVictory >>= discardFromHand you
      "50137" -> cardSpecific GameRef "aosReturnThunderbolt" (you,toTarget a)
      "50138" -> cardSpecific a "aosFindTapIn" you
      "50141" -> findMinion you Cards.aos50139 a
      "50142" -> do
        hero <- identityMatches HeroIdentity you
        if not hero then do
          discarded <- discardControlledOfTypes you [AllyType,SupportType]
          unless discarded $ gainSurge a
        else do
          n <- characterAttackValue (IdentityCharacter you)
          chars <- friendlyCharactersWithFewestRemainingHitPoints
          chooseEffect you [targeting (characterRef c) (damageCharacter c a $ toDamage n FromAbility) | c <- chars]
      "50147" -> findMinion you Cards.aos50143 a
      "50151" -> findMinion you Cards.aos50148 a
      "50155" -> findMinion you Cards.aos50152 a
      "50160" -> findMinion you Cards.aos50156 a
      "50163" -> findMinion you Cards.aos50161 a
      "50164" -> rotateMinions
      "50175" -> currentVillainSchemes you
      "50176" -> do
        hero <- identityMatches HeroIdentity you
        if hero then do
          withTheVillain you $ \vid -> enemyAttacksWithCompletion vid you a
        else addSecretsToLeast 2
      "50177" -> do
        cards <- controlledCards you [AllyType,SupportType]
        enemies <- selectList AnyMinion
        scored <- traverse (\mid -> (mid,) <$> getEnemyRemainingHitPoints (EnemyMinionId mid)) enemies
        chooseEffect you $
          [targeting mid (defeatMinionBy a mid >> currentVillainSchemes you) | (mid,hp) <- scored, hp == minimum (map snd scored)]
          <> [targeting target do
            def <- getsGame (`abilityCardDef` target)
            removeFromPlay target
            removeSecrets you $ fromIntegral $ max 0 $ fromMaybe 0 $ def >>= cdCost
            | target <- cards]
      "50184a" -> interference you Energy
      "50184b" -> interference you Mental
      "50184c" -> interference you Physical
      _ -> pure ()


-- Actual taken-damage windows decide this card, rather than the attack's ATK.
newtype AosMightMakesRight = AosMightMakesRight (Attrs Treachery `With` Bool)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery AosMightMakesRight where
  toTreacheryAttrs (AosMightMakesRight (a `With` _)) = a

instance RunMessage AosMightMakesRight where
  runMessage msg e@(AosMightMakesRight (a `With` damaged)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      hero <- identityMatches HeroIdentity you
      if hero then withTheVillain you $ \vid -> enemyAttacksWithCompletion vid you a
      else addSecretsToLeast 2
      pure $ AosMightMakesRight (a `With` False)
    CheckWindows windows -> do
      let damageFromZemo source d = damageAmount d > 0 && case source of
            VillainRef _ -> damageSource d `elem` [FromAttack,FromOverkill]
            _ -> False
          took = any (\case
            W.Window W.After (W.IdentityTakeDamageFrom _ source d) -> damageFromZemo source d
            W.Window W.After (W.AllyTakeDamageFrom _ source d) -> damageFromZemo source d
            W.Window W.After (W.EnemyTookDamageFrom _ d source) -> damageFromZemo source d
            _ -> False) windows
      pure $ AosMightMakesRight (a `With` (damaged || took))
    CardSpecific (isTarget a -> True) "attackCompleted" (fromJSON -> Success (details :: Maybe Attack)) -> do
      when (isJust details && not damaged) do
        first <- selectJust FirstPlayer
        removeSecrets first 3
      pure e
    _ -> AosMightMakesRight . (`With` damaged) <$> runMessage msg a


-- The discard stops at the end of the original deck, even if that discard
-- reshuffles the identity's deck before its completion callback.
newtype AosNabbed = AosNabbed (Attrs Treachery `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery AosNabbed where
  toTreacheryAttrs (AosNabbed (a `With` _)) = a

instance RunMessage AosNabbed where
  runMessage msg e@(AosNabbed (a `With` remaining)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      absent <- selectNone (SideSchemeIs Cards.aos50081)
      when absent $ searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Cards.aos50081)
      cardSpecific a "aosStartNabbed" you
      pure e
    CardSpecific (isTarget a -> True) "aosStartNabbed" (fromJSON -> Success you) -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      unless (null deck) $ discardFromDeck you 1 a
      pure $ AosNabbed (a `With` subtractNatural 1 (fromIntegral $ length deck))
    WithDiscarded (isTarget a -> True) FromDeck cards -> do
      case headMay [c | PlayerCard c <- cards] of
        Just card | cardMatch (CardWithType AllyType) card -> do
          selectOne (SideSchemeIs Cards.aos50081) >>= traverse_ (\sid -> do
            placePlayerCardUnder sid card
            placeThreatOn a sid $ fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card
            placeSideSchemeAccelerationToken sid)
          pure $ AosNabbed (a `With` 0)
        Just card | remaining > 0 -> do
          for_ (pcOwner card) $ \you -> discardFromDeck you 1 a
          pure $ AosNabbed (a `With` (remaining-1))
        _ -> pure e
    _ -> AosNabbed . (`With` remaining) <$> runMessage msg a
