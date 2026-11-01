module Marvel.Minion.Minions.HoodPack where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attack
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Projection
import Marvel.Hood.Helpers
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Deck
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype HoodMinion = HoodMinion (Attrs Minion)
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

hood24015 :: MinionCard HoodMinion
hood24015 = minion HoodMinion Cards.hood24015 (Sch 1) (Atk 3) (HP 7)

hood24016 :: MinionCard HoodMinion
hood24016 = minion HoodMinion Cards.hood24016 (Sch 2) (Atk 2) (HP 6)

hood24018 :: MinionCard HoodMinion
hood24018 = minion HoodMinion Cards.hood24018 (Sch 1) (Atk 1) (HP 8)

hood24024 :: MinionCard HoodMinion
hood24024 = minion HoodMinion Cards.hood24024 (Sch 2) (Atk 1) (HP 5)

hood24025 :: MinionCard HoodMinion
hood24025 = minion HoodMinion Cards.hood24025 (Sch 1) (Atk 2) (HP 4)

hood24026 :: MinionCard HoodMinion
hood24026 = minion HoodMinion Cards.hood24026 (Sch 2) (Atk 2) (HP 4)

hood24027 :: MinionCard HoodMinion
hood24027 = minion HoodMinion Cards.hood24027 (Sch 1) (Atk 1) (HP 5)

hood24034 :: MinionCard HoodMinion
hood24034 = minion HoodMinion Cards.hood24034 (Sch 3) (Atk 1) (HP 4)

hood24035 :: MinionCard HoodMinion
hood24035 = minion HoodMinion Cards.hood24035 (Sch 1) (Atk 3) (HP 10)

hood24041 :: MinionCard HoodMinion
hood24041 = minion HoodMinion Cards.hood24041 (Sch 0) (Atk 1) (HP 3)

hood24043 :: MinionCard HoodMinion
hood24043 = minion HoodMinion Cards.hood24043 (Sch 1) (Atk 2) (HP 4)

hood24044 :: MinionCard HoodMinion
hood24044 = minion HoodMinion Cards.hood24044 (Sch 2) (Atk 1) (HP 5)

hood24045 :: MinionCard HoodMinion
hood24045 = minion HoodMinion Cards.hood24045 (Sch 1) (Atk 2) (HP 5)

hood24046 :: MinionCard HoodMinion
hood24046 = minion HoodMinion Cards.hood24046 (Sch 1) (Atk 2) (HP 3)

hood24047 :: MinionCard HoodMinion
hood24047 = minion HoodMinion Cards.hood24047 (Sch 2) (Atk 2) (HP 3)

hood24065 :: MinionCard HoodMinion
hood24065 = minion HoodMinion Cards.hood24065 (Sch 2) (Atk 2) (HP 8)

hood24066 :: MinionCard HoodMinion
hood24066 = minion HoodMinion Cards.hood24066 (Sch 1) (Atk 3) (HP 7)

hood24067 :: MinionCard HoodMinion
hood24067 = minion HoodMinion Cards.hood24067 (Sch 2) (Atk 2) (HP 6)

hood24068 :: MinionCard HoodMinion
hood24068 = minion HoodMinion Cards.hood24068 (Sch 3) (Atk 1) (HP 7)

instance HasModifiersFor HoodMinion where
  getModifiersFor _ target (HoodMinion a) = case toCardCode a of
    "24016" | target == toRef a -> do
      chars <- selectList AnyCharacter
      n <- fromIntegral . length <$> filterM (characterHasCondition ConfusedStatus) chars
      pure [KeywordModifier (Retaliate n) | n > 0]
    "24027" -> do
      affected <- case target of
        IdentityRef you -> pure $ you == minionEngagedIdentity a
        AllyRef aid -> allyMatches (AllyControlledBy $ IdentityWithId $ minionEngagedIdentity a) aid
        _ -> pure False
      hero <- case target of IdentityRef you -> identityMatches HeroIdentity you; _ -> pure True
      pure [AdditionalReadyCost (ResourceCost $ Just Mental) | affected && hero]
    "24026" | target == toRef a -> pure [KeywordModifier Overkill, KeywordModifier Ranged]
    "24066" | target == toRef a -> pure [KeywordModifier Overkill]
    _ -> pure []

instance HasAbilities HoodMinion where
  getAbilities (HoodMinion a) =
    [windowAbility a 1 (W.EnemyAttackedAndDamaged enemy AnyCharacter) ForcedResponse NoCost (runAbility a 1) | code `elem` ["24015","24043"]]
    <> [windowAbility a 7 (W.CharacterWouldTakeAttackDamageFrom enemy AnyCharacter) ForcedInterrupt NoCost (runAbility a 7) | code == "24024"]
    <> [windowAbility a 2 (W.EnemyActivates W.When enemy AnyIdentity) ForcedInterrupt NoCost (runAbility a 2) | code == "24018"]
    <> [windowAbility a 3 (W.EnemyAttacked W.After enemy AnyIdentity) ForcedResponse NoCost (runAbility a 3) | code `elem` ["24044","24068"]]
    <> [windowAbility a 4 matcher ForcedResponse NoCost (runAbility a 4) | code == "24045", matcher <- attacked W.After]
    <> [windowAbility a 5 matcher ForcedInterrupt NoCost (runAbility a 5) | code == "24046", matcher <- attacked W.When]
    <> [windowAbility a 6 (W.EnemyWouldAttack enemy AnyIdentity) ForcedInterrupt NoCost (runAbility a 6) | code == "24047"]
   where
    code = toCardCode a
    enemy = EnemyWithId $ EnemyMinionId $ minionId a
    attacked timing = [W.IdentityAttacked timing AnyIdentity enemy, W.AllyAttacked timing AnyAlly enemy]

instance RunMessage HoodMinion where
  runMessage msg e@(HoodMinion attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "hoodZaboSchemeBonus" _ -> pure $ HoodMinion attrs {minionScheme = Sch (unSch (minionScheme attrs) + 3)}
    CardSpecific (isTarget attrs -> True) "hoodZaboSchemeBonusEnd" _ -> pure $ HoodMinion attrs {minionScheme = Sch (subtractNatural 3 $ unSch $ minionScheme attrs)}
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      case toCardCode attrs of
        "24016" -> controlledCharacters you >>= traverse_ (`giveStatus` ConfusedStatus)
        "24025" -> do
          allies <- selectList (AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly)
          traverse_ exhaust allies
          selectList EncounterMainScheme >>= traverse_ (\sid -> placeThreat sid (fromIntegral $ length allies))
        "24034" -> do
          hydes <- selectList (MinionIs Cards.hood24035)
          for_ hydes $ \hyde -> do
            removeFromPlay attrs
            minionAttacksWithChange hyde you 2 [Overkill]
        "24035" -> do
          zabos <- selectList (MinionIs Cards.hood24034)
          for_ zabos $ \zabo -> do
            engaged <- selectJust (IdentityEngagedWith $ MinionWithId zabo)
            removeFromPlay zabo
            minionStep mid (MinionEngagedIdentity engaged)
            giveStatus mid ToughStatus
            chars <- selectList (CharacterControlledBy AnyIdentity)
            heroesAndAllies <- filterM (\case IdentityCharacter p -> identityMatches HeroIdentity p; _ -> pure True) chars
            for_ heroesAndAllies $ \c -> damageCharacter c attrs (toDamage 1 FromAbility)
        _ -> pure ()
      HoodMinion <$> runMessage msg attrs
    MinionMessage mid MinionDefeated | mid == minionId attrs -> do
      stunned <- anyM (characterHasCondition StunnedStatus) =<< selectList (CharacterControlledBy AnyIdentity)
      if toCardCode attrs == "24015" && stunned then e <$ shuffleMinionIntoEncounterDeck mid else do
        when (toCardCode attrs == "24034") $ searchAndPutIntoPlayEncounterCard (minionEngagedIdentity attrs) (CardIs Cards.hood24035)
        HoodMinion <$> runMessage msg attrs
    -- Crossfire chooses the attack's actual target before defense is declared.
    MinionMessage mid (MinionBeginAttack you) | mid == minionId attrs, toCardCode attrs == "24026" -> do
      chars <- friendlyCharactersWithFewestRemainingHitPoints
      first <- selectJust FirstPlayer
      chooseOrRunOne first [targeting (characterRef c) (cardSpecific attrs "crossfireTarget" (you,c)) | c <- chars]
      pure e
    CardSpecific (isTarget attrs -> True) "crossfireTarget" (fromJSON -> Success (you,c)) ->
      HoodMinion <$> runMessage (MinionMessage (minionId attrs) $ MinionBeginAttack you)
        attrs {minionAttackTarget = Just c, minionAttacking = fmap (\a -> a {attackCharacter = c, attackTarget = Just c}) (minionAttacking attrs)}
    MinionMessage mid MinionAttacked | mid == minionId attrs, toCardCode attrs == "24065" -> do
      extra <- case minionAttacking attrs of
        Just a | not (attackDefended a) -> pure 2
        _ -> pure 0
      let updated = attrs {minionAttacking = fmap (\a -> a {attackChange = attackChange a + fromIntegral extra}) (minionAttacking attrs)}
      HoodMinion <$> runMessage msg updated
    RanAbility _ (isTarget attrs -> True) 7 windows _ -> do
      let damaged = [IdentityCharacter you | W.IdentityTakeDamageFrom you _ _ <- windows]
            <> [AllyCharacter aid | W.AllyTakeDamageFrom aid _ _ <- windows]
      for_ (headMay damaged) $ \c -> characterAttackValue c >>= increasePendingCharacterDamage c
      pure e
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [c | W.EnemyAttacksAndDamages _ c _ <- windows] $ \c ->
        if toCardCode attrs == "24015" then giveStatus c StunnedStatus
        else case c of IdentityCharacter who -> void $ discardControlledCost False who [UpgradeType]; _ -> pure ()
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      discardUntil FromEncounterDeck (CardWithType AttachmentType) attrs
      pure e
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard c) -> do
      you <- getBoostedPlayer (EnemyMinionId $ minionId attrs)
      revealEncounterCard you c
      pure e
    RanAbility resolver (isTarget attrs -> True) 3 windows _ -> do
      let you = fromMaybe resolver $ headMay ([who | W.EnemyAttack _ who <- windows] <> [who | W.EnemyAttackAlly _ _ who <- windows])
      chars <- if toCardCode attrs == "24044" then map AllyCharacter <$> selectList (AllyControlledBy $ IdentityWithId you) else controlledCharacters you
      for_ chars $ \c -> damageCharacter c attrs (toDamage 1 FromAbility)
      pure e
    RanAbility you (isTarget attrs -> True) n windows _ | n `elem` [4,5] -> do
      let attackers = [IdentityCharacter who | W.IdentityAttack who _ <- windows]
            <> [AllyCharacter aid | W.AllyAttack aid _ <- windows]
      for_ (headMay attackers) $ \c -> if n == 4 then giveStatus c StunnedStatus else case c of
        IdentityCharacter who -> enemyAttacks (minionId attrs) who
        AllyCharacter aid -> do
          controller <- maybe (pure you) pure =<< getSourceController (AllyRef aid)
          enemyAttacksAlly (minionId attrs) controller aid
        _ -> pure ()
      pure e
    RanAbility resolver (isTarget attrs -> True) 6 windows _ -> do
      let you = fromMaybe resolver $ headMay [who | W.EnemyAttack _ who <- windows]
      discardFor you attrs RandomFromHand 1 1
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      case toCardCode attrs of
        "24018" -> do
          first <- selectJust FirstPlayer
          villain <- selectJust ActiveVillain
          card <- getBoostCard (toTarget attrs)
          for_ card $ \c -> do
            consumeBoostCard c
            activationAfter enemy villain (putEncounterCardIntoPlay first c)
        "24025" -> exhaustCharacterChoice you
        "24027" -> discardFromDeckUntil you (CardWithType AllyType) attrs
        "24043" -> void $ discardControlledOfTypes you [UpgradeType]
        "24044" -> do
          allies <- selectList (AllyControlledBy $ IdentityWithId you)
          chooseOne you [targeting aid (damageCharacter aid attrs $ toDamage 2 FromAbility) | aid <- allies]
        "24045" -> do
          chars <- controlledCharacters you
          scored <- traverse (\c -> (c,) <$> characterAttackValue c) chars
          let highest = maximum (0 : map snd scored)
          chooseOne you [targeting (characterRef c) (giveStatus c StunnedStatus) | (c,n) <- scored, n == highest]
        "24046" -> void $ discardControlledCost False you [SupportType]
        "24047" -> do
          cards <- unHand <$> project PlayerIdentityHand you
          let specific = filter (cardMatch HeroSpecificCard) cards
          chooseOne you [targeting c (discardCard c) | c <- specific]
        _ -> pure ()
      pure e
    _ -> HoodMinion <$> runMessage msg attrs
