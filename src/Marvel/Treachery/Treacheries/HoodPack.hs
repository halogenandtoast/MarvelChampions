module Marvel.Treachery.Treacheries.HoodPack where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Discard ()
import Marvel.EncounterSet qualified as Set
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hood.Helpers
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Deck
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.SideScheme.Types ()
import Marvel.Trait qualified as Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Treacheries.ShadowOfThePast (ShadowOfThePast (..))
import Marvel.Window qualified as W
import Marvel.Treachery.Types

newtype HoodTreachery = HoodTreachery (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

hood24017 :: TreacheryCard HoodTreachery
hood24017 = treachery HoodTreachery Cards.hood24017

hood24028 :: TreacheryCard HoodTreachery
hood24028 = treachery HoodTreachery Cards.hood24028

hood24029 :: TreacheryCard HoodTreachery
hood24029 = treachery HoodTreachery Cards.hood24029

hood24030 :: TreacheryCard HoodTreachery
hood24030 = treachery HoodTreachery Cards.hood24030

hood24031 :: TreacheryCard HoodTreachery
hood24031 = treachery HoodTreachery Cards.hood24031

hood24032 :: TreacheryCard HoodTreachery
hood24032 = treachery HoodTreachery Cards.hood24032

hood24036 :: TreacheryCard HoodTreachery
hood24036 = treachery HoodTreachery Cards.hood24036

hood24048 :: TreacheryCard HoodTreachery
hood24048 = treachery HoodTreachery Cards.hood24048

hood24050 :: TreacheryCard HoodTreachery
hood24050 = treachery HoodTreachery Cards.hood24050

hood24051 :: TreacheryCard HoodTreachery
hood24051 = treachery HoodTreachery Cards.hood24051

hood24052 :: TreacheryCard HoodTreachery
hood24052 = treachery HoodTreachery Cards.hood24052

hood24053 :: TreacheryCard HoodTreachery
hood24053 = treachery HoodTreachery Cards.hood24053

hood24054 :: TreacheryCard HoodTreachery
hood24054 = treachery HoodTreachery Cards.hood24054

hood24059 :: TreacheryCard HoodTreachery
hood24059 = treachery HoodTreachery Cards.hood24059

hood24069 :: TreacheryCard HoodTreachery
hood24069 = treachery HoodTreachery Cards.hood24069

hood24070 :: TreacheryCard HoodTreachery
hood24070 = treachery HoodTreachery Cards.hood24070

instance RunMessage HoodTreachery where
  runMessage msg e@(HoodTreachery attrs)
    | toCardCode attrs == "24053" = do
        ShadowOfThePast updated <- runMessage msg (ShadowOfThePast attrs)
        pure $ HoodTreachery updated
    | otherwise = runQueueT $ case msg of
    Boost (CheckWindows windows) | toCardCode attrs == "24069" -> do
      for_ [card | W.Window _ (W.CountingBoostIcons card) <- windows, toCardCode card == toCardCode attrs] $ \card -> do
        n <- selectListCount (MinionWithTrait Trait.Elite)
        changeBoostIconCount card (+ n)
      pure e
    Boost inner@(CardSpecific target _ _) | target == toRef attrs -> runMessage inner e
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      revealEffect you
      HoodTreachery <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "confuseAfterStun" (fromJSON -> Success you) -> do
      chooseStatus you ConfusedStatus
      pure e
    CardSpecific (isTarget attrs -> True) "crossfireIndirect" (fromJSON -> Success you) -> do
      n <- selectListCount (MinionWithTrait Trait.CrossfiresCrew)
      dealIndirectDamage you attrs n
      pure e
    CardSpecific (isTarget attrs -> True) "ruinationThreat" _ -> do
      placeThreatOnEach attrs 2 AnyScheme
      pure e
    CardSpecific (isTarget attrs -> True) "hydeFormulaDamage" (fromJSON -> Success zabo) -> do
      dealDamage (zabo :: MinionId) attrs (toDamage 4 FromAbility)
      pure e
    CardSpecific (isTarget attrs -> True) "hydeFormulaAttack" (fromJSON -> Success you) -> do
      hydes <- selectList (MinionIs Minions.hood24035)
      for_ hydes $ \hyde -> giveStatus hyde ToughStatus >> enemyAttacks hyde you
      pure e
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      you <- maybe getYou pure (treacheryResolver attrs)
      revealEncounterCard you card
      pure e
    WithDiscarded (isTarget attrs -> True) FromEncounterDeck cards | toCardCode attrs == "24051" -> do
      you <- maybe getYou pure (treacheryResolver attrs)
      for_ (headMay [c | EncounterCard c <- cards, cdCardType (getCardDef c) == MinionType]) (putEncounterCardIntoPlay you)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      case toCardCode attrs of
        "24017" -> doubleTrouble you
        "24029" -> dealEncounterCard you
        "24032" -> do
          controlledCharacters you >>= traverse_ (\c -> damageCharacter c attrs (toDamage 1 FromAbility))
          withTheVillain you dealBoost
        "24050" -> withTheVillain you dealBoost
        "24052" -> withTheVillain you dealBoost
        "24054" -> case enemy of
          EnemyVillainId vid -> whenM (isJust <$> getVillainAttack vid) $ attackGains vid Overkill
          _ -> pure ()
        "24059" -> citywideCrisis you
        _ -> pure ()
      pure e
    _ -> HoodTreachery <$> runMessage msg attrs
   where
    doubleTrouble you = do
      chooseStatus you StunnedStatus
      cardSpecific attrs "confuseAfterStun" you
    revealEffect you = case toCardCode attrs of
      "24017" -> doubleTrouble you
      "24028" -> do
        discardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait Trait.CrossfiresCrew) attrs
        cardSpecific attrs "crossfireIndirect" you
      "24029" -> do
        getPlayers >>= traverse_ dealEncounterCard
        selectOne ActiveVillain >>= traverse_ dealBoost
      "24030" -> do
        discardUntil FromEncounterDeck (CardWithType SideSchemeType) attrs
        cardSpecific attrs "ruinationThreat" ()
      "24031" -> do
        code <- getIdentityCode you
        let nemesis = Set.getNemesisSet code
            defs = filter ((== Just nemesis) . cdEncounterSet) Minions.designatedNemesisMinions
        searchEverywhereAndPutIntoPlayEncounterCard you (CardWithType MinionType <> CardInEncounterSet nemesis <> (if null defs then AnyCard else CardMatchesAny (map CardIs defs)))
      "24032" -> exhaust you >> damageIdentity you attrs (toDamage 2 FromAbility)
      "24036" -> do
        zabos <- selectList (MinionIs Minions.hood24034)
        hydes <- selectList (MinionIs Minions.hood24035)
        for_ zabos $ \zabo -> do
          cardSpecific zabo "hoodZaboSchemeBonus" ()
          minionSchemesAgainst zabo you
          cardSpecific zabo "hoodZaboSchemeBonusEnd" ()
          cardSpecific attrs "hydeFormulaDamage" zabo
        cardSpecific attrs "hydeFormulaAttack" you
        when (null zabos && null hydes) $ gainSurge attrs
      "24048" -> do
        enemies <- selectList (EnemyWithTrait Trait.Criminal)
        actual <- filterM (activationWillResolve you) enemies
        for_ enemies $ \enemy -> case enemy of
          EnemyVillainId vid -> villainActivatesAgainst vid you
          EnemyMinionId mid -> do
            hero <- identityMatches HeroIdentity you
            if hero then enemyAttacks mid you else minionSchemesAgainst mid you
          EnemyAttachmentId _ -> pure ()
        when (null actual) $ gainSurge attrs
      "24050" -> withTheVillain you $ \vid -> do
        villainStep vid (VillainSchemesWithBonus you 1)
      "24051" -> do
        hero <- identityMatches HeroIdentity you
        if hero then do
          currentVillainAttacks you
          selectList (MinionEngagedWith $ IdentityWithId you) >>= traverse_ (`enemyAttacks` you)
          gainSurge attrs
        else discardTopOfEncounterDeckFor 7 attrs
      "24052" -> do
        discarded <- discardControlledCost True you [UpgradeType, SupportType]
        unless discarded $ gainSurge attrs
      "24054" -> do
        hero <- identityMatches HeroIdentity you
        when hero $ withTheVillain you (\vid -> enemyAttacksGaining vid you [Overkill])
      "24059" -> citywideCrisis you
      "24069" -> do
        minions <- selectList (MinionWithTrait Trait.Elite)
        actual <- for minions $ \mid -> do
          you' <- selectJust (IdentityEngagedWith $ MinionWithId mid)
          active <- activationWillResolve you' (EnemyMinionId mid)
          hero <- identityMatches HeroIdentity you'
          if hero then enemyAttacks mid you' else minionSchemesAgainst mid you'
          pure active
        unless (or actual) $ gainSurge attrs
      "24070" -> do
        enemies <- selectList (EnemyWithTrait Trait.Brute)
        available <- filterM (characterMatches CharacterCanGainTough . toCharacter) enemies
        traverse_ (`giveStatus` ToughStatus) enemies
        when (null available) $ discardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait Trait.Brute) attrs
      _ -> pure ()
    citywideCrisis _ = do
      schemes <- selectList AnySideScheme
      revealed <- filterM (\sid -> maybe False SideSchemes.hasWhenRevealed <$> getsGame (`abilityCardDef` SideSchemeRef sid)) schemes
      if null revealed then placeThreatOnEach attrs 2 AnyScheme
      else for_ revealed (\sid -> sideSchemeStep sid RevealSideScheme)
