module Marvel.CivilWar.LeaderTreacheries where

import Marvel.Prelude
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Damage
import Marvel.Entity
import Marvel.EncounterSet qualified as E
import Marvel.Game.Source
import Marvel.Hood.Helpers (exhaustCharacterChoice,controlledCards)
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.Trait qualified as T
import Marvel.Treachery.Types

newtype LeaderTreachery = LeaderTreachery (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

leaderTreacheryDefinitions :: [CardDef]
leaderTreacheryDefinitions = [C.cw56070,C.cw56071,C.cw56101,C.cw56102,C.cw56103,C.cw56146,C.cw56147,C.cw56176,C.cw56178,C.cw57009,C.cw57010]

leaderTreacheries :: [TreacheryCard LeaderTreachery]
leaderTreacheries = map (treachery LeaderTreachery) leaderTreacheryDefinitions

captainMarvelEnergy :: HasGame m => m [AttachmentId]
captainMarvelEnergy = selectList $ AttachmentIs C.cw56098

addCaptainMarvelEnergy :: ReverseQueue m => Natural -> m ()
addCaptainMarvelEnergy n = captainMarvelEnergy >>= traverse_ (`placeCounters` n)

ironManHasTech :: HasGame m => VillainId -> m Bool
ironManHasTech vid = selectAny $ AttachmentMatches [AttachmentOnEnemy $ EnemyVillainId vid,AttachmentWithTrait T.Tech]

instance RunMessage LeaderTreachery where
  runMessage msg e@(LeaderTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      hero <- identityMatches HeroIdentity you
      case toCardCode a of
        "56070" -> if hero then discardFromDeck you 4 a else do
          upgrades <- selectRefs $ UpgradeControlledBy $ IdentityWithId you
          supports <- selectRefs $ SupportControlledBy $ IdentityWithId you
          chooseAndDiscardControlled you (upgrades <> supports)
        "56071" -> leaderOfFamily E.CwIronMan >>= traverse_ (\vid ->
          if hero then do
            tech <- ironManHasTech vid
            enemyAttacksGaining vid you [Piercing | tech]
          else do
            villainSchemesAgainst vid you
            cardSpecific a "supersonicAfterScheme" (you,vid))
        "56101" -> if hero then do
          leaderOfFamily E.CwCaptainMarvel >>= traverse_ (`enemyAttacks` you)
          addCaptainMarvelEnergy 1
          else do
            allies <- selectRefs $ AllyControlledBy $ IdentityWithId you
            supports <- selectRefs $ SupportControlledBy $ IdentityWithId you
            chooseAndDiscardControlled you (allies <> supports)
        "56102" -> if hero then placeThreatOnEach a 2 AnyScheme else do
          leaderOfFamily E.CwCaptainMarvel >>= traverse_ (`villainSchemesAgainst` you)
          addCaptainMarvelEnergy 1
        "56103" -> do
          addCaptainMarvelEnergy 1
          cardSpecific a "energyAbsorptionHeal" ()
        "56146" -> leaderOfFamily E.CwCaptainAmerica >>= traverse_ (\vid -> do
          attached <- selectAny $ AttachmentMatches [AttachmentIs C.cw56143,AttachmentOnEnemy $ EnemyVillainId vid]
          if attached then giveStatus vid ToughStatus else returnCaptainAmericaShield you vid
          dealBoost vid)
        "56147" -> leaderOfFamily E.CwCaptainAmerica >>= traverse_ (\vid ->
          if not hero then villainSchemesAgainst vid you else do
            attached <- selectAny $ AttachmentMatches [AttachmentIs C.cw56143,AttachmentOnEnemy $ EnemyVillainId vid]
            if attached then do
              damageAllCharacters a (toDamage 2 FromAbility) (CharacterControlledBy $ IdentityWithId you)
            else returnCaptainAmericaShield you vid)
        "56176" -> do
          stunIdentity you
          confuseIdentity you
          finesse <- selectList $ AttachmentIs C.cw56174
          traverse_ (`placeCounters` 1) finesse
        "56178" -> do
          cards <- controlledCards you [AllyType,UpgradeType,SupportType]
          scored <- for cards $ \target -> (target,) . max 0 . fromMaybe 0 . (cdCost =<<) <$> getsGame (`abilityCardDef` target)
          leader <- leaderOfFamily E.CwSpiderWoman
          let highest = [target | (target,cost) <- scored,cost == maximum (0 : map snd scored)]
          chooseOne you $
            [labeled "$label.cwDiscardHighestCost" $ chooseOrRunOne you [targeting target $ discardControlled target | target <- highest] | notNull highest]
            <> [labeled "$label.cwSpiderWomanSchemes" $ villainSchemesAgainst vid you | Just vid <- [leader]]
        "57009" -> leaderOfFamily E.CwSheHulk >>= traverse_ (\vid -> do
          if hero then enemyAttacks vid you else villainSchemesAgainst vid you
          giveStatus vid ToughStatus)
        "57010" -> do
          identities <- selectList $ IdentityWithId you <> UnexhaustedIdentity
          allies <- selectList $ AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly
          traverse_ exhaust identities
          traverse_ exhaust allies
          when (null identities && null allies) $ gainSurge a
        _ -> pure ()
      pure $ LeaderTreachery a {treacheryResolver = Just you}
    WithDiscarded (isTarget a -> True) FromDeck (onlyPlayerCards -> cards) | toCardCode a == "56070" -> do
      let n = fromIntegral $ length [r | card <- cards,(PrintedResource,r) <- cdResources $ getCardDef card,r `elem` [Energy,Wild]]
      traverse_ (\you -> damageIdentity you a $ toDamage n FromAbility) (treacheryResolver a)
      pure e
    CardSpecific (isTarget a -> True) "supersonicAfterScheme" (fromJSON -> Success (you,vid)) -> do
      tech <- ironManHasTech vid
      when tech $ giveStatus (you :: IdentityId) ConfusedStatus
      pure e
    CardSpecific (isTarget a -> True) "energyAbsorptionHeal" _ -> do
      counters <- sum <$> (traverse getCountersOn =<< captainMarvelEnergy)
      leaderOfFamily E.CwCaptainMarvel >>= traverse_ (\vid -> healFrom a vid counters)
      pure e
    Boost (CardSpecific (isTarget a -> True) "pheromonesAfterBoostStun" (fromJSON -> Success (you,old :: Natural))) -> do
      current <- getCharacterStatusCount (IdentityCharacter you) StunnedStatus
      when (current <= old) $ confuseIdentity you
      pure e
    Boost (RevealedAsBoost target _) | isTarget a target,toCardCode a == "56103" -> e <$ addCaptainMarvelEnergy 1
    Boost (RevealedAsBoost target enemy) | isTarget a target,toCardCode a == "56176" -> do
      you <- getBoostedPlayer enemy
      old <- getCharacterStatusCount (IdentityCharacter you) StunnedStatus
      stunIdentity you
      cardSpecific a "pheromonesAfterBoostStun" (you,old)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target,toCardCode a == "57010" -> do
      exhaustCharacterChoice =<< getBoostedPlayer enemy
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target,toCardCode a == "56178" -> do
      you <- getBoostedPlayer enemy
      n <- sum <$> traverse (getCharacterStatusCount $ IdentityCharacter you) [StunnedStatus,ConfusedStatus,ToughStatus]
      addEnemyBoost enemy n
      pure e
    _ -> LeaderTreachery <$> runMessage msg a
