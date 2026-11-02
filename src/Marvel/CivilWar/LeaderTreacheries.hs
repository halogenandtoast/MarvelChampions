module Marvel.CivilWar.LeaderTreacheries where

import Marvel.Prelude
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Damage
import Marvel.Entity
import Marvel.EncounterSet qualified as E
import Marvel.Game.Source
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
leaderTreacheryDefinitions = [C.cw56070,C.cw56071,C.cw56101,C.cw56102,C.cw56103]

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
    Boost (RevealedAsBoost target _) | isTarget a target,toCardCode a == "56103" -> e <$ addCaptainMarvelEnergy 1
    _ -> LeaderTreachery <$> runMessage msg a
